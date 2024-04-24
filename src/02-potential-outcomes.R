# ---------------------------------------------------------------------------- #
# 02-potential-outcomes
# ---------------------------------------------------------------------------- #
# 
# This script estimates individualized average potential outcomes (IAPOs) for 
# all specified labor market programs using double-robust machine learning for 
# the "Swiss Active Labor Market Policy Dataset, Ref. 1203".
# Use pre-processed data only. 
# APOs and ATEs and their respective standard errors can be estimated from the
# resulting IAPOs. 
#
# ---------------------------------------------------------------------------- #
# Author: Sebastian Zezulka
# 2024-04-24
#
# ---------------------------------------------------------------------------- #
# This code is based on the publications by:
# 1. Michael C. Knaus (2022). Double machine learning-based programme evaluation under unconfoundedness.The Econometrics Journal.
# see especially: 
# https://github.com/MCKnaus/mcknaus.github.io/blob/master/assets/code/Data_preparation_MCK2022.R
# 
# 2. John Körtner and Ruben L. Bach (2023). Inequality-Averse Outcome-Based Matching. 
# 
# Many thanks to John Körtner and Ruben Bach for sharing their code as well as 
# for many helpful discussions to Michael Knaus.
# All remaining errors are my own.
# 
# ---------------------------------------------------------------------------- #


# execute "00-utils.R" first!

# set seed
set.seed(seed)

# ---------------------------------------------------------------------------- #
# Libraries
library(tidyverse)
library(devtools)
library(causalDML)
library(policytree)

# ---------------------------------------------------------------------------- #
# Data
db_pre <- read.csv(data_path_pre)

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_potential_outcomes <- function(df, treatments_list, var_list, outcome_name, cv=4) {
  
  #############################################################
  # 
  # Estimates IAPOs and IATEs from DR scores.
  #
  # df :              Pre-processed data frame.
  # treatments_list : List of treatments for which IAPOs/IATEs are estimated. 
  # var_list :        List of variable names for covariate matrix.
  # outcome_name :    Character. Name of outcome variable used, e.g. "y_exit12".
  # cv :              Number of cross-validations, default = 4.
  #
  # Returns data frame with nuisance parameters, and IATE and IAPO estimates.
  #
  #############################################################
  
  # check
  if (!(is.character(outcome_name))) {stop("Error: Name of outcome variable must be character.")}
    
  # create treatment variable
  w = df$treatment6
  w = factor(w, 
             treatments_list)
  
  # preparation
  wm = prep_w_mat(w)
  cfm = prep_cf_mat(nrow(df), 
                    cv, 
                    wm)
  X <- data.matrix(df[,var_list])
  y <- df[,outcome_name]
  
  # create methods
  forest = create_method("forest_grf",
                         args=list(tune.parameters = "all",
                                   seed=seed))
  mean = create_method("mean")
  
  # estimate propensity scores
  em = nuisance_e(list(forest),
                  wm, 
                  X, 
                  cfm, 
                  cv=cv, 
                  path=NULL, 
                  quiet=FALSE)

  # estimate outcomes
  mm = nuisance_m(list(forest),
                  y, 
                  wm, 
                  X, 
                  cfm, 
                  cv=cv, 
                  path=NULL, 
                  quiet=FALSE, 
                  weights=FALSE)

  # estimate DR scores
  dr_scores <- f_dr_scores(wm, 
                           em, 
                           mm, 
                           y)
  
  # estimate smoothed IAPOs
  bound_list <- list(TRUE, 1, 0)
  train_idx <- df$training
  theta <- f_smoothed_iapos(wm, 
                            dr_scores$gamma, 
                            X,
                            train_idx,
                            seed=seed, 
                            bound_outcomes=bound_list[[1]], 
                            up=bound_list[[2]], 
                            low=bound_list[[3]])
  
  # get IATEs for baseline program
  baseline_program <- 'no program'
  iates <- matrix(NA, nrow(wm), ncol(wm)-1)
  iates <- theta[, -which(colnames(theta) == baseline_program)] - theta[, baseline_program]
  
  # update variable names
  colnames(em) <- gsub(" ", "_", paste0("em_", colnames(em)))
  colnames(mm) <- gsub(" ", "_", paste0("mm_", colnames(mm)))
  colnames(theta) <- gsub(" ", "_", paste0('iapo_', colnames(theta)))
  colnames(iates) <- gsub(" ", "_", paste0('iate_', colnames(iates)))
  
  df <- cbind(df, em, mm, iates, theta)
  
  return(df)
}

f_dr_scores <- function(w_mat, nui_prop, nui_outcomes, outcomes) {
  
  #############################################################
  #
  # Returns double-robust scores, gamma, alongside the
  # inverse probability weights, ipw_mat, used in their construction.
  #
  # w_mat :         Matrix of binary treatment indicators (n x T+1)
  # nui_prop :      Propensity scores (n x T+1)
  # nui_outcomes :  Pseudo outcomes per treatment (n x T+1)
  # outcomes :      Observed outcomes y (n x 1)
  #
  #############################################################
  
  ipw_mat <- f_ipw(w_mat, nui_prop)
  gamma <- f_gamma(w_mat, nui_outcomes, ipw_mat, outcomes)
  
  return(list(ipw=ipw_mat, gamma=gamma))
  
}

f_ipw <- function(w_mat, nui_prop) {
  
  #############################################################
  #
  # Estimates normalised Inverse Probability Weights, ipw.
  #
  # w_mat :     matrix of binary treatment indicators (n x T+1)
  # nui_prop :  Propensity scores (n x T+1)
  #
  #############################################################
  
  ipw = matrix(0, nrow(w_mat), ncol(w_mat))
  
  for (i in 1:ncol(w_mat)) {
    
    ipw[,i] = w_mat[,i] / nui_prop[,i]
    # normalisation
    ipw[,i] = ipw[,i] / (sum(ipw[,i]) * nrow(w_mat))
  }
  
  colnames(ipw) <- colnames(w_mat)
  
  return(ipw)
}

f_gamma <- function(w_mat, nui_outcomes, ipw_mat, outcomes) {
  
  #############################################################
  #
  # Estimates double-robust scores, gamma, according to
  # equation (3.1) in Knaus (2022).
  #
  # w_mat :         Matrix of binary treatment indicators (n x T+1)
  # nui_outcomes :  Pseudo outcomes per treatment (n x T+1)
  # ipw_mat :       Inverse probability weights (n x T+1)
  # outcomes :      Observed outcomes y (n x 1)
  #
  #############################################################
  
  gamma = matrix(NA, nrow(w_mat), ncol(w_mat))
  
  for (i in 1:ncol(w_mat)) {
    
    gamma[,i] = nui_outcomes[,i] + ipw_mat[,i] * (outcomes - nui_outcomes[,i])
  }
  
  colnames(gamma) <- colnames(w_mat)
  
  return(gamma)
}

f_smoothed_iapos <- function(w_mat, gamma, x, train_test_idx, bound_outcomes=TRUE, up=1, low=0, seed=12345) {
  
  #############################################################
  # 
  # Returns smoothed individualised average potential outcomes (theta),
  # using the estimates of the double-robust scores, gamma, as pseudo-outcomes.
  #
  # Regression forest build on scores for training data, only estimates for
  # the test set are used in the following analysis. 
  #
  # Note that Körtner and Bach (2023) use cross-validation to get IAPO estimates
  # for all observations. 
  #
  # see: https://github.com/MCKnaus/causalDML/issues/4#issuecomment-1238095799
  #
  # w_mat :           Matrix of binary treatment indicators (n x T+1)
  # gamma :           Matrix of DR potential outcomes (n x T+1)
  # x :               Covariate matrix (n x d)
  # train_test_idx :  Vector for train-test split.
  # bound_outcomes :  Boolean: True if results are to be bounded in range.
  # up :              Upper bound of range.
  # low :             Lower bound of range.
  # seed :            Seed, default = 12345.
  #
  #############################################################
  
  theta = matrix(NA, nrow(w_mat), ncol(w_mat))
  
  # loop over all treatment options
  for (i in 1:ncol(w_mat)) {
    
    # training set
    r.forest <- regression_forest(x[train_test_idx,], 
                                  gamma[train_test_idx,i], 
                                  tune.parameters = 'all', 
                                  seed=seed) 
    # test set
    theta[!train_test_idx,i] <- predict(r.forest, x[!train_test_idx,])$predictions
    
    cat('Variable:', i, 'of', ncol(w_mat), '.\n')
  }
  
  # bound outcomes in interval [up, down]
  if (bound_outcomes==TRUE) {
    theta <- f_bound_outcomes(theta, up, low)
  }
  
  colnames(theta) <- colnames(w_mat)
  return(theta)
}

f_bound_outcomes <- function(iapo_mat, up=1, low=0) {
  
  #############################################################
  # 
  # Bounds results in interval [low, up].
  # 
  # iapo_mat :  Matrix of estimated/smoothed IAPOs.
  # up :        Upper bound, default = 1.
  # low :       Lower bound, default = 0.
  #
  #############################################################
  
  # count how many outcomes are outside of specified effect range
  num_up <- apply(iapo_mat, 2, function(x) sum(x > up))
  num_low <- apply(iapo_mat, 2, function(x) sum(x < low))
  
  cat('Number of outcomes larger than ', up, ': ', num_up, '\n')
  cat('Number of outcomes smaller than ', low, ': ', num_low, '\n')
  
  # bound the outcomes outside of range
  for (i in 1:ncol(iapo_mat)) {
    iapo_mat[,i][iapo_mat[,i] > up] <- up
    iapo_mat[,i][iapo_mat[,i] < low] <- low
  }
  
  return(iapo_mat)
}

f_cDML <- function(df, treatments_list, var_list, outcome_name, cv=4) {
  
  #############################################################
  # 
  # Not required for the analysis. Serves only to compare average 
  # results with implementation in the CausalDML package.
  # Returns cDML object. 
  #
  # df :              Data frame.
  # treatments_list : List of treatments. 
  # var_list :        List of variable names for covariate matrix.
  # outcome_name :    Name of outcome variable, e.g. y_exit12. String.
  # cv :              Number of cross-validation folds, default = 4.
  #
  #############################################################
  
  # create treatment
  w = df$treatment6
  w = factor(w, 
             treatments_list)
  
  # preparation
  wm = prep_w_mat(w)
  cfm = prep_cf_mat(nrow(df), 
                    cv, 
                    wm)
  X <- data.matrix(df[,var_list])
  y <- df[,outcome_name]
  
  # create methods
  forest = create_method("forest_grf",
                         args=list(tune.parameters = "all",
                                   seed=seed))
  
  # estimation
  cDML = DML_aipw(y,
                  w,
                  X,
                  ml_w=list(forest),
                  ml_y=list(forest),
                  quiet=FALSE
  )
  
  return(cDML)
}

f_se_t_p <- function(df, treatments_list) {
  
  #############################################################
  # 
  # Estimates and returns standard errors, confidence intervals, 
  # t-statistics, and p-values for average treatment effects.
  #
  # df :              Data frame.
  # treatments_list : List of treatments for which IAPOs/IATEs are estimated. 
  #
  #############################################################
  
  # create treatment list for test (=simulation) data
  w <- df[df$training==0, "treatment6"]
  w = factor(w, treatments_list)
  wm = prep_w_mat(w)
  
  # select IATEs of test data
  df_iates <- df %>%
    filter(training==0) %>%
    select(starts_with('iate_')) 
  
  # prepare results matrix
  results_iate <- matrix(NA, ncol(wm)-1, 6)
  colnames(results_iate) <- c("ATE", "SE", "CI_low", "CI_up", "t", "p")
  rownames(results_iate) = rep("Platzhalter", nrow(results_iate))
  
  for (i in 1:(ncol(wm)-1)) {
    
    # ATE
    results_iate[i,1] <- mean(df_iates[,i])
    # SE ATE
    results_iate[i,2] <- sqrt(mean((df_iates[,i]-mean(df_iates[,i]))^2) / nrow(wm)) 
    
    # 95%-CI lower
    results_iate[i,3] <- results_iate[i,1] - ((1.65 * results_iate[i,2]) / sqrt(nrow(wm)))
    # 95%-CI upper
    results_iate[i,4] <- results_iate[i,1] + ((1.65 * results_iate[i,2]) / sqrt(nrow(wm)))
    
    rownames(results_iate)[i] = paste(colnames(wm)[i+1],"-",colnames(wm)[1])
  }
  
  # t-stat
  results_iate[,5] = results_iate[,1] / results_iate[,2]
  # p-value
  results_iate[,6] = 2 * stats::pt(abs(results_iate[,3]),nrow(wm),lower = FALSE)
  
  return(results_iate)
}

# ---------------------------------------------------------------------------- #
# Estimate Potential Outcomes and ATE Standard Errors
# ---------------------------------------------------------------------------- #

outcome <- 'y_exit12'
no_cv <- 4
db_effects <- f_potential_outcomes(db_pre, 
                                   treatments_list, 
                                   effects_var_list, 
                                   outcome_name=outcome, 
                                   cv=no_cv)

# quick analysis
db_effects %>% filter(training==0) %>% select(starts_with('iate_')) %>% summary(., mean())
db_effects %>% filter(training==0) %>% select(starts_with('iapo_')) %>% summary(., mean())


# Standard Errors for ATEs
iate_results_testset <- f_se_t_p(db_effects, treatments_list)

# ---------------------------------------------------------------------------- #
# save
# ---------------------------------------------------------------------------- #
write.csv(db_effects, file=data_path_effects)


# ---------------------------------------------------------------------------- #
# compare average results with vanilla causalDML implementation
# ---------------------------------------------------------------------------- #

# cDML <- f_cDML(db_pre, treatments_list, effects_var_list, outcome)
 
# summary(cDML$ATE)


# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #