# ---------------------------------------------------------------------------- #
# 02-Estimate potential outcomes
# ---------------------------------------------------------------------------- #

library(devtools)
library(causalDML)
library(policytree)

# ---------------------------------------------------------------------------- #
# set working directory 
wd_path <- c("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")
setwd(wd_path)

source("src/01-prepare-data.R")
set.seed(seed)

# ---------------------------------------------------------------------------- #
# Preparations
# ---------------------------------------------------------------------------- #

f_potential_outcomes <- function(df, treatments_list, var_list, outcome_name, cv=4) {
  
  #############################################################
  # 
  # Estimation of IAPOs, IATEs from DR Scores.
  #
  # df :              Dataset.
  # treatments_list : List of treatments. 
  # var_list :        List of variable names for covariate matrix.
  # outcome_name :    Name of outcome variable, e.g. y_exit12. String.
  # -x- bound_list -x- : List of (Boolean, upper, and lower limit).
  # cv :              Number of cross-validations, default = 4.
  #
  #############################################################
  
  # tests
  # outcome_name <- as.character(outcome_name)
  # stopifnot(is.character(outcome_name), 
  #           "Name of outcome variable must be character.")
  # 
  # if (length(bound_list) == 3) {
  #   bound_outcomes <- as.character(bound_list[[1]])
  #   up <- as.numeric(bound_list[[2]])
  #   low <- as.numeric(bound_list[[3]])
  #   
  #   } else {
  #     stop("Error: List must have three elements.")
  #     
  #   }
  # 
  # stopifnot(is.logical(bound_outcomes), 
  #           "First element in bound_list must be Boolean.")
    
  # create treatment
  w = df$treatment6
  w = factor(w, 
             treatments_list)
  
  # preparation
  wm = prep_w_mat(w)
  cfm = prep_cf_mat(nrow(df), 
                    cv, 
                    wm)
  x <- data.matrix(df[,var_list])
  y <- df[,outcome_name]
  
  # create methods
  forest = create_method("forest_grf",
                         args=list(tune.parameters = "all",
                                   seed=seed))
  mean = create_method("mean")
  
  # estimate propensity scores
  em = nuisance_e(list(forest),
                  wm, 
                  x, 
                  cfm, 
                  cv=cv, 
                  path=NULL, 
                  quiet=FALSE)

  # estimate outcomes
  mm = nuisance_m(list(forest),
                  y, 
                  wm, 
                  x, 
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
  theta <- f_smoothed_iapos(wm, 
                            dr_scores$gamma, 
                            x,
                            cfm,
                            cv=cv, 
                            seed=seed, 
                            bound_outcomes=bound_list[[1]], 
                            up=bound_list[[2]], 
                            low=bound_list[[3]])
  
  # estimate IATEs against baseline program
  baseline_program <- 'no program'
  iates <- matrix(NA, nrow(wm), ncol(wm)-1)
  iates <- theta[, -which(colnames(theta) == baseline_program)] - theta[, baseline_program]
  
  # update variable names
  colnames(em) <- gsub(" ", "_", paste0("em_", colnames(em)))
  colnames(theta) <- gsub(" ", "_", paste0('iapo_', colnames(theta)))
  colnames(iates) <- gsub(" ", "_", paste0('iate_', colnames(iates)))
  
  df <- cbind(df, iates, theta, em)
  
  return(df)
}

f_dr_scores <- function(w_mat, nui_prop, nui_outcomes, outcomes) {
  
  #############################################################
  #
  # Estimates DR scores. Returns Gamma 
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
  # Estimates IPWs, normalised by norm 1.
  #
  # w_mat :     matrix of binary treatment indicators (n x T+1)
  # nui_prop :  Propensity scores (n x T+1)
  #
  #############################################################
  
  ipw = matrix(0, nrow(w_mat), ncol(w_mat))
  
  for (i in 1:ncol(w_mat)) {
    
    ipw[,i] = w_mat[,i] / nui_prop[,i]
    # TODO norm 1 normalisation
    ipw[,i] = ipw[,i] / (sum(ipw[,i]) * nrow(w_mat))
  }
  
  colnames(ipw) <- colnames(w_mat)
  
  return(ipw)
}

f_gamma <- function(w_mat, nui_outcomes, ipw_mat, outcomes) {
  
  #############################################################
  #
  # Estimates Gamma matrix.
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

f_smoothed_iapos <- function(w_mat, gamma, x, cfm, cv=4, seed=12345, bound_outcomes=TRUE, up=1, low=0) {
  
  #############################################################
  # 
  # Returns smoothed individualised average potential outcomes from Gamma.
  # check: https://github.com/MCKnaus/causalDML/issues/4#issuecomment-1238095799
  #
  # w_mat :           Matrix of binary treatment indicators (n x T+1)
  # gamma :           Matrix of DR potential outcomes (n x T+1)
  # x :               Covariate matrix (n x d)
  # cfm :             Matrix of binary cross-fitting fold indicators (n x # cross-folds).
  # cv :              Number of cross-validations, default = 4.
  # seed :            Seed, default = 12345.
  # bound_outcomes :  Boolean: True if results are to be bounded in range.
  # up :              Upper bound of range.
  # low :             Lower bound of range.
  #
  #############################################################
  
  # cfm = prep_cf_mat(nrow(w_mat), cv, w_mat)
  theta = matrix(NA, nrow(w_mat), ncol(w_mat))
  
  for (i in 1:ncol(w_mat)) {
    
    # cross-validation (theta)
    t = matrix(NA, nrow(cfm), ncol(cfm))
    
    for (c in 1:cv) {
      oos = cfm[,c]
      r.forest <- regression_forest(x[!oos,], 
                                    gamma[!oos,i], 
                                    tune.parameters = 'all', 
                                    seed=seed) 
      t[oos,c] <- predict(r.forest, x[oos,])$predictions
      
    }
    theta[,i] <- rowSums(t, na.rm = TRUE)
    
    cat('Variable:', i, 'of', ncol(w_mat), '.\n')
  }
  
  if (bound_outcomes==TRUE) {
    theta <- f_bound_outcomes(theta, up, low)
  }
  
  colnames(theta) <- colnames(w_mat)
  return(theta)
}

f_bound_outcomes <- function(iapo_mat, up=1, low=0) {
  
  #############################################################
  # 
  # Bounds results in range [0, 1]
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
  # Estimation of APOs and ATEs from CausalDML package.
  #
  # df :              Dataset.
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
  x <- data.matrix(df[,var_list])
  y <- df[,outcome_name]
  
  # create methods
  forest = create_method("forest_grf",
                         args=list(tune.parameters = "all",
                                   seed=seed))
  mean = create_method("mean")
  
  # estimation
  cDML = DML_aipw(y,
                  w,
                  x,
                  ml_w=list(forest),
                  ml_y=list(forest),
                  quiet=FALSE
  )
  
  return(cDML)
}

f_ndr_learner <- function(df, treatments_list, var_list, outcome_name,cv=4) {
  
  #############################################################
  #
  # (N)DR-learner from causalDML package. 
  #
  # df :              Dataset.
  # treatments_list : List of treatments. 
  # var_list :        List of variable names for covariate matrix.
  # outcome_name :    Name of outcome variable, e.g. y_exit12. String.
  # cv :              Number of cross-validations, default = 4.
  #
  #############################################################
  
  # create treatment
  w = df$treatment6
  w = factor(w, 
             treatments_list)
  
  # preparation
  x <- data.matrix(df[,var_list])
  y <- df[,outcome_name]
  
  # create methods
  forest = create_method("forest_grf",
                         args=list(tune.parameters = "all",
                                   seed=seed))
  mean = create_method("mean")
  
  # (n)dr learner
  ndr = ndr_learner(y,
                    w,
                    x,
                    ml_w = list(forest),
                    ml_y = list(forest),
                    ml_tau = list(forest),
                    compare_all = FALSE,
                    nfolds = cv,
                    quiet=FALSE,
  )
  
  return(ndr)
}

# ---------------------------------------------------------------------------- #
# Potential Outcomes
# ---------------------------------------------------------------------------- #

outcome <- 'y_exit12'
no_cv <- 4
db <- f_potential_outcomes(db, treatments_list, effects_var_list, outcome_name=outcome, cv=no_cv)

# analyse
db %>% select(starts_with('iate_')) %>% colMeans()
db %>% select(starts_with('iapo_')) %>% summary(., mean())

  
# ---------------------------------------------------------------------------- #
# save
write.csv(db, file="data/1203_ALMP_Sample_IATEs.csv")
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# compare results
# ---------------------------------------------------------------------------- #


cDML <- f_cDML(db, treatments_list, effects_var_list, outcome)

summary(cDML$ATE)

ndr <- f_ndr_learner(db, treatments_list, effects_var_list, outcome)
# TODO Fehler: honest fraction too close to 1 or 0

# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #