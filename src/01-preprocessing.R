# ---------------------------------------------------------------------------- #
# 01-preprocessing
# ---------------------------------------------------------------------------- #
# 
# Run this script to prepare the raw data "Swiss Active Labor Market Policy
# Dataset, Ref. 1203", available at Siwssbase, for the estimation of potential 
# outcomes of the recorded labor market programs.  
#
# ---------------------------------------------------------------------------- #
# Author: Sebastian Zezulka
# 2024-04-24
# 
# ---------------------------------------------------------------------------- #
# Data: 
# Michael Lechner, Michael Knaus, Martin Huber, Markus Frölich, Stefanie Behncke,
# Giovanni Mellace, Anthony Strittmatter (2020). Swiss Active Labor Market 
# Policy Evaluation [Dataset]. Distributed by FORS, Lausanne. 
# https://doi.org/10.23662/FORS-DS-1203-1
#
# This code is based on the publications by:
# 1. Michael C. Knaus (2022). Double machine learning-based programme evaluation
# under unconfoundedness.The Econometrics Journal.
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


# Execute "00-config.R" first.

# set seed
set.seed(seed)

# ---------------------------------------------------------------------------- #
# Libraries
library(tidyverse)
library(grf)

# ---------------------------------------------------------------------------- #
# Data
db_raw = read.csv(data_path_raw)

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_preprocessing <- function(df, treatment_list, var_list, random_sample=FALSE, train_share=0.5, seed=12345) {
  
  #############################################################
  # 
  # Pre-processing of raw data. 
  # Adds original IDs, removes non German speaking cantons, keeps only specified 
  # programs, and (optionally) create 30% sample.
  # Creates pseudo random starting points.
  # Creates the outcome variables.
  #
  # df :                Dataset
  # treatment_list :    List of treatments to be kept. 
  # var_list :          List of variables for covariate matrix.
  # random_sample :     Boolean, true if df is to be reduced to 30% sample.
  # train_share :       Size of training data, default = 0.5.
  # seed :              Seed, default = 12345.
  #
  # Outputs pre-processed data frame.
  # 
  #############################################################
  
  df <- df %>% 
    mutate(ID = row_number()) %>%
    select(ID, dplyr::everything()) %>%
    filter(canton_german == 1) %>%
    filter(treatment6 %in% treatment_list) %>%
    mutate(foreigner = 1 - swiss,
           foreigner_married = foreigner * married) 
  
  if (random_sample==TRUE) {
    df <- df %>%
      group_by(treatment6) %>%
      slice_sample(prop = 0.3) %>%
      ungroup()
  }
  
  df <- f_pseudo_random_starting(df, var_list, seed = seed)
  
  df <- f_create_outcomes(df)
  
  df <- f_train_test(df, train_share = train_share)
  
  return(df)
}

f_pseudo_random_starting <- function(df, var_list, seed=12345) {
  
  #############################################################
  # 
  # Generates pseudo random program starting points for those 
  # in "no program".
  #
  # df :          Dataset, requires variables "treatment6" and "start_q2"
  # var_list :    List of variable names for covariate matrix.
  # seed :        Seed, default = 12345.
  #
  # Outputs data frame.
  #
  # Note the hard-coded treatment6 and start_q2 variables!
  #
  #############################################################
  
  # create covariate matrix
  x <- data.matrix(df[,var_list])
  
  # assign pseudo random starting
  rf_late = regression_forest(x[df$treatment6 != "no program",], 
                              df$start_q2[df$treatment6 != "no program"], 
                              tune.parameters = "all", 
                              seed = seed)
  p_late = predict(rf_late,
                   x[df$treatment6 == "no program",])
  
  # generate pseudo starting point for those in no program
  df$elap = df$start_q2
  df$elap[df$treatment6 == "no program"] = 0 + rbernoulli(sum(df$treatment6 == "no program"), p_late)
  # table(df$elap)
  
  # remove those that are employed at the pseudo starting point
  n_rows <- nrow(df)
  df <- df %>% 
    filter(!(df$elap == 1 & (df$employed1 == 1 | df$employed2 == 1 | df$employed3 == 1)))
  
  n_rows_after <- nrow(df)
  n_dropped <- n_rows - n_rows_after
  print(paste('Number of dropped individuals:', n_dropped))
  
  return(df)
}

f_create_outcomes <- function(df) {
  
  #############################################################
  #
  # Creates outcome variables: y. Returns data frame.
  #
  # df :    Dataset.
  # 
  #############################################################
  
  if (!("elap" %in% names(df))) {
    stop("Error: Variable \"elap\" from pseudo random starting required.")
  }
  
  if ("y_emp" %in% names(df)) {
    stop("Error: Do not apply function twice.")
  }
  
  # First outcome: y_emp 
  # months in employment in 31 months after treatment start
  emp <- matrix(NA, nrow(df), 31)
  
  emp[df$elap == 0,] <- as.matrix(df[df$elap == 0, c(sprintf("employed%s", seq(3,33)))])
  emp[df$elap == 1,] <- as.matrix(df[df$elap == 1, c(sprintf("employed%s", seq(6,36)))])
  df$y_emp = rowSums(emp)
  
  # Second outcome: y_unemp 
  # duration of first unemployment spell (first month of employment or 37)
  df$y_unemp <- df$employed1
  
  for (i in 2:36) {
    temp_var_name <- paste0('employed', i)
    df[df[[temp_var_name]] == 1, temp_var_name] <- i
  }
  
  for (i in 2:36) {
    temp_var_name <- paste0('employed', i)
    df$y_unemp <- ifelse(df$y_unemp == 0, df[[temp_var_name]], df$y_unemp)
  }
  
  df$y_unemp <- ifelse(df$y_unemp == 0, 37, df$y_unemp)
  df$y_unemp <- df$y_unemp - 1
  
  # Third outcome: y_exit12 
  # binary variable, long-term unemployed if at least 12 month unemployment after (pseudo) program start
  df$y_exit12 <- NA
  df$y_exit12[df$elap == 0] <- ifelse(df$y_unemp[df$elap == 0] > 15, 1, 0)
  df$y_exit12[df$elap == 1] <- ifelse(df$y_unemp[df$elap == 1] > 18, 1, 0)
  
  # Note change in comparison with Körtner and Bach (2023), who used
  # df$y_exit12 <- ifelse(df$y_unemp > 12, 1, 0)

  return(df)
}

f_train_test <- function(df, train_share=0.5) {
  
  #############################################################
  #
  # Create train/test split variable.
  #
  # df :          Dataset.
  # train_share : Size of training data, default = 0.5
  #
  # Returns data frame.
  #
  #############################################################
  
  n_rows <- nrow(df)
  idx <- sort(sample(1:n_rows, round(train_share * n_rows)))
  
  # TRUE for training, FALSE for test set
  df$training <- FALSE
  df$training[idx] <- TRUE
  
  return(df)
  
}

# ---------------------------------------------------------------------------- #
# Preprocessing
# ---------------------------------------------------------------------------- #

db_pre <- f_preprocessing(db_raw, 
                          treatments_list,
                          effects_var_list, 
                          random_sample=FALSE, 
                          train_share=0.5,
                          seed=seed)

write.csv(db_pre, file=data_path_pre)

# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #