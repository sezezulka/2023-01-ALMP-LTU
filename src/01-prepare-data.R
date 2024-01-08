# ---------------------------------------------------------------------------- #
# 01-Data Preparation
# ---------------------------------------------------------------------------- #
# 
# Sources:
# - Knaus (2022)
# https://github.com/MCKnaus/mcknaus.github.io/blob/master/assets/code/
# Data_preparation_MCK2020.R
# - Körtner and Bach (2023)
#
# ---------------------------------------------------------------------------- #

# set working directory 
wd_path <- c("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")
setwd(wd_path)

# load config 
source("src/00-utils.R")

# libraries
library(tidyverse)
library(grf)

# set seed
set.seed(seed)
# TODO .Random.seed[1] does not reflect seed!

# read data
db = read.csv(raw_data_path)

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_preprocessing <- function(df, treatment_list, var_list, random_sample=FALSE, seed=12345) {
  
  #############################################################
  # 
  # Pre-processing of data frame. 
  # Add original ID, remove non German speaking cantons, keep  only specified 
  # programs, and (optionally) create 30% sample.
  # Create pseudo random starting points.
  # Create outcome variables.
  #
  # df :                Dataset
  # treatment_list :    List of treatments to be kept. 
  # var_list :          List of variables for covariate matrix.
  # random_sample :     Boolean, true if df is reduced to 30% sample.
  # seed :              Seed, default = 12345.
  # 
  #############################################################
  
  df <- df %>%
    mutate(ID = row_number()) %>%
    select(ID, dplyr::everything()) %>%
    filter(canton_german == 1) %>%
    filter(treatment6 %in% treatment_list)
  
  if (random_sample==TRUE) {
    df <- df %>%
      group_by(treatment6) %>%
      slice_sample(prop = 0.3) %>%
      ungroup()
  }
  
  df <- f_pseudo_random_starting(df, var_list, seed = seed)
  
  df <- f_create_outcomes(df)
  
  return(df)
}

f_pseudo_random_starting <- function(df, var_list, seed=12345) {
  
  #############################################################
  # 
  # Generate pseudo random program starting points for those in "no program".
  #
  # df :          Dataset, requires variables "treatment6" and "start_q2"
  # var_list :    List of variable names for covariate matrix.
  # seed :        Seed, default = 12345.
  #
  # Note coded treatment6 and start_q2 variables!
  #
  #############################################################
  
  # make covariate matrix
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
  df <- df %>% 
    filter(!(df$elap == 1 & (df$employed1 == 1 | df$employed2 == 1 | df$employed3 == 1)))
  # TODO add count how many people are dropped 
  
  return(df)
}

f_create_outcomes <- function(df) {
  
  #############################################################
  #
  # Create outcome variables, y. Returns df.
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
  
  # 1. outcome y_emp: 
  # months in employment in 31 months after treatment start
  emp <- matrix(NA, nrow(df), 31)
  
  emp[df$elap == 0,] <- as.matrix(df[df$elap == 0, c(sprintf("employed%s", seq(3,33)))])
  emp[df$elap == 1,] <- as.matrix(df[df$elap == 1, c(sprintf("employed%s", seq(6,36)))])
  df$y_emp = rowSums(emp)
  
  # 2. outcome y_unemp: 
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
  
  # 3. outcome y_exit12: 
  # long-term unemployment (12 month unemployment after (pseudo) program start)
  df$y_exit12 <- NA
  df$y_exit12[df$elap == 0] <- ifelse(df$y_unemp[df$elap == 0] > 15, 1, 0)
  df$y_exit12[df$elap == 1] <- ifelse(df$y_unemp[df$elap == 1] > 18, 1, 0)
  
  # version from Körtner/Bach
  # df$y_exit12 <- ifelse(df$y_unemp > 12, 1, 0)
  
  return(df)
}

# ---------------------------------------------------------------------------- #
# Preprocessing
# ---------------------------------------------------------------------------- #

db <- f_preprocessing(db, treatments_list, effects_var_list, random_sample=TRUE, seed=seed)


# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #