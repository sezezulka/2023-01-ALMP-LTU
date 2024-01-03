# ---------------------------------------------------------------------------- #
# 03-Estimate risk scores
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(causalDML)
# library(aif360)
# library(mlr3)
# library(mlr3fairness)

# ---------------------------------------------------------------------------- #
# set working directory
wd_path <- c("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")
setwd(wd_path)

# load config 
source("src/00-utils.R")

set.seed(seed)

# load data
db = read.csv(effect_data_path)

# or run preprocessing
# source("src/01-prepare-data.R")

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_risk_logistic_reg <- function(df, var_list, outcome_name, cv=4) {
  
  #############################################################
  #
  # Runs logistic regression to estimate risk scores of LTU.
  #
  # df :            Dataset.
  # var_lits :      List of variable names for risk estimation.
  # outcome_name :  Name of outcome variable, y. Character.
  # cv :            Number of cross-validation folds, default = 4.
  #
  #############################################################
  
  cfm_risk = prep_cf_mat(nrow(df), cv)
  
  x <- data.matrix(df[,var_list])
  
  
  temp = matrix(NA, nrow(cfm_risk), ncol(cfm_risk))
  risk_scores = matrix(NA, nrow(df), 1)
  
  # cross-validation
  for (c in 1:cv) {
    
    oos = cfm_risk[,c]
    
    # logit model specification
    logit_model <- paste0(outcome, " ~ ", paste0(var_list, collapse = " + "))
    r.log <- glm(
      logit_model, 
      data = df[!oos,],
      family = binomial(link='logit'), 
    )
    
    # prediction on left-out data
    temp[oos,c] <- predict(r.log, df[oos,var_list], type = 'response')
  }
  
  risk_scores <- rowSums(temp, na.rm = TRUE)
  
  df$risk_score_logistic <- risk_scores
  
  return(df)
}


# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #
outcome <- "y_exit12"
db <- f_risk_logistic_reg(db, risk_var_list, outcome, cv=4)

# analysis
hist(db$risk_score_logistic, breaks=100)

ggplot(db, aes(x=risk_score_logistic, fill=as.factor(female))) +
  geom_histogram(position = "identity", alpha=0.7, bins=100) +
  labs(title="Histogram of risk score, by Gender", x="Risk Score", y="Frequency") +
  scale_fill_manual(values = c("#56B4E9", "#009E73"), name = "Gender (binary)") +
  theme_minimal()

ggplot(db, aes(x=risk_score_logistic, fill=as.factor(swiss))) +
  geom_histogram(position = "identity", alpha=0.7, bins=100) +
  labs(title="Histogram of risk score, by Citizenship", x="Risk Score", y="Frequency") +
  scale_fill_manual(values = c("#56B4E9", "#009E73"), name = "Citizenship (binary)") +
  theme_minimal()

# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #