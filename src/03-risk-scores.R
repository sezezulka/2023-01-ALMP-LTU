# ---------------------------------------------------------------------------- #
# 03-risk-scores
# ---------------------------------------------------------------------------- #
# 
# This script estimates both fairness constraint and unconstrained risk scores 
# of long-term unemployment. It implements a logistic ridge regression and 
# implements "statistical parity" and "equal opportunity" as fairness constraints 
# on the risk scores. 
# The sensitive attribute, outcome variable and strength of the fairness 
# constraint can be customized as required. 
# 
# ---------------------------------------------------------------------------- #
# Author: Sebastian Zezulka
# 2024-04-18
# 
# ---------------------------------------------------------------------------- #

# execute "00-utils.R" first!

set.seed(seed)

# ---------------------------------------------------------------------------- #
# Libraries
library(causalDML)
library(fairml)
library(glmnet)

# ---------------------------------------------------------------------------- #
# Data
db_effects = read.csv(data_path_effects)

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_risk_scores <- function(df, 
                          var_list, 
                          outcome_name, 
                          sensitive_name, 
                          method_fairness_list,
                          quiet = FALSE) {
  
  #############################################################
  #
  # Returns simulation (test) dataset with estimated (fair) risk scores.
  #
  # df :                    Data frame.
  # var_list :              List of variables used for prediction of risk scores.
  # outcome_name :          String, name of outcome variable.
  # sensitive_name :        String, name of sensitive attribute variable.
  # method_fairness_list :  List of fairness method with unfairness constraint,
  #                         either length 2 or 3.
  # quiet :                 Boolean.
  #
  #############################################################
  
  # check
  if (!length(method_fairness_list) %in% c(2,3) ) {stop("Error: Must specify 2 or 3 fairness methods.")}
  
  # train-test split
  train_idx <- df$training
  
  # data (s for sensitive attribute)
  X_train <- data.matrix(df[train_idx,var_list])
  X_test <- data.matrix(df[!train_idx,var_list])
  
  y_train <- as.factor(df[train_idx,outcome_name])
  y_test <- as.factor(df[!train_idx,outcome_name])
  
  s_train <- data.matrix(df[train_idx,sensitive_name])
  s_test <- data.matrix(df[!train_idx,sensitive_name])
  
  # logistic regression
  y_score_log <- f_risk_log_reg(X_train, 
                                y_train,
                                X_test
                                )
  
  # fair logistic regression
  risk_fair_scores <- f_risk_fair_estimation(X_train,
                                             y_train,
                                             X_test,
                                             y_test,
                                             s_train,
                                             s_test,
                                             method_fairness_list,
                                             quiet = FALSE)
  
  # add results for test/simulation set
  if (length(method_fairness_list)==2) {
    df_test <- df %>%
      filter(training==0) %>%
      mutate(risk_score_log := y_score_log,
             risk_score_sp := risk_fair_scores[,1],
             risk_score_eo := risk_fair_scores[,2]
      )
  } else {
    df_test <- df %>%
      filter(training==0) %>%
      mutate(risk_score_log := y_score_log,
             risk_score_sp := risk_fair_scores[,1],
             risk_score_eo := risk_fair_scores[,2],
             risk_score_if := risk_fair_scores[,3]
      )
  }
  
  return(df_test)
}



f_risk_log_reg <- function(X_train, y_train, X_test) {
  
  #############################################################
  #
  # Returns risk scores for logistic ridge regression. 
  # Optimal lambda is choosen by cross-validation and used for prediction.
  #
  # X_train :   Training features.
  # y_train :   Training outcomes.
  # X_test :    Test/Simulation features.
  #
  #############################################################
  
  # train model
  ridge.log <- cv.glmnet(X_train,
                         as.factor(y_train),
                         family = 'binomial',
                         alpha = 0
                         )
  
  print(paste("Optimal lambda:", ridge.log$lambda.1se))
  
  # prediction on test set
  y_score_log <- predict(ridge.log, X_test, s = "lambda.1se", type = 'response')
  
  return(unname(y_score_log))
}



f_risk_fair_estimation <- function(X_train,
                                   y_train,
                                   X_test,
                                   y_test,
                                   s_train,
                                   s_test,
                                   method_fairness_list,
                                   quiet = FALSE) {
  
  #############################################################
  #
  # Returns risk scores from fairness constrained logistic ridge regression.
  #
  # X_train :               Training features.
  # y_train :               Training outcomes.
  # s_train :               Training sensitive attributes.
  # X_test :                Test/Simulation features.
  # y_test :                Test/Simulation outcomes.
  # s_test :                Test/Simulation sensitive attributes.
  # method_fairness_list :  List of fairness method with unfairness constraint,
  #                         either length 2 or 3.
  # quiet :                 Boolean.
  #
  #############################################################
  
  # preparation
  number_col <- length(method_fairness_list)
  risk_fair_results <- matrix(NA, nrow(X_test), number_col)
  colnames(risk_fair_results) <- paste0("Var", 1:number_col)
  
  # fair models
  i <- 1
  for (m in names(method_fairness_list)) {
    
    name <- paste0(m, '_', method_fairness_list[[i]])
    colnames(risk_fair_results)[i] <- name

    # fair logistic regressions
    r.fair <- fairml::fgrrm(y_train, 
                            X_train, 
                            s_train, 
                            unfairness = method_fairness_list[[i]], 
                            definition = m, 
                            family = "binomial", 
                            lambda = 0,
    )
    y_score_fair_fgrrm <- predict(r.fair, 
                                  X_test, 
                                  s_test, 
                                  type="response"
    )
    
    risk_fair_results[,i] <- y_score_fair_fgrrm
    
    # reports
    if (quiet == FALSE) {
      cat("Iteration", i, "with method", m, "and unfairness level", method_fairness_list[[i]], "\n")
    }
    # update counter
    i <- i + 1
  }
  
  return(risk_fair_results)
}


# ---------------------------------------------------------------------------- #
# Risk Score estimation
# ---------------------------------------------------------------------------- #
outcome <- "y_exit12"
sensitive_attribute <- "female"
methods_fairness <- list("sp-komiyama" = 0.01,
                         "eo-komiyama" = 0.01
                         )

db_risk <- f_risk_scores(db_effects,                    
                         risk_var_list,
                         outcome,
                         sensitive_attribute,
                         methods_fairness)

# ---------------------------------------------------------------------------- #
# save
# ---------------------------------------------------------------------------- #
write.csv(db_risk, file=data_path_risk)


# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #