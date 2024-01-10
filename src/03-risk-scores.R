# ---------------------------------------------------------------------------- #
# 03-Estimate risk scores
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(causalDML)

library(fairml)
library(fairness)
library(caret)
library(glmnet)

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

f_risk_logistic_reg_solo <- function(df, var_list, outcome_name, cv=4) {
  
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
  
  # TODO check cross validation!
  # cross-validation
  for (c in 1:cv) {
    
    oos = cfm_risk[,c]
    
    # logit model specification
    logit_model <- paste0(outcome_name, " ~ ", paste0(var_list, collapse = " + "))
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

f_risk_log_reg <- function(X_train, y_train, X_test) {
  
  #############################################################
  #
  # Ridge logistic regression.
  #
  # X_train :
  # y_train :
  # X_test : 
  #
  #############################################################
  
  # train model
  ridge.log <- cv.glmnet(X_train,
                         y_train,
                         family = 'binomial',
                         alpha = 0)
  
  print(paste("Optimal lambda:", ridge.log$lambda.min))
  
  # prediction on test set
  y_score_log <- predict(ridge.log, X_test, s = "lambda.min", type = 'response')
  
  return(unname(y_score_log))
}

f_risk_fair_experiments <- function(df,
                                    var_list,
                                    outcome_name,
                                    sensitive_name,
                                    train_share = 0.7,
                                    methods_all = FALSE, 
                                    loss = 'cel',
                                    quiet = FALSE) {
  
  #############################################################
  #
  # Train fairness constrained logistic regression.
  #
  # df :                Data frame.
  # var_list :          List of variable names for prediction.
  # outcome_name :      String, name of outcome variable.
  # sensitive_name :    String, name of sensitive attribute variable.
  # train_share :       Numeric, share of training data.
  # methods_all :       Boolean, if true, then all three methods applied. 
  # loss :              String, "cel", "mse" or "abs".
  # quiet :             Boolean.
  #
  #############################################################
  
  # train-test split
  idx <- sample(1:nrow(df), train_share * nrow(df))
  
  # data
  X_train <- data.matrix(df[idx,var_list])
  X_test <- data.matrix(df[-idx,var_list])
  y_train <- as.factor(df[idx,outcome_name])
  y_test <- as.factor(df[-idx,outcome_name])
  s_train <- data.matrix(df[idx,sensitive_name])
  s_test <- data.matrix(df[-idx,sensitive_name])
  
  # preparation
  if (methods_all == TRUE) {
    methods <- c("sp-komiyama", "eo-komiyama", "if-berk")
    # steps <- 0.05
    unfair_levels <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5) # seq(0,0.4,steps)
  } else {
    methods <- c("sp-komiyama", "eo-komiyama") 
    # steps <- 0.02
    unfair_levels <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5) # seq(0,0.2,steps)
  }
  
  number_col <- length(methods) * length(unfair_levels)
  risk_fair_results <- matrix(NA, nrow(X_test), number_col)
  colnames(risk_fair_results) <- paste0("Var", 1:number_col)
  comparision_methods <- c()
  
  # fair models
  i <- 1
  for (m in methods) {
    for (u in unfair_levels) {
      
      name <- paste0(m, '_', u)
      colnames(risk_fair_results)[i] <- name
      comparision_methods[i] <- name
      
      # fair logistic regressions
      r.fair <- fairml::fgrrm(y_train, 
                              X_train, 
                              s_train, 
                              unfairness = u, 
                              definition = m, 
                              family = "binomial", 
                              lambda = 0.01,
      )
      y_score_fair_fgrrm <- predict(r.fair, 
                                    X_test, 
                                    s_test, 
                                    type="response"
      )
      
      risk_fair_results[,i] <- y_score_fair_fgrrm
      
      # reports
      if (quiet == FALSE) {
        cat("Iteration", i, "with method", m, "and unfairness level", u, "\n")
      }
      # update counter
      i <- i + 1
    }
  }
  
  # loss
  loss_list <- list()
  y_test <- as.numeric(y_test)
  
  for (i in comparision_methods) {
    
    if (loss=='cel') {
      # cross entropy loss
      epsilon <- 1e-15
      predicted_probs <- pmax(epsilon, pmin(1 - epsilon, risk_fair_results[,i]))
      
      cross_entropy_loss <- -mean(y_test * log(predicted_probs) + (1 - y_test) * log(1 - predicted_probs))
      loss_list[[i]] <- cross_entropy_loss
      
    } else if (loss=='mse') {
      # mean squared error
      mse_loss <- mean((y_test - risk_fair_results[,i])^2)
      loss_list[[i]] <- mse_loss
      
    } else if ( loss == 'abs') {
      # mean absolute error
      absolute_loss <- mean(abs(y_test - risk_fair_results[,i]))
      loss_list[[i]] <- absolute_loss
      
    } else {
      stop("Error: Loss is missspecified.")
    }
  }
  
  # return
  outcomes = list(risk_scores=risk_fair_results, 
                  loss_list=loss_list, 
                  methods=comparision_methods,
                  train_idx=idx
                  )
  return(outcomes)
}

risk_fair_estimation <- function(X_train,
                                 y_train,
                                 X_test,
                                 y_test,
                                 s_train,
                                 s_test,
                                 method_fairness_list,
                                 quiet = FALSE) {
  
  #############################################################
  #
  # Train fairness constrained logistic regression.
  #
  # X_train :
  # y_train :
  # X_test :
  # y_test :
  # s_train :
  # s_test :
  # method_fairness_list : 
  # quiet :             Boolean.
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
                            lambda = 0.01,
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

f_plot_fair_loss <- function(loss_list, method) {
  
  #############################################################
  #
  # Plots loss vs fairness.
  #
  # loss_list : List of losses.
  # method :    String, "sp", "eo", or "if"
  #
  #############################################################
  
  if (method=='eo') {
    title <- 'Separation'
  } else if (method=='sp') {
    title <- 'Independence' 
  } else if (method=='if') {
    title <- 'Individual Fairness'
  }
  unfair_levels <- c(0, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5)
  selected_list <- loss_list[grep(paste0("^", method), names(loss_list))]
  
  min_idx <- which.min(selected_list)
  
  data <- data.frame(x=unfair_levels, y=unlist(selected_list))
  
  ggplot(data, aes(x=x, y=y)) +
    geom_line() +
    geom_point() +
    labs(title=paste0('Loss/Fairness: ', title), x="Fairness", y="Loss") +
    geom_vline(xintercept = data[min_idx,'x'], linetype = "solid", color = "black", size = 1) +
    theme_minimal()
}

f_risk_scores <- function(df, 
                          var_list, 
                          outcome_name, 
                          sensitive_name, 
                          train_share = 0.7, 
                          method_fairness_list,
                          quiet = FALSE) {
  
  #############################################################
  #
  # Estimate risk scores.
  #
  # df :                Dataframe.
  # var_list :          List of variable names for prediction.
  # outcome_name :      String, name of outcome variable.
  # sensitive_name :    String, name of sensitive attribute variable.
  # train_share :       Numeric, share of training data.
  # method_fairness_list : List of fairness method with unfairness constraint.
  # quiet :             Boolean.
  #
  #############################################################
  
  # train-test split
  idx <- sample(1:nrow(df), train_share * nrow(df))
  
  # data
  X_train <- data.matrix(df[idx,var_list])
  X_test <- data.matrix(df[-idx,var_list])
  y_train <- as.factor(df[idx,outcome_name])
  y_test <- as.factor(df[-idx,outcome_name])
  s_train <- data.matrix(df[idx,sensitive_name])
  s_test <- data.matrix(df[-idx,sensitive_name])
  
  # logistic regression
  y_score_log <- f_risk_log_reg(X_train, 
                                y_train,
                                X_test
                                )
  
  # fair logistic regression
  risk_fair_scores <- risk_fair_estimation(X_train,
                                    y_train,
                                    X_test,
                                    y_test,
                                    s_train,
                                    s_test,
                                    method_fairness_list,
                                    quiet = FALSE)
  
  # return results for test set
  df <- df %>%
    slice(-idx) %>%
    mutate(risk_score_log = y_score_log)
  
  df <- cbind(df, risk_fair_scores)
  
  return(df)
  
}

# ---------------------------------------------------------------------------- #
# Risk Score estimation
# ---------------------------------------------------------------------------- #
outcome <- "y_exit12"
sensitive_attribute <- "swiss"
methods_fairness <- list("sp-komiyama" = ,
                         "eo-komiyama" = ,
                         "if-berk" = )

db_simulation <- f_risk_scores(db, 
                               risk_var_list,
                               outcome,
                               sensitive_attribute,
                               train_share = 0.5,
                               methods_fairness)




# ---------------------------------------------------------------------------- #
# Risk Score: Experiments
# ---------------------------------------------------------------------------- #

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
# Fair Risk Scores: Experiments
# ---------------------------------------------------------------------------- #

risk_fair_female <- f_risk_fair_experiments(db, risk_var_list, outcome, 'female', methods_all = FALSE, loss = 'cel')
risk_fair_swiss <- f_risk_fair_experiments(db, risk_var_list, outcome, 'swiss', methods_all = TRUE, loss = 'cel')


f_plot_fair_loss(risk_fair_female$loss_list, "sp")
f_plot_fair_loss(risk_fair_female$loss_list, "eo")
f_plot_fair_loss(risk_fair_female$loss_list, "if")


f_plot_fair_loss(risk_fair_swiss$loss_list, "sp")
f_plot_fair_loss(risk_fair_swiss$loss_list, "eo")
f_plot_fair_loss(risk_fair_swiss$loss_list, "if")



# ---------------------------------------------------------------------------- #
# Fair Risk Scores: Analysis
# ---------------------------------------------------------------------------- #



for (col in colnames(risk_fair_results)) {
  hist(risk_fair_results[risk_fair_results$`db$female`==1, col], breaks = 100, main = col, col = rgb(0,0,1, alpha=0.5), border = "black")
  hist(risk_fair_results[risk_fair_results$`db$female`==0, col], breaks = 100, main = col, col = rgb(1,0,0, alpha=0.5), border = "black", add=TRUE)
  legend("topright", legend = c("Female", "Male"), fill = c(rgb(0, 0, 1, alpha = 0.5), rgb(1, 0, 0, alpha = 0.5)))
  # dev.off()
}

hist(risk_fair_results[,'sp-komiyama_0.8'], breaks=100, col=rgb(0,0,1, alpha=0.5))
hist(risk_fair_results[,'eo-komiyama_0.8'], breaks=100, col=rgb(0,1,0, alpha=0.5), add=TRUE)
hist(risk_fair_results[,'if-berk_0.8'], breaks=100, col=rgb(1,0,0, alpha=0.5), add=TRUE)


for (c in comparision_methods) {
  cat(c)
  
  # Accuracy Parity
  acc_parity(risk_fair_results, "db$y_exit12", "db$female", probs = c)
  # Independence
  dem_parity(risk_fair_results, "db$y_exit12", "db$female", probs = c)
  # Separation
  equal_odds(risk_fair_results, "db$y_exit12", "db$female", probs = c)
  # Sufficiency
  pred_rate_parity(risk_fair_results, "db$y_exit12", "db$female", probs = c)
  # ROC curve
  roc_parity(risk_fair_results, "db$y_exit12", "db$female", probs = c)
}

dem_parity(risk_fair_results, "db$y_exit12", "db$female", probs = "eo-komiyama_0")
dem_parity(risk_fair_results, "db$y_exit12", "db$female", probs = "eo-komiyama_0.4")
dem_parity(risk_fair_results, "db$y_exit12", "db$female", probs = "eo-komiyama_0.8")

dem_parity(risk_fair_results, "db$y_exit12", "db$female", probs = "sp-komiyama_0")
dem_parity(risk_fair_results, "db$y_exit12", "db$female", probs = "sp-komiyama_0.4")
dem_parity(risk_fair_results, "db$y_exit12", "db$female", probs = "sp-komiyama_0.8")





# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #