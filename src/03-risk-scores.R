# ---------------------------------------------------------------------------- #
# 03-Estimate risk scores
# ---------------------------------------------------------------------------- #

# library(aif360)
library(causalDML)
# library(mlr3)
# library(mlr3fairness)

# ---------------------------------------------------------------------------- #
# set working directory
wd_path <- c("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")
setwd(wd_path)

# load config 
source("src/00-utils.R")

set.seed(seed)

db = read.csv(effect_data_path)


# ---------------------------------------------------------------------------- #
# cv folds
no_folds = 4
cfm_risk = prep_cf_mat(length(db$y_exit12), no_folds)

temp = matrix(NA, nrow(cfm_risk), ncol(cfm_risk))
risk_scores = matrix(NA, nrow(db), 1)

for (c in 1:no_folds) {
  # cross-validation
  
  # TODO does this make sense?  
  oos = cfm_risk[,c]
  
  # logit model specification
  logit_model <- paste("y_exit12 ~", paste0(colnames(x_risk), collapse = " + "))
  r.log <- glm(
    logit_model, 
    family=binomial(link='logit'), 
    data=db[!oos,]
    )
  
  # prediction on left-out data
  temp[oos,c] <- predict(r.log, db[oos,paste0(colnames(x_risk))], type = 'response')
}
risk_scores <- rowSums(temp, na.rm = TRUE)

# alternative: training/test split

# TODO make fairness evaluation for risk scores 
# TODO fairness constraint predictions!
# TODO correlate risk scores with employability score from CW

db$risk_log <- risk_scores

