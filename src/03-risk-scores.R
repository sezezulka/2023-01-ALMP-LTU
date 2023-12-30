# ---------------------------------------------------------------------------- #
# estimate risk scores
# ---------------------------------------------------------------------------- #

#library(aif360)
library(causalDML)

source("src/01-prepare-data.R")
# TODO required for x_risk matrix, cleaner way?

# ---------------------------------------------------------------------------- #

# TODO change if necessary 
setwd("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")

#source("src/01-prepare-data.R")
db = read.csv("data/almp_effects.csv")


# ---------------------------------------------------------------------------- #

# cv folds
no_folds = 4
cfm_risk = prep_cf_mat(length(db$y_exit12), no_folds)

temp = matrix(NA, nrow(cfm_risk), ncol(cfm_risk))
risk_scores = matrix(NA, nrow(db), 1)

for (c in 1:no_folds) {
  # cross-validation
  oos = cfm_risk[,c]
  
  # logit model specification
  logit_model <- paste("y_exit12 ~", paste0(colnames(x_risk), collapse = " + "))
  r.log <- glm(logit_model, family=binomial(link='logit'), data=db[!oos,])
  
  # prediction on left-out data
  temp[oos,c] <- predict(r.log, db[oos,paste0(colnames(x_risk))], type = 'response')
}
risk_scores <- rowSums(temp, na.rm = TRUE)

# alternative: training/test split

# TODO make fairness evaluation for risk scores 
# TODO fairness constraint predictions!
# TODO correlate risk scores with employability score from CW

db$risk_log <- risk_scores

