# ---------------------------------------------------------------------------- #
# estimate potential outcomes
# ---------------------------------------------------------------------------- #

library(devtools)
# install_github(repo="MCKnaus/causalDML")
library(causalDML)
library(policytree)

# ---------------------------------------------------------------------------- #

# set working directory 
setwd("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")

source("src/01-prepare-data.R")

# ---------------------------------------------------------------------------- #

# create methods
mean = create_method("mean")
forest = create_method("forest_grf", 
                   args=list(tune.parameters = "all", seed=seed))

# preparation 
no_cv = 4
wm = prep_w_mat(w)
cfm = prep_cf_mat(length(y_exit12), no_cv, wm)

# TODO why not nuisance_dss_e and nuisance_dss_m?

# propensity
em = nuisance_e(list(forest),
                wm, x, cfm, 
                cv=no_cv, 
                path=NULL, 
                quiet=FALSE)
summary(em)

# estimate outcomes
mm = nuisance_m(list(forest),
                y_exit12, wm, x, cfm, 
                cv=no_cv, 
                path=NULL, 
                quiet=FALSE, 
                weights=FALSE)
summary(mm)

# DR scores
ipw = matrix(0, nrow(wm), ncol(wm))
gamma = matrix(NA, nrow(wm), ncol(wm))

for (i in 1:ncol(wm)) {
  ipw[,i] = wm[,i] / em[,i]
  # norm = 1
  # TODO important?
  ipw[,i] = ipw[,i] / (sum(ipw[,i]) * nrow(wm))
}

for (i in 1:ncol(wm)) {
  gamma[,i] = mm[,i] + ipw[,i] * (y_exit12 - mm[,i])
}


# cv prediction of debiased outcomes: 
## check: https://github.com/MCKnaus/causalDML/issues/4#issuecomment-1238095799
## smooth IAPO predictions 

# TODO why not first bounding and then smoothing?

theta = matrix(NA, nrow(wm), ncol(wm))
for (i in 1:ncol(wm)) {
  # cross-validation (theta)
  t = matrix(NA, nrow(cfm), ncol(cfm))

  for (c in 1:no_cv) {
    # cross-validation
    oos = cfm[,c]
    r.forest <- regression_forest(x[!oos,], gamma[!oos,i], tune.parameters = 'all', seed=seed) # 'none'
    t[oos,c] <- predict(r.forest, x[oos,])$predictions

  }
  theta[,i] <- rowSums(t, na.rm = TRUE)
}


# ---------------------------------------------------------------------------- #
# analyse 

# TODO why not plot against gamma? what does this tell?
# gamma and mm are perfectly aligned! regression works...
# for theta, mm:
# 6, 7 are negatively correlated!
# 1, 4, 5 seem ok
# 2, 3 fan out for higher values 

for (i in 1:7) {
  x_label <- paste0("theta_", i)
  y_label <- paste0("mm_", i)
  title_plot <- paste0("IAPOs vs. Nuisance Outcome, program ", i)
  title_hist <- paste0("Histogram IAPO scores, program ", i)
  
  plot(theta[,i], mm[,i],
       xlab=x_label, 
       ylab=y_label,
       main=title_plot)
  
  hist(theta[,i], breaks=100, 
       xlim=c(0,1),
       main=title_hist,
       xlab=x_label)
  # TODO not nice, plots on top of each other!
}



# ---------------------------------------------------------------------------- #

# ggplot(as.data.frame(theta), aes(x=V1)) +
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) +
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_np.png", width = 6, height = 4, units = 'in')
# 
# ggplot(as.data.frame(theta_original), aes(x=V2)) + 
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) + 
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_pc.png", width = 6, height = 4, units = 'in')
# 
# ggplot(as.data.frame(theta_original), aes(x=V3)) + 
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) + 
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_lg.png", width = 6, height = 4, units = 'in')
# 
# ggplot(as.data.frame(theta_original), aes(x=V4)) + 
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) + 
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_vc.png", width = 6, height = 4, units = 'in')

# ---------------------------------------------------------------------------- #
# save original theta's
theta_original <- theta

# test whether results are outside of effect range [0, 1]
apply(theta, 2, function(x) sum(x < 0))
apply(theta, 2, function(x) sum(x > 1))

# bound the outcome
for (i in 1:ncol(wm)) {
  theta[,i][theta[,i] > 1] <- 1
  theta[,i][theta[,i] < 0] <- 0
}

# ---------------------------------------------------------------------------- #
# add results to data

# potential outcomes
db$iapo_no_program  <- theta[,1]
db$iapo_vocational  <- theta[,2]
db$iapo_computer  <- theta[,3] # not in Knaus
db$iapo_language  <- theta[,4]
db$iapo_job_search    <- theta[,5]
db$iapo_employment <- theta[,6]
db$iapo_personality <- theta[,7] # not in Knaus

# IATEs against no_program
db$iate_vocational  <- theta[,2] - theta[,1]
db$iate_computer  <- theta[,3] - theta[,1]
db$iate_language    <- theta[,4] - theta[,1]
db$iate_job_search  <- theta[,5] - theta[,1]
db$iate_employment    <- theta[,6] - theta[,1]
db$iate_personality <- theta[,7] - theta[,1]

# propensity scores
db$em_no_program <- em[,1]
db$em_vocational <- em[,2]
db$em_computer <- em[,3]
db$em_language <- em[,4]
db$em_job_search <- em[,5]
db$em_employment <- em[,6]
db$em_personality <- em[,7]

# ---------------------------------------------------------------------------- #
# save
write.csv(db, file="data/almp_effects.csv")


# ---------------------------------------------------------------------------- #
# analyse results
# ---------------------------------------------------------------------------- #

db %>% select(starts_with('iate_')) %>% summary(., mean())
db %>% select(starts_with('iapo_')) %>% summary(., mean())

# TODO estimate IATEs from IAPO scores and compare with ndr_learner results as check! 


# ---------------------------------------------------------------------------- #
# potential outcomes and treatment effects
## Run the main function that outputs nuisance parameters, APO and ATE

cDML = DML_aipw(y_exit12,
                w,
                x,
                ml_w=list(forest),
                ml_y=list(forest),
                quiet=FALSE
                )

# comparision
summary(cDML$APO)
plot(cDML$APO)

print(colMeans(gamma))
print(colMeans(theta))

# 
summary(cDML$ATE)

## (N)DR-learner
ndr = ndr_learner(y_exit12,
                  w,
                  x,
                  ml_w = list(forest),
                  ml_y = list(forest),
                  ml_tau = list(forest),
                  quiet=FALSE,
                  compare_all = FALSE)

# # get APO estimates from one fold?
# # ndr$list[[1]]$APO$m_mat
# 
# ndr_cates <- ndr$cates[1:6, , 2]
# 
# # bound the outcome
# for (i in 1:ncol(wm)) {
#   ndr$cates[i,,2][ndr$cates[i,,2] > 1] <- 1
#   ndr$cates[i,,2][ndr$cates[i,,2] < 0] <- 0
# }
# 
# # Plot the results
# label_w = levels(w)
# df_box = NULL
# for (i in 1:6) {
#   df = data.frame("DRL" = ndr$cates[i,,1], "NDRL" = ndr$cates[i,,2])
#   df = gather(df)
#   df = cbind(label_w[i+1],df)
#   colnames(df)[1] = "label"
#   df_box = rbind(df_box,df)
# }
# ggplot(data=df_box) + geom_boxplot( aes(x=factor(label,label_w[-1]),y=value,fill=key)) +
#   theme_bw() + theme(axis.title.x=element_blank(),legend.title = element_blank()) +
#   ylab("Individualized average treatment effect") + geom_hline(yintercept = 0) + geom_hline(yintercept = -1,linetype="dashed") +
#   geom_hline(yintercept = 1,linetype="dashed") +
#   theme(text=element_text(family="serif",size = 16, colour="black"),axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   scale_fill_grey(start = 0.9,end=0.4)




