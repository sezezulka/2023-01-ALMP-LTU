# ---------------------------------------------------------------------------- #
# Data Preparation
#
# Sources:
# https://github.com/MCKnaus/mcknaus.github.io/blob/master/assets/code/
# Data_preparation_MCK2020.R
# ---------------------------------------------------------------------------- #

# set working directory 
setwd("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")

library(tidyverse)
library(grf)

seed = 12345
set.seed(seed)

# read data
db = read.csv("data/swissubase_1203_1_0/1203_ALMP_Data_E_v1.0.0.csv")
db$ID <- seq_along(db[,1])

# ---------------------------------------------------------------------------- #

# remove non German speaking cantons 
db <- db %>% 
  filter(canton_german == 1) %>%
  # filter(!(treatment6 == "employment" | treatment6 == "personality" | treatment6 == "job search")) %>% 
  # select 30% random sample
  slice_sample(prop = 0.3)

# get covariate matrix
x <- as.matrix(select(db, 
                     age,canton_moth_tongue,city_big,city_medium,city_no,cw_age,cw_cooperative,
                     cw_educ_above_voc,cw_educ_tertiary,cw_female,cw_missing,cw_own_ue,cw_tenure,
                     cw_voc_degree,emp_share_last_2yrs,emp_spells_5yrs,employability,female,foreigner_b,foreigner_c,
                     gdp_pc,married,other_mother_tongue,past_income,prev_job_manager,prev_job_sec_mis,prev_job_sec1,
                     prev_job_sec2,prev_job_sec3,prev_job_self,prev_job_skilled,prev_job_unskilled,qual_semiskilled,
                     qual_degree,qual_unskilled,qual_wo_degree,swiss,ue_cw_allocation1,ue_cw_allocation2,ue_cw_allocation3,
                     ue_cw_allocation4,ue_cw_allocation5,ue_cw_allocation6,ue_spells_last_2yrs,unemp_rate))


# ---------------------------------------------------------------------------- #
# assign pseudo random starting
rf_late = regression_forest(x[db$treatment6 != "no program",], 
                            db$start_q2[db$treatment6 != "no program"], 
                            tune.parameters = "all", seed = seed)
p_late = predict(rf_late,x[db$treatment6 == "no program",])

# generate pseudo starting point for those in no program
db$elap = db$start_q2
db$elap[db$treatment6 == "no program"] = 0 + rbernoulli(sum(db$treatment6 == "no program"), p_late)
table(db$elap)

# remove those that are employed at the pseudo starting point
db = db %>% 
  filter(!(db$elap == 1 & (db$employed1 == 1 | db$employed2 == 1 | db$employed3 == 1)))


# ---------------------------------------------------------------------------- #
# outcome y_emp: months in employment 31 months after treatment start
emp <- matrix(NA, nrow(db), 31)
emp[db$elap == 0,] <- as.matrix(db[db$elap == 0, c(sprintf("employed%s", seq(3,33)))])
emp[db$elap == 1,] <- as.matrix(db[db$elap == 1, c(sprintf("employed%s", seq(6,36)))])
db$y_emp = rowSums(emp)

# outcome y_unemp: duration of unemployment (month of exit)
db$y_unemp = db$employed1

for (i in 2:36) {
  x <- paste0('employed', i)
  db[db[[x]] == 1, x] <- i
}

for (i in 2:36) {
  x <- paste0('employed', i)
  db$y_unemp <- ifelse(db$y_unemp == 0, db[, x], db$y_unemp)
}

db$y_unemp <- ifelse(db$y_unemp == 0, 37, db$y_unemp)

# outcome y_exit12: becoming LTU
db$y_exit12 <- ifelse(db$y_unemp > 12, 1, 0)

# outcome variables
y_exit12 <- db$y_exit12
y_emp <- db$y_emp
y_unemp <- db$y_unemp

# ---------------------------------------------------------------------------- #
# create treatment
w = db$treatment6
w = factor(w, c("no program","vocational","computer","language", "job search", "employment", "personality"))

# variable labels for covariate matrix x
label_x = c("Age","Mother tongue in canton's language","Lives in big city","Lives in medium city",
            "Lives in no city","Caseworker age","Caseworker cooperative","Caseworker education: above vocational training",
            "Caseworker education: tertiary track","Caseworker female","Missing caseworker characteristics",
            "Caseworker has own unemployemnt experience","Caseworker tenure","Caseworker education: vocational degree",
            "Fraction of months employed last 2 years","Number of employment spells last 5 years","Employability",
            "Female","Foreigner with temporary permit","Foreigner with permanent permit","Cantonal GDP p.c.",
            "Married","Mother tongue other than German, French, Italian","Past income","Previous job: manager",
            "Missing sector","Previous job in primary sector","Previous job in secondary sector","Previous job in tertiary sector",
            "Previous job: self-employed","Previous job: skilled worker","Previous job: unskilled worker","Qualification: semiskilled",
            "Qualification: some degree","Qualification: unskilled","Qualification: skilled without degree","Swiss citizen",
            "Allocation of unemployed to caseworkers: by industry","Allocation of unemployed to caseworkers: by occupation",
            "Allocation of unemployed to caseworkers: by age","Allocation of unemployed to caseworkers: by employability",
            "Allocation of unemployed to caseworkers: by region","Allocation of unemployed to caseworkers: other",
            "Number of unemployment spells last 2 years","Cantonal unemployment rate (in %)")


# covariate matrix for risk score covariate matrix
x_risk = as.matrix(select(db, 
                     age,canton_moth_tongue,city_big,city_medium,city_no,
                     emp_share_last_2yrs,emp_spells_5yrs,female,foreigner_b,foreigner_c,
                     gdp_pc,married,other_mother_tongue,past_income,prev_job_manager,prev_job_sec_mis,prev_job_sec1,
                     prev_job_sec2,prev_job_sec3,prev_job_self,prev_job_skilled,prev_job_unskilled,qual_semiskilled,
                     qual_degree,qual_unskilled,qual_wo_degree,swiss,
                     ue_spells_last_2yrs,unemp_rate))

# variable labels for x_risk
label_x_risk = c("Age","Mother tongue in canton's language","Lives in big city","Lives in medium city",
            "Lives in no city",
            "Fraction of months employed last 2 years","Number of employment spells last 5 years",
            "Female","Foreigner with temporary permit","Foreigner with permanent permit","Cantonal GDP p.c.",
            "Married","Mother tongue other than German, French, Italian","Past income","Previous job: manager",
            "Missing sector","Previous job in primary sector","Previous job in secondary sector","Previous job in tertiary sector",
            "Previous job: self-employed","Previous job: skilled worker","Previous job: unskilled worker","Qualification: semiskilled",
            "Qualification: some degree","Qualification: unskilled","Qualification: skilled without degree","Swiss citizen",
            "Number of unemployment spells last 2 years","Cantonal unemployment rate (in %)")

# variable lables for risk estimation including outcome y_exit12
label_risk = c("Age","Mother tongue in canton's language","Lives in big city","Lives in medium city",
               "Lives in no city",
               "Fraction of months employed last 2 years","Number of employment spells last 5 years",
               "Female","Foreigner with temporary permit","Foreigner with permanent permit","Cantonal GDP p.c.",
               "Married","Mother tongue other than German, French, Italian","Past income","Previous job: manager",
               "Missing sector","Previous job in primary sector","Previous job in secondary sector","Previous job in tertiary sector",
               "Previous job: self-employed","Previous job: skilled worker","Previous job: unskilled worker","Qualification: semiskilled",
               "Qualification: some degree","Qualification: unskilled","Qualification: skilled without degree","Swiss citizen",
               "Number of unemployment spells last 2 years","Cantonal unemployment rate (in %)", "y_exit12")

# ---------------------------------------------------------------------------- #
# clean
rm(label_x, p_late, rf_late, emp)
# ---------------------------------------------------------------------------- #