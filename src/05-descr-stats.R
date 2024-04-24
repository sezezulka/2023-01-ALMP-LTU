# ---------------------------------------------------------------------------- #
# 05-descriptive-statistics
# ---------------------------------------------------------------------------- #
# 
# This script produces all reported numbers and tables for Zezulka and Genin
# (2024) from the simulation studies.
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
library(tidyverse)
library(gt)
library(Hmisc)

# ---------------------------------------------------------------------------- #
# Data
db_effects = read.csv(data_path_effects)
db_sim = read.csv(data_path_sim)

# ---------------------------------------------------------------------------- #
# Figure 3 (b), Average Treatment Effects and Standard Errors
# ---------------------------------------------------------------------------- #
# Standard errors and 95%-CI are estimated in "02-potential-outcomes.R"

db_sim %>% 
  filter(training==0) %>%
  select(starts_with('iate_')) %>% 
  colMeans()

db_sim %>% 
  filter(training==0) %>%
  select(starts_with('iapo_')) %>% 
  colMeans()


# ---------------------------------------------------------------------------- #
# Appendix B.1 and B.2, Table 1 and 2 
# ---------------------------------------------------------------------------- #
# uncomment filter(training==0) to get results for simulation data

summary_table_overall <- db_effects %>%
  # filter(training==0) %>%
  summarise(
    Obs = n(),
    Share_LTU = mean(y_exit12),
    Share_Women = mean(female),
    Avg_Age = mean(age),
    Share_Foreign = mean(1 - swiss),
    Avg_Employability = mean(employability),
    Avg_Past_Income = mean(past_income)
  )

summary_table_treatment <- db_effects %>%
  # filter(training==0) %>%
  group_by(treatment6) %>%
  summarise(
    Obs = n(),
    Share_LTU = mean(y_exit12),
    Share_Women = mean(female),
    Share_Foreign = mean(1 - swiss),
    Avg_Age = mean(age),
    Avg_Employability = mean(employability),
    Avg_Past_Income = mean(past_income)
  ) %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics by Treatment",
    subtitle = "Overall dataset and 6 different treatments"
  ) %>%
  fmt_number(
    columns = c(Share_LTU, Share_Women, Share_Foreign),
    decimals = 2,
    suffixing = " %"
  ) %>%
  fmt_number(
    columns = c(Avg_Age, Avg_Employability, Avg_Past_Income),
    decimals = 2
  )

# ---------------------------------------------------------------------------- #
# Appendix B.4, Table 4
# ---------------------------------------------------------------------------- #
# In: Jupyter Notebook "06-test-risk-scores"


# ---------------------------------------------------------------------------- #
# Appendix B.5, Table 5, Rates of long-term unemployment, baseline capacities
# ---------------------------------------------------------------------------- #
# 1. status quo
# Gender gap
mean(db_sim$y_exit12)
mean(db_sim$y_exit12[db_sim$female==1])
mean(db_sim$y_exit12[db_sim$female==0])
mean(db_sim$y_exit12[db_sim$female==1]) - mean(db_sim$y_exit12[db_sim$female==0])

# Citizen gap
mean(db_sim$y_exit12[db_sim$foreigner==1])
mean(db_sim$y_exit12[db_sim$foreigner==0])
mean(db_sim$y_exit12[db_sim$foreigner==1]) - mean(db_sim$y_exit12[db_sim$foreigner==0])

# ---------------------------------------------------------------------------- #
# 2. Belgian, optimal
# Gender gap
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1)
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$female==1])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$female==0])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$female==1]) - mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$female==0])

# Citizen gap
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$foreigner==1])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$foreigner==0])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$foreigner==1]) - mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_1[db_sim$foreigner==0])

# iapo_policy_Belgian_risk_score_sp_upper_1
# iapo_policy_Belgian_risk_score_eo_upper_1

# ---------------------------------------------------------------------------- #
# 3. Belgian, random

# iapo_policy_Belgian_risk_score_log_lower_1
# iapo_policy_Belgian_risk_score_sp_lower_1
# iapo_policy_Belgian_risk_score_eo_lower_1

# ---------------------------------------------------------------------------- #
# 4. Austrian, optimal

# iapo_policy_Austrian_risk_score_log_upper_1 
# iapo_policy_Austrian_risk_score_sp_upper_1
# iapo_policy_Austrian_risk_score_eo_upper_1

# ---------------------------------------------------------------------------- #
# 5. Austrian, random

# iapo_policy_Austrian_risk_score_log_lower_1 
# iapo_policy_Austrian_risk_score_sp_lower_1  
# iapo_policy_Austrian_risk_score_eo_lower_1 

# ---------------------------------------------------------------------------- #
# Appendix B.5, Table 6, Rates of long-term unemployment, five-fold capacities
# ---------------------------------------------------------------------------- #
# 1. status quo
# Gender gap
mean(db_sim$y_exit12)
mean(db_sim$y_exit12[db_sim$female==1])
mean(db_sim$y_exit12[db_sim$female==0])
mean(db_sim$y_exit12[db_sim$female==1]) - mean(db_sim$y_exit12[db_sim$female==0])

# Citizen gap
mean(db_sim$y_exit12[db_sim$foreigner==1])
mean(db_sim$y_exit12[db_sim$foreigner==0])
mean(db_sim$y_exit12[db_sim$foreigner==1]) - mean(db_sim$y_exit12[db_sim$foreigner==0])

# ---------------------------------------------------------------------------- #
# 2. Belgian, optimal
# Gender gap
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5)
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$female==1])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$female==0])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$female==1]) - mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$female==0])

# Citizen gap
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$foreigner==1])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$foreigner==0])
mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$foreigner==1]) - mean(db_sim$iapo_policy_Belgian_risk_score_log_upper_5[db_sim$foreigner==0])

# iapo_policy_Belgian_risk_score_sp_upper_5
# iapo_policy_Belgian_risk_score_eo_upper_5

# ---------------------------------------------------------------------------- #
# 3. Belgian, random

# iapo_policy_Belgian_risk_score_log_lower_5
# iapo_policy_Belgian_risk_score_sp_lower_5
# iapo_policy_Belgian_risk_score_eo_lower_5

# ---------------------------------------------------------------------------- #
# 4. Austrian, optimal

# iapo_policy_Austrian_risk_score_log_upper_5 
# iapo_policy_Austrian_risk_score_sp_upper_5
# iapo_policy_Austrian_risk_score_eo_upper_5

# ---------------------------------------------------------------------------- #
# 5. Austrian, random

# iapo_policy_Austrian_risk_score_log_lower_5
# iapo_policy_Austrian_risk_score_sp_lower_5 
# iapo_policy_Austrian_risk_score_eo_lower_5 


# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #