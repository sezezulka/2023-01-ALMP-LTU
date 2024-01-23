# ---------------------------------------------------------------------------- 
# 05 Figures Supplemantary
# ---------------------------------------------------------------------------- 

library(tidyverse)

setwd("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")

file = "data/1203_ALMP_effects_risk_fairFemale_sim.csv"
db_sim = read.csv(file)

# ---------------------------------------------------------------------------- 
#
# ---------------------------------------------------------------------------- 

iapo_names <- paste0("iapo_", sub(" ", "_", treatments_list))


iapo_min <- db_sim %>% 
  select(all_of(iapo_names)) %>%
  rowwise() %>%
  mutate(min_value = min(c_across()))

test <- as.data.frame(db_sim["risk_score_log"])
test[, "iapo_min"] <- iapo_min["min_value"]
test[,"female"] <- db_sim["female"]
test[,"y_exit12"] <- db_sim["y_exit12"]
test[,'treatment6'] <- db_sim["treatment6"]
test[,'noprogram6'] <- ifelse(db_sim['treatment6']=="no program", 1, 0)
 
ggplot(test, aes(x = risk_score_log, y=iapo_min, colour=as.factor(noprogram6))) +
  geom_point(alpha=0.5, size=0.75) +
  theme_classic()
   

ggplot(db_sim, aes(x = risk_score_log, y=y_unemp)) +
  geom_point() +
  theme_classic()


ggplot(db_sim, aes(x = risk_score_log, y=iapo_no_program)) +
  geom_point() +
  theme_classic()

ggplot(db_sim, aes(x = risk_score_log, y=iapo_vocational)) +
  geom_point() +
  theme_classic()

ggplot(db_sim, aes(x = risk_score_log, y=iapo_computer)) +
  geom_point() +
  theme_classic()

ggplot(db_sim, aes(x = risk_score_log, y=iapo_language)) +
  geom_point() +
  theme_classic()

ggplot(db_sim, aes(x = risk_score_log, y=iapo_job_search)) +
  geom_point() +
  theme_classic()

ggplot(db_sim, aes(x = risk_score_log, y=iapo_employment)) +
  geom_point() +
  theme_classic()

ggplot(db_sim, aes(x = risk_score_log, y=iapo_personality)) +
  geom_point() +
  theme_classic()

