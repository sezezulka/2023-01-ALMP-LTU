# ---------------------------------------------------------------------------- #
# algorithmically-informed policies
# ---------------------------------------------------------------------------- #

library(tidyverse)
library(gt)
library(Hmisc)


# ---------------------------------------------------------------------------- 
# change if necessary 
setwd("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")

seed = 12345
set.seed(seed)

db = read.csv("data/almp_effects.csv")


# ---------------------------------------------------------------------------- #
# descriptive statistics
# ---------------------------------------------------------------------------- #
summary_table_overall <- db %>%
  summarise(
    Obs = n(),
    Share_LTU = mean(y_exit12),
    Share_Women = mean(female),
    Avg_Age = mean(age),
    Share_Foreign = mean(1 - swiss),
    Avg_Employability = mean(employability),
    Avg_Past_Income = mean(past_income)
  )

summary_table_treatment <- db %>%
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
# LTU gaps
# ---------------------------------------------------------------------------- #
mean(db$y_exit12[db$female==1])
mean(db$y_exit12[db$female==0])
mean(db$y_emp[db$female==1])
mean(db$y_emp[db$female==0])

mean(db$y_exit12[db$swiss==1])
mean(db$y_exit12[db$swiss==0])
mean(db$y_emp[db$swiss==1])
mean(db$y_emp[db$swiss==0])

mean(db$y_exit12[db$qual_unskilled==1])
mean(db$y_exit12[db$qual_unskilled==0])
mean(db$y_emp[db$qual_unskilled==1])
mean(db$y_emp[db$qual_unskilled==0])

median_income <- median(db$past_income)
# TODO find median income of swiss population as threshold
mean(db$y_exit12[db$past_income > median_income/2])
mean(db$y_exit12[db$qual_unskilled <= median_income/2])
mean(db$y_emp[db$past_income > median_income/2])
mean(db$y_emp[db$past_income >= median_income/2])


# ---------------------------------------------------------------------------- #
# ATEs and APOs
# ---------------------------------------------------------------------------- #
db %>% select(starts_with('iate_')) %>% 
  colMeans()

db %>% select(starts_with('iapo_')) %>% 
  colMeans()

# plot IATEs
pivot_longer(db, starts_with('iate_')) %>%
  ggplot(aes(name, value)) +
  geom_violin() +
  stat_summary(fun.data = "mean_sdl", 
               geom="pointrange", color="black") +
  # geom_jitter(shape=16, position=position_jitter(0.1)) +
  labs(title = "estimated IATEs for different programs (no program is baseline)",
       x = "ALMP",
       y = "IATE") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# ---------------------------------------------------------------------------- #
# Risk Scires
# ---------------------------------------------------------------------------- #
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
