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

db_sim = read.csv("data/1203_ALMP_effects_risk_fairFemale_sim.csv")


# ---------------------------------------------------------------------------- #
# descriptive statistics
# ---------------------------------------------------------------------------- #
summary_table_overall <- db_sim %>%
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

summary_table_treatment <- db_sim %>%
  filter(training==0) %>%
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
# Gender gap
mean(db_test$y_exit12)
mean(db_test$y_exit12[db_test$female==1])
mean(db_test$y_exit12[db_test$female==0])
mean(db_test$y_exit12[db_test$female==1]) - mean(db_test$y_exit12[db_test$female==0])

# citizengap
mean(db_test$y_exit12)
mean(db_test$y_exit12[db_test$foreigner==1])
mean(db_test$y_exit12[db_test$foreigner==0])
mean(db_test$y_exit12[db_test$foreigner==1]) - mean(db_test$y_exit12[db_test$foreigner==0])

# old
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
db_sim %>% 
  filter(training==0) %>%
  select(starts_with('iate_')) %>% 
  colMeans()

db_sim %>% 
  filter(training==0) %>%
  select(starts_with('iapo_')) %>% 
  colMeans()

# plot IATEs
db_iates <- db_sim %>%
  filter(training==0) %>%
  select(starts_with('iate_')) 

treatment_names <-  c("Computer", "Employment", "Job Search", "Language", "Personality", "Vocational Training")

data_long <- gather(db_iates, key = "IATE", value = "Value")
data_long <- data_long %>%
  mutate(IATE = sub("iate_", "", IATE)) %>%
  mutate(IATE = sub("_", " ", IATE))

data_long$IATE <- factor(data_long$IATE, treatments_list[-1])



db_gender <- db_sim %>%
  group_by(female) %>%
  reframe(across(starts_with('iate_')))

db_gender_long <- db_gender %>%
  pivot_longer(cols = starts_with("iate_"), 
               names_to = "IATE",
               values_to = "Value")

ggplot(db_gender_long, aes(x=interaction(as.factor(female), y=IATE), y=Value, fill=as.factor(female))) +
  geom_violin() +
  geom_hline(yintercept = 0) + 
  stat_summary(fun.data = "mean_sdl", 
                geom="pointrange", color="black") +
  # geom_point(position = position_jitter(width = 0.1), size = 0.5, alpha = 0.3) +   
  # geom_boxplot(width = 0.1, fill = "white", color = "black") +  # Overlay boxplot for clarity
  geom_text(stat = "summary", fun = function(x) mean(x), 
            aes(label = round(..y.., 2)),
            vjust = 2, show.legend = FALSE) + 
  labs(title = "Estimated Individualised Average Treatment Effects (IATEs) on LTU of Swiss Labor Market Programs.",
       subtitle = "No Porgram is baseline treatment.",
       x = "IATE", 
       y = "Value",
       fill = "Gender") +
  scale_fill_manual(
    values = c(10, 20),
    labels = c("0" = "Male", "1" = "Female")) +
  scale_x_discrete(labels = rep(treatment_names, each=2)) +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  





iate_results_test[,1]

ggplot(data_long, aes(x=IATE, y=Value)) +
  geom_violin() +
  stat_summary(fun.y = "mean", 
              geom="point", colour = "black", shape = 3, size=4) +
  geom_hline(yintercept = 0) + 
  labs(y = "IATE") +
  scale_x_discrete(labels = treatments_list[-1]) +  
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 




# ---------------------------------------------------------------------------- #
# Risk Scores
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
