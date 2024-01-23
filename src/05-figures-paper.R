# ---------------------------------------------------------------------------- 
# 05 Figures Paper
# ---------------------------------------------------------------------------- 

library(tidyverse)

setwd("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")

file = "data/1203_ALMP_effects_risk_fairFemale_sim.csv"
db_sim = read.csv(file)

# ---------------------------------------------------------------------------- 
# Functions
# ---------------------------------------------------------------------------- 

f_reshape_long <- function(df) {
  
  #############################################################
  #
  #
  #
  #############################################################
  
  long_df <- df %>%
    tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                        names_to = "variable",
                        values_to = "mean_value") %>%
    mutate(capacity = readr::parse_number(variable))
  
  long_df[ , ncol(long_df) + 1] <- 0                  # Create column for level of inequality aversion
  colnames(long_df)[ncol(long_df)] <-"ineq_aversion"  
  long_df[grep("lo_ineq_averse",long_df$variable),]$ineq_aversion<-1
  long_df[grep("med_ineq_averse",long_df$variable),]$ineq_aversion<-2
  long_df[grep("hi_ineq_averse",long_df$variable),]$ineq_aversion<-3
  
  long_df[ , ncol(long_df) + 1] <- ""                  # Create column for Austrian/Belgian
  colnames(long_df)[ncol(long_df)] <-"policy"  
  long_df[grep("Belgian",long_df$variable),]$policy<-"Belgian"
  long_df[grep("Austrian",long_df$variable),]$policy<-"Austrian"
  
  long_df[ , ncol(long_df) + 1] <- "upper"                  # Create column for upper/lower/empirical assignment strategy
  colnames(long_df)[ncol(long_df)] <-"assignment"  # Rename column name
  # long_df[grep("upper",long_df$variable),]$assignment<-"upper"
  long_df[grep("lower",long_df$variable),]$assignment<-"lower"
  
  long_df[ , ncol(long_df) + 1] <- "0_log"                  # Create column for upper/lower/empirical assignment strategy
  colnames(long_df)[ncol(long_df)] <-"fairness"  # Rename column name
  long_df[grep("_sp_",long_df$variable),]$fairness<-"1_sp"
  long_df[grep("_eo_",long_df$variable),]$fairness<-"2_eo"
  # long_df[grep("_if_",long_df$variable),]$fairness<-"3_if"
  
  long_df[ , ncol(long_df) + 1] <- 1                  # Create column for upper/lower/empirical assignment strategy
  colnames(long_df)[ncol(long_df)] <-"fairness_alpha"  # Rename column name
  long_df[grep("_sp_",long_df$variable),]$fairness_alpha<-.66
  long_df[grep("_eo_",long_df$variable),]$fairness_alpha<-.33
  # long_df[grep("_if_",long_df$variable),]$fairness_alpha<-.25
  
  return(long_df) 
}

# ---------------------------------------------------------------------------- 
# Prepare Data
# ---------------------------------------------------------------------------- 

selected_vars <- grep("iapo_policy_", names(db_sim), value = TRUE)
# drop "individual fairness"
selected_vars <- selected_vars[!grepl("_if_", selected_vars)]

# prepare data, by gender
db_gender <- db_sim %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

db_policies <- db_sim %>%
  summarise(across(all_of(selected_vars), mean))

# reshape
gender_long <- f_reshape_long(db_gender)
long <- f_reshape_long(db_policies)

gender_long[ , ncol(gender_long) + 1] <- 0                # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"female_mean"  # Rename column name
gender_long$female_mean = gender_long$female * gender_long$mean_value

gender_long[ , ncol(gender_long) + 1] <- 0              # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"male_mean"  # Rename column name
gender_long$male_mean = (1-gender_long$female) * gender_long$mean_value

f_long <- filter(gender_long, female==1)
m_long <- filter(gender_long, female==0)

f_long$male_mean <- m_long$male_mean
f_long$mean_value <- long$mean_value

f_long_belgian_upper <- filter(f_long,policy=="Belgian" & assignment=="upper")
f_long_austrian_upper <- filter(f_long,policy=="Austrian" & assignment=="upper")
f_long_austrian_lower <- filter(f_long,policy=="Austrian" & assignment=="lower")
f_long_belgian_lower <- filter(f_long,policy=="Belgian" & assignment=="lower")

f_long_nofair_upper <- filter(f_long,fairness=="0_log" & assignment=="upper")
f_long_nofair_lower <- filter(f_long,fairness=="0_log" & assignment=="lower")

pre_ltu_female <- mean(db_sim[db_sim$female==1, "y_exit12"])
pre_ltu_male <- mean(db_sim[db_sim$female==0, "y_exit12"])

belgiumcolor="#1704E0"
austriacolor="#E03D2B"


# ---------------------------------------------------------------------------- 
# Fig 2: Swiss LTU
# ---------------------------------------------------------------------------- 
db_ltu_swiss <- readxl::read_xlsx("data/estat-une-ltu.xlsx")
db_ltu_swiss$gap <- db_ltu_swiss[,"women"] - db_ltu_swiss[,"men"]
db_ltu_swiss[,2:4] <- db_ltu_swiss[,2:4] * 100

ggplot(db_ltu_swiss, aes(x = year)) +
  geom_line(aes(y = women, colour = as.factor(group)), size = 1.5) +
  geom_point(aes(y = women, colour = as.factor(group), shape = as.factor(group)), size = 6) +
  geom_line(aes(y = men, colour = as.factor(1-group)), size = 1.5) +
  geom_point(aes(y = men, colour = as.factor(1-group), shape = as.factor(1-group)), size = 6) +
  geom_line(aes(y = gap, colour = as.factor(group*2)), size = 1.5) +
  geom_point(aes(y = gap, colour = as.factor(group*2), shape = as.factor(group*2)), size = 6) +
  geom_hline(yintercept = 0, size = 0.5) +
  scale_color_manual(name = "Gender:",
                     values = c("purple", "#61D04F", "#d95f02"), #  #fdc086
                     labels = c("Female", "Male", "Gap")) +
  scale_shape_manual(name = "Gender:",
                      values = c(17, 16, 18),
                      labels = c("Female", "Male", "Gap")) +
  scale_x_continuous(limits = c(1990, 2022), 
                     breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020),
                     labels = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  labs(y = "Swiss Long-Term Unemployment Rates, in %") +
  theme_classic() +
  theme(legend.position = "right", 
        legend.justification = c("left", "bottom"), 
        legend.box.just = "left",
        # legend.box.background = element_rect(color="black", size=2),
        legend.key.size = unit(1, 'cm'),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "vertical",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank())




# ---------------------------------------------------------------------------- 
# Fig 3: Violin Plots
# ----------------------------------------------------------------------------
# display ATEs
db_sim %>% 
  select(starts_with('iate_')) %>% 
  colMeans()

# db_sim %>% 
#   filter(training==0) %>%
#   select(starts_with('iapo_')) %>% 
#   colMeans()

# plot IATEs
db_iates <- db_sim %>%
  select(starts_with('iate_')) 

treatment_names <-  c("Vocational", "Computer", "Language", "Job Search", "Employment", "Personality")

data_long <- gather(db_iates, key = "IATE", value = "Value")
data_long <- data_long %>%
  mutate(IATE = sub("iate_", "", IATE)) %>%
  mutate(IATE = sub("_", " ", IATE))

data_long$IATE <- factor(data_long$IATE, treatments_list[-1])

ggplot(data_long, aes(x=IATE, y=Value)) +
  geom_violin(fill="grey") +
  stat_summary(fun.y = "mean", 
               geom="point", colour = "red", shape = 3, size=4) +
  geom_hline(yintercept = 0) + 
  labs(y = "IATEs") +
  scale_x_discrete(labels = treatment_names) +  
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# ---------------------------------------------------------------------------- 
# Fig 4: Gender Gap
# ---------------------------------------------------------------------------- 

# Belgian Upper
ggplot(f_long_belgian_upper, aes(x = capacity, 
                                 y = mean_value, 
                                 ymin = male_mean, 
                                 ymax = female_mean, 
                                 fill= policy, 
                                 alpha = as.factor(fairness))) +
  geom_ribbon() +
  geom_line(aes(y = female_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = male_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha=0.5) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = 17, alpha = 1) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = 16, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 0.9, y = 0.425, label = "ex-ante gender gap"),
            hjust = -0.2, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 1.3, y = 0.423, xend = 0.95, yend = 0.415),
             curvature = -0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash')) +
  ylim(.33,.44) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

# Austrian Upper
ggplot(f_long_austrian_upper, aes(x = capacity, 
                                  y = mean_value, 
                                  ymin = male_mean, 
                                  ymax = female_mean, 
                                  fill = policy, 
                                  alpha = fairness)) +
  geom_ribbon() +
  geom_line(aes(y = female_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = male_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha=0.5) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = 17, alpha = 1) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = 16, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 2.8, y = 0.41, label = "ex-post gender gap"),
            hjust = 0, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 2.7, y = 0.41, xend = 2.9, yend = 0.37),
             curvature = 0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash')) +
  ylim(.33,.44) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

# Belgian Lower
ggplot(f_long_belgian_lower, 
                                 aes(x = capacity, 
                                     y = mean_value, 
                                     ymin = male_mean, 
                                     ymax = female_mean, 
                                     fill = policy, 
                                     alpha =fairness)) +
  geom_ribbon() +
  geom_line(aes(y = female_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = male_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha=0.5) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = 17, alpha = 1) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = 16, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash')) +
  ylim(.33,.44) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))


# Austrian Lower
ggplot(f_long_austrian_lower, 
                                  aes(x = capacity, 
                                      y = mean_value, 
                                      ymin = male_mean, 
                                      ymax = female_mean, 
                                      fill = policy, 
                                      alpha = as.factor(fairness))) +
  geom_ribbon() +
  geom_line(aes(y = female_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = male_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha=0.5) +
  geom_point(aes(y = female_mean, shape = as.factor(female)), 
             position = position_dodge(width = 0.2), color = 'black', size = 4, alpha = 1) +
  geom_point(aes(y = male_mean, shape = as.factor(1-female)), 
             position = position_dodge(width = 0.2), color = 'black', size = 4, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Stat. Parity", "2_eo" = "Equal Opp.")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_shape_manual(values = c("0" = 16, "1" = 17), name = "Gender:",
                     labels = c("0" = "Male", "1" = "Female")) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash'),
                        labels = c("0_log" = "None", "1_sp" = "Stat. Par.", "2_eo" = "Equal Opp."))  +
  ylim(.33,.44) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate",
       linetype = "Fair:") +
  theme_classic() + 
  guides(fill = FALSE,
         alpha = FALSE) +
  theme(legend.position = c(0.02, 0.05), 
        legend.justification = c("left", "bottom"), 
        legend.box.just = "left",
        # legend.box.background = element_rect(color="black", size=2),
        legend.key.size = unit(1.5, 'cm'),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "horizontal",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) 


# ---------------------------------------------------------------------------- 
# Fig 5: Overall Plots
# ---------------------------------------------------------------------------- 

# Upper
ggplot(f_long_nofair_upper, 
       aes(x = capacity, 
           y = mean_value, 
           ymin = male_mean, 
           colour = policy, 
           ymax = female_mean, 
           fill = policy)) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(y = mean_value), size = 1) +
  geom_line(aes(y = female_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = male_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05))+
  geom_point(size = 6, shape = 18, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = 17, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = 16, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = 17, color = "black") +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = 16, color = "black") +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 0.9, y = 0.425, label = "ex-ante gender gap"),
            hjust = -0.2, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 1.3, y = 0.423, xend = 0.95, yend = 0.415),
             curvature = -0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 2.5, y = 0.395, label = "ex-post population rate"),
            hjust = 0, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 2.4, y = 0.395, xend = 2.45, yend = 0.373),
             curvature = 0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  ylim(.34,.44) +
  theme_classic() +
  theme(legend.position =  "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

# Lower
ggplot(f_long_nofair_lower, 
       aes(x = capacity, 
           y = mean_value, 
           ymin = male_mean, 
           colour = policy, 
           ymax = female_mean, 
           fill = policy)) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(y = mean_value), 
            size = 1, position = position_dodge(width = 0.1)) +
  geom_line(aes(y = female_mean), 
            colour = 'black', size = 0.5, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = male_mean), 
            colour = 'black', size = 0.5, position = position_dodge(width = 0.05))+
  geom_point(size = 6, shape = 18, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = female_mean, shape = as.factor(female)), 
             color = 'black', size = 4, alpha = 1, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean, shape = as.factor(1-female)), 
             position = position_dodge(width = 0.05), color = 'black', size = 4, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), 
             size = 4, shape = 17, color = "black") +
  geom_point(aes(x = 0.9, y = pre_ltu_male), 
             size = 4, shape = 16, color = "black") +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate",
       colour = "Policy:")+
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_shape_manual(values = c("0" = 16, "1" = 17), name = "Gender:",
                     labels = c("0" = "Male", "1" = "Female")) +
  ylim(.34,.44) +
  theme_classic() +
  guides(fill = FALSE,
         alpha = FALSE) +
  theme(legend.position = c(0.05, 0.05), 
        legend.justification = c("left", "bottom"), 
        legend.box.just = "left",
        # legend.box.background = element_rect(color="black", size=2),
        legend.key.size = unit(1.5, 'cm'),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "horizontal",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) 
