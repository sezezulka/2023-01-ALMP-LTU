# ---------------------------------------------------------------------------- #
# 06-figures-paper
# ---------------------------------------------------------------------------- #
# 
# This script produces all figures for the main body of Zezulka and Genin (2024).
#
# ---------------------------------------------------------------------------- #
# Authors: Sebastian Zezulka and Konstantin Genin
# 2024-04-18
#
# ---------------------------------------------------------------------------- #

# execute "00-utils.R" first!

set.seed(seed)

# ---------------------------------------------------------------------------- #
# Libraries
library(tidyverse)
library(readxl)
library(readr)

# ---------------------------------------------------------------------------- #
# Data
db_sim <- read.csv(data_path_sim)
db_ltu_swiss <- readxl::read_xlsx("data/estat-une-ltu.xlsx")

# TODO
db_sim <- read.csv("data/ARCHIVE/1203_ALMP_effects_risk_fairFemale_sim.csv")

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_sim_viz_prep <- function(df, sens_group) {
  
  #############################################################
  #
  # Returns long format of simulation results of IAPOs for plotting.
  #
  # df :          Data frame with simulation results.
  # sens_group :  Sensitive attribute of interest: {gender, citizenship}.
  #
  #############################################################
  
  # select simulation results
  selected_vars <- grep("iapo_policy_", names(df), value = TRUE)
  selected_vars <- selected_vars[!grepl("_if_", selected_vars)]
  
  # average LTU results by policies
  df_policies <- df %>%
    summarise(across(all_of(selected_vars), mean))
  
  # average LTU results by gender and policies
  df_gender <- df %>%
    group_by(female) %>%
    summarise(across(all_of(selected_vars), mean))
  
  # average LTU results by citzenship and policies
  df_citizen <- df %>%
    group_by(swiss) %>%
    summarise(across(all_of(selected_vars), mean))
  
  # make long format
  df_policies <- f_reshape_long(df_policies)
  
  if (sens_group=="gender") { 
    # long format by gender
    df_gender <- f_reshape_long(df_gender, by_gender=TRUE, by_citizen=FALSE)
    
    # prepare vizualisation data
    df_viz_long <- filter(df_gender, female==1)
    
    # add male average results
    m_long <- filter(df_gender, female==0)
    df_viz_long$male_mean <- m_long$male_mean
    
    # add overall policy average results
    df_viz_long$mean_value <- df_policies$mean_value
    
  } else if (sens_group=="citizenship") {
    # long format by citizenship
    df_citizen <- f_reshape_long(df_citizen, by_gender=FALSE, by_citizen=TRUE)
    
    # prepare vizualisation data
    
    df_viz_long <- filter(df_citizen, swiss==1)
    
    # add non-citizen average results
    noncit_long <- filter(df_citizen, swiss==0)
    df_viz_long$nonswiss_mean <- noncit_long$nonswiss_mean
    
    # add overall policy average results
    df_viz_long$mean_value <- df_policies$mean_value
    
  } else {
    stop("Error: Sensitive attribute must be either 'gender' or 'citizenship'.")
  }
  
  return(df_viz_long)
}


f_reshape_long <- function(df, by_gender=FALSE, by_citizen=FALSE) {
  
  #############################################################
  #
  # Returns long format of simulation data.
  #
  # df :          Data frame with simulation results.
  # by_gender :   Boolean: set TRUE  if data is grouped by gender.
  # by_citizen :  Boolean: set TRUE if data is grouped by citizen.
  #
  #############################################################
  
  df_long <- df %>%
    tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                        names_to = "variable",
                        values_to = "mean_value") %>%
    mutate(capacity = readr::parse_number(variable))
  
  # Create policy column (Austrian/Belgian)
  df_long[ , ncol(df_long) + 1] <- "Belgian"                  
  colnames(df_long)[ncol(df_long)] <-"policy"  
  df_long[grep("Austrian",df_long$variable),]$policy<-"Austrian"
  
  # Create assignment column (optimal/random assignment strategy)
  df_long[ , ncol(df_long) + 1] <- "upper"                  
  colnames(df_long)[ncol(df_long)] <-"assignment" 
  df_long[grep("lower",df_long$variable),]$assignment<-"lower"
  
  # Create fairness column (fairness constraint)
  df_long[ , ncol(df_long) + 1] <- "0_log"                  
  colnames(df_long)[ncol(df_long)] <-"fairness" 
  df_long[grep("_sp_",df_long$variable),]$fairness<-"1_sp"
  df_long[grep("_eo_",df_long$variable),]$fairness<-"2_eo"
  # df_long[grep("_if_",df_long$variable),]$fairness<-"3_if"
  
  # Create fairness_alpha column (colour transparency for each fairness constraint)
  df_long[ , ncol(df_long) + 1] <- 1                  
  colnames(df_long)[ncol(df_long)] <-"fairness_alpha" 
  df_long[grep("_sp_",df_long$variable),]$fairness_alpha<-.66
  df_long[grep("_eo_",df_long$variable),]$fairness_alpha<-.33
  # df_long[grep("_if_",df_long$variable),]$fairness_alpha<-.25
  
  if (by_gender==TRUE) {
    # Create column for female average result
    df_long[ , ncol(df_long) + 1] <- 0                
    colnames(df_long)[ncol(df_long)] <-"female_mean"
    df_long$female_mean = df_long$female * df_long$mean_value
    
    # Create column for male average result
    df_long[ , ncol(df_long) + 1] <- 0              
    colnames(df_long)[ncol(df_long)] <-"male_mean"
    df_long$male_mean = (1-df_long$female) * df_long$mean_value
  }
  
  if (by_citizen==TRUE) {
    # Create column for Swiss citizen average result
    df_long[ , ncol(df_long) + 1] <- 0                  
    colnames(df_long)[ncol(df_long)] <-"swiss_mean"     
    df_long$swiss_mean = df_long$swiss * df_long$mean_value
    
    # Create column for non-Swiss citizen average result
    df_long[ , ncol(df_long) + 1] <- 0                  
    colnames(df_long)[ncol(df_long)] <-"nonswiss_mean"
    df_long$nonswiss_mean = (1-df_long$swiss) * df_long$mean_value
  }
  
  return(df_long)
  }


# ---------------------------------------------------------------------------- #
# Prepare Data
# ---------------------------------------------------------------------------- #

# Swiss LTU Data
db_ltu_swiss$gap <- db_ltu_swiss[,"women"] - db_ltu_swiss[,"men"]
db_ltu_swiss[,2:4] <- db_ltu_swiss[,2:4] * 100
db_ltu_swiss$group <- 1       # required for ggplot groupings

# ---------------------------------------------------------------------------- #
# IATEs
treatment_names <-  c("Vocational", "Computer", "Language", "Job Search", "Employment", "Personality")

db_iates <- db_sim %>% select(starts_with('iate_')) %>% 
  gather(key = "IATE", value = "Value") %>% 
  mutate(IATE = sub("iate_", "", IATE)) %>%
  mutate(IATE = sub("_", " ", IATE)) 

db_iates$IATE <- factor(db_iates$IATE, treatments_list[-1])

# ---------------------------------------------------------------------------- #
# Simulation results 
db_sim_viz_gender <- f_sim_viz_prep(db_sim, "gender")

# figure-specific selection
f_long_belgian_upper <- db_sim_viz_gender %>% 
  filter(policy=="Belgian" & assignment=="upper")
f_long_austrian_upper <- db_sim_viz_gender %>% 
  filter(policy=="Austrian" & assignment=="upper")
f_long_austrian_lower <- db_sim_viz_gender %>% 
  filter(policy=="Austrian" & assignment=="lower")
f_long_belgian_lower <- db_sim_viz_gender %>% 
  filter(policy=="Belgian" & assignment=="lower")

f_long_nofair_upper <- db_sim_viz_gender %>% 
  filter(fairness=="0_log" & assignment=="upper")
f_long_nofair_lower <- db_sim_viz_gender %>% 
  filter(fairness=="0_log" & assignment=="lower")

# set visualization values
pre_ltu_female <- mean(db_sim[db_sim$female==1, "y_exit12"])
pre_ltu_male <- mean(db_sim[db_sim$female==0, "y_exit12"])


# ---------------------------------------------------------------------------- #
# Fig 2: Swiss LTU Rates
# ---------------------------------------------------------------------------- #

# LTU time series
fig2_swiss_ltu_rate <- ggplot(db_ltu_swiss, aes(x = year)) +
  geom_line(aes(y = women, colour = as.factor(group)), size = 1.5) +
  geom_point(aes(y = women, colour = as.factor(group), shape = as.factor(group)), size = 6) +
  geom_line(aes(y = men, colour = as.factor(1-group)), size = 1.5) +
  geom_point(aes(y = men, colour = as.factor(1-group), shape = as.factor(1-group)), size = 6) +
  geom_line(aes(y = gap, colour = as.factor(group*2)), size = 1.5) +
  geom_point(aes(y = gap, colour = as.factor(group*2), shape = as.factor(group*2)), size = 6) +
  geom_hline(yintercept = 0, size = 0.5) +
  scale_color_manual(name = "Gender:",
                     values = c("purple", "#61D04F", "#d95f02"),
                     labels = c("Female", "Male", "Gap")) +
  scale_shape_manual(name = "Gender:",
                      values = c(shape_female, shape_male, 18),
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

ggsave("output/Fig2-Swiss-LTU-Rates.pdf", fig2_swiss_ltu_rate, width = 10, height = 5)


# ---------------------------------------------------------------------------- #
# Fig 3 (a): Individualized Average Treatment Effects 
# ---------------------------------------------------------------------------- #

# violin plots of IATEs
fig3a_iates <- ggplot(db_iates, aes(x=IATE, y=Value)) +
  geom_violin(fill="grey", alpha=0.7) +
  stat_summary(fun.y = "mean", 
               geom="point", colour = "black", shape = 3, size=5) +
  geom_hline(yintercept = 0) + 
  labs(y = "IATEs") +
  scale_x_discrete(labels = treatment_names) +  
  theme_classic() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("output/Fig3a-IATEs.pdf", fig3a_iates, width = 7, height = 5)


# ---------------------------------------------------------------------------- #
# Fig 4: Gender Gap
# ---------------------------------------------------------------------------- #

# (a) Belgian Upper
fig4a_belgian_optimal <- ggplot(f_long_belgian_upper, aes(x = capacity, 
                                 y = mean_value, 
                                 ymin = male_mean, 
                                 ymax = female_mean, 
                                 fill= policy, 
                                 alpha = as.factor(fairness))) +
  geom_ribbon() +
  geom_line(aes(y = female_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = male_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha=0.5) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = shape_female, alpha = 1) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = shape_male, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = shape_female) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = shape_male) +
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

ggsave("output/Fig4a-Belgian-optimal.pdf", fig4a_belgian_optimal, width = 7, height = 6)


# (c) Austrian Upper
fig4c_austrian_optimal <- ggplot(f_long_austrian_upper, aes(x = capacity, 
                                  y = mean_value, 
                                  ymin = male_mean, 
                                  ymax = female_mean, 
                                  fill = policy, 
                                  alpha = fairness)) +
  geom_ribbon() +
  geom_line(aes(y = female_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = male_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha=0.5) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = shape_female, alpha = 1) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = shape_male, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = shape_female) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = shape_male) +
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

ggsave("output/Fig4c-Austrian-optimal.pdf", fig4c_austrian_optimal, width = 7, height = 6)


# (b) Belgian Lower
fig4b_belgian_random <- ggplot(f_long_belgian_lower, 
                                 aes(x = capacity, 
                                     y = mean_value, 
                                     ymin = male_mean, 
                                     ymax = female_mean, 
                                     fill = policy, 
                                     alpha =fairness)) +
  geom_ribbon() +
  geom_line(aes(y = female_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = male_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha=0.5) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = shape_female, alpha = 1) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = shape_male, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = shape_female) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = shape_male) +
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

ggsave("output/Fig4b-Belgian-random.pdf", fig4b_belgian_random, width = 7, height = 6)


# Austrian Lower
fig4d_austrian_random <- ggplot(f_long_austrian_lower, 
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
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = shape_female) +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = shape_male) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Stat. Parity", "2_eo" = "Equal Opp.")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_shape_manual(values = c("0" = shape_male, "1" = shape_female), name = "Gender:",
                     labels = c("0" = "Male", "1" = "Female")) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash'),
                        labels = c("0_log" = "None", "1_sp" = "Stat. Par.", "2_eo" = "Equal Opp."))  +
  ylim(.33,.44) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate",
       linetype = "Fair:") +
  theme_classic() + 
  guides(fill = "none",
         alpha = "none") +
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

ggsave("output/Fig4d-Austrian-random.pdf", fig4d_austrian_random, width = 7, height = 6)


# ---------------------------------------------------------------------------- #
# Fig 5: Overall Plots
# ---------------------------------------------------------------------------- #

# (a) Optimal/Upper
fig5a_optimal <- ggplot(f_long_nofair_upper, 
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
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = shape_female, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = shape_male, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = shape_female, color = "black") +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = shape_male, color = "black") +
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

ggsave("output/Fig5a-optimal.pdf", fig5a_optimal, width = 7, height = 6)


# (b) Random/Lower
fig5b_random <- ggplot(f_long_nofair_lower, 
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
             size = 4, shape = shape_female, color = "black") +
  geom_point(aes(x = 0.9, y = pre_ltu_male), 
             size = 4, shape = shape_male, color = "black") +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate",
       colour = "Policy:")+
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_shape_manual(values = c("0" = shape_male, "1" = shape_female), name = "Gender:",
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

ggsave("output/Fig5b-random.pdf", fig5b_random, width = 7, height = 6)


# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #