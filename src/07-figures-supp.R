# ---------------------------------------------------------------------------- #
# 07-figures-supplementary
# ---------------------------------------------------------------------------- #
# 
# This script produces all figures for the Supplemantary of Zezulka and Genin (2024).
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

f_iate_viz_prep <- function(df, list_treatments) {
  
  #############################################################
  #
  # Returns IATE estimates by gender for plotting.
  #
  # df :              Data frame with simulation results.
  # list_treatments : List of treatments for which IAPOs/IATEs are estimated. 
  #
  #############################################################
  
  df_iate_gender <- df %>%
    group_by(female) %>%
    reframe(across(starts_with('iate_'))) %>%
    pivot_longer(cols = starts_with('iate_'),
                 names_to = 'IATE',
                 values_to = 'Value') %>%
    mutate(IATE = sub('iate_', '', IATE)) %>%
    mutate(IATE = sub('_', ' ', IATE))
  
  df_iate_gender$IATE <- factor(df_iate_gender$IATE, list_treatments[-1])
  
  return(df_iate_gender)
}

f_almp_participants <- function(df) {
  
  #############################################################
  #
  # Returns data frame with number of participants by program.
  #
  # df :          Data frame with simulation results.
  #
  #############################################################
  
  # status quo
  freq_0 <- table(df$treatment6)
  freq_0_f <- table(df$treatment6[df$female==1])
  freq_0_m <- table(df$treatment6[df$female==0])
  
  # Belgian policy
  freq_B_log_1_f <- table(df$policy_Belgian_risk_score_log_upper_1[df$female==1])
  freq_B_log_1_m <- table(df$policy_Belgian_risk_score_log_upper_1[df$female==0])
  
  freq_B_sp_1_f <- table(df$policy_Belgian_risk_score_sp_upper_1[df$female==1])
  freq_B_sp_1_m <- table(df$policy_Belgian_risk_score_sp_upper_1[df$female==0])
  
  freq_B_eo_1_f <- table(df$policy_Belgian_risk_score_eo_upper_1[df$female==1])
  freq_B_eo_1_m <- table(df$policy_Belgian_risk_score_eo_upper_1[df$female==0])
  
  freq_B_log_5_f <- table(df$policy_Belgian_risk_score_log_upper_5[df$female==1])
  freq_B_log_5_m <- table(df$policy_Belgian_risk_score_log_upper_5[df$female==0])
  
  freq_B_sp_5_f <- table(df$policy_Belgian_risk_score_sp_upper_5[df$female==1])
  freq_B_sp_5_m <- table(df$policy_Belgian_risk_score_sp_upper_5[df$female==0])
  
  freq_B_eo_5_f <- table(df$policy_Belgian_risk_score_eo_upper_5[df$female==1])
  freq_B_eo_5_m <- table(df$policy_Belgian_risk_score_eo_upper_5[df$female==0])
  
  # Austrian policy
  freq_A_log_1_f <- table(df$policy_Austrian_risk_score_log_upper_1[df$female==1])
  freq_A_log_1_m <- table(df$policy_Austrian_risk_score_log_upper_1[df$female==0])
  
  freq_A_sp_1_f <- table(df$policy_Austrian_risk_score_sp_upper_1[df$female==1])
  freq_A_sp_1_m <- table(df$policy_Austrian_risk_score_sp_upper_1[df$female==0])
  
  freq_A_eo_1_f <- table(df$policy_Austrian_risk_score_eo_upper_1[df$female==1])
  freq_A_eo_1_m <- table(df$policy_Austrian_risk_score_eo_upper_1[df$female==0])
  
  freq_A_log_5_f <- table(df$policy_Austrian_risk_score_log_upper_5[df$female==1])
  freq_A_log_5_m <- table(df$policy_Austrian_risk_score_log_upper_5[df$female==0])
  
  freq_A_sp_5_f <- table(df$policy_Austrian_risk_score_sp_upper_5[df$female==1])
  freq_A_sp_5_m <- table(df$policy_Austrian_risk_score_sp_upper_5[df$female==0])
  
  freq_A_eo_5_f <- table(df$policy_Austrian_risk_score_eo_upper_5[df$female==1])
  freq_A_eo_5_m <- table(df$policy_Austrian_risk_score_eo_upper_5[df$female==0])
  
  # make data frame
  almp_participants <- data.frame(
    status_quo = as.vector(freq_0),
    status_quo_female = as.vector(freq_0_f),
    status_quo_male = as.vector(freq_0_m),
    B_log_1_female = as.vector(freq_B_log_1_f),
    B_log_1_male = as.vector(freq_B_log_1_m),
    B_sp_1_female = as.vector(freq_B_sp_1_f),
    B_sp_1_male = as.vector(freq_B_sp_1_m),
    B_eo_1_female = as.vector(freq_B_eo_1_f),
    B_eo_1_male = as.vector(freq_B_eo_1_m),
    B_log_5_female = as.vector(freq_B_log_5_f),
    B_log_5_male = as.vector(freq_B_log_5_m),
    B_sp_5_female = as.vector(freq_B_sp_5_f),
    B_sp_5_male = as.vector(freq_B_sp_5_m),
    B_eo_5_female = as.vector(freq_B_eo_5_f),
    B_eo_5_male = as.vector(freq_B_eo_5_m),
    A_log_1_female = as.vector(freq_A_log_1_f),
    A_log_1_male = as.vector(freq_A_log_1_m),
    A_sp_1_female = as.vector(freq_A_sp_1_f),
    A_sp_1_male = as.vector(freq_A_sp_1_m),
    A_eo_1_female = as.vector(freq_A_eo_1_f),
    A_eo_1_male = as.vector(freq_A_eo_1_m),
    A_log_5_female = as.vector(freq_A_log_5_f),
    A_log_5_male = as.vector(freq_A_log_5_m),
    A_sp_5_female = as.vector(freq_A_sp_5_f),
    A_sp_5_male = as.vector(freq_A_sp_5_m),
    A_eo_5_female = as.vector(freq_A_eo_5_f),
    A_eo_5_male = as.vector(freq_A_eo_5_m)
  )
  
  row.names(almp_participants) <- names(freq_0_f)
  
  return(almp_participants)
}

f_risk_scores_vs_po <- function(df, list_treatments) {
  
  #############################################################
  #
  # Returns data frame with fairness (un)constraint risk scores
  # and optimal (minimal) potential outcomes for plotting
  #
  # df :               Data frame with simulation results.
  # list_treatments : List of treatments for which IAPOs/IATEs are estimated. 
  #
  #############################################################
  
  iapo_names <- paste0("iapo_", sub(" ", "_", list_treatments))
  
  df_risk_vs_po <- data.frame(matrix(NA, nrow=nrow(df), 0))
  
  df_risk_vs_po[, "iapo_opt"] <- df %>%
    select(all_of(iapo_names)) %>%
    apply(1, min)
  
  df_risk_vs_po[,'risk_score_log'] <- df["risk_score_log"]
  df_risk_vs_po[,'risk_score_sp'] <- df["risk_score_sp"]
  df_risk_vs_po[,'risk_score_eo'] <- df["risk_score_eo"]
  df_risk_vs_po[,'noprogram6'] <- ifelse(df['treatment6']=="no program", 1, 0)
  
  return(df_risk_vs_po)
}


# ---------------------------------------------------------------------------- #
# Prepare Data
# ---------------------------------------------------------------------------- #
# IATE estimates, by gender
db_iate_gender <- f_iate_viz_prep(db_sim, treatments_list)

treatment_names <- c("Vocational", "Computer", "Language", "Job Search", "Employment", "Personality")


# ---------------------------------------------------------------------------- #
# Risk Scores 
risk_scores <- grep("risk_score", names(db_sim), value = TRUE)[1:3]

db_risk_scores <- db_sim %>%
  group_by(female) %>%
  select(all_of(risk_scores))

# ---------------------------------------------------------------------------- #
# Citizen Gap Simulation results 
db_sim_viz_citizen <- f_sim_viz_prep(db_sim, "citizenship")


c_long_belgian_upper <- db_sim_viz_citizen %>% 
  filter(policy == "Belgian" & assignment == "upper")

c_long_austrian_upper <- db_sim_viz_citizen %>%
  filter(policy == "Austrian" & assignment == "upper")

c_long_austrian_lower <- db_sim_viz_citizen %>%
  filter(policy == "Austrian" & assignment == "lower")

c_long_belgian_lower <- db_sim_viz_citizen %>%
  filter(policy == "Belgian" & assignment == "lower")

c_long_nofair_upper <- db_sim_viz_citizen %>%
  filter(fairness == "0_log" & assignment == "upper")

c_long_nofair_lower <- db_sim_viz_citizen %>%
  filter(fairness == "0_log" & assignment == "lower")

# set visualization values
pre_ltu_citizen <- mean(db_sim[db_sim$swiss == 1, "y_exit12"])
pre_ltu_non_citizen <- mean(db_sim[db_sim$swiss == 0, "y_exit12"])


# ---------------------------------------------------------------------------- #
# Gender Gap Simulation results by Sub-groups
# 1. Unmarried Non-Citizen
db_nswiss_nmarried <- db_sim %>%
  filter(db_sim$swiss==0 & db_sim$married==0 ) %>%
  f_sim_viz_prep(., "gender") %>%
  filter(fairness=='0_log' & assignment=='upper')
  
# 2. Married Non-Citizen
db_nswiss_married <- db_sim %>%
  filter(db_sim$swiss==0 & db_sim$married==1 ) %>%
  f_sim_viz_prep(., "gender") %>%
  filter(fairness=='0_log' & assignment=='upper')

# 3. Unmarried Swiss Citizen
db_swiss_nmarried <- db_sim %>%
  filter(db_sim$swiss==1 & db_sim$married==0 ) %>%
  f_sim_viz_prep(., "gender") %>%
  filter(fairness=='0_log' & assignment=='upper')

# 4. Married Swiss Citizen
db_swiss_married <- db_sim %>%
  filter(db_sim$swiss==1 & db_sim$married==1 ) %>%
  f_sim_viz_prep(., "gender") %>%
  filter(fairness=='0_log' & assignment=='upper')


# ---------------------------------------------------------------------------- #
# Potential outcomes under optimal assignment
db_risk_vs_po <- f_risk_scores_vs_po(db_sim, treatments_list)

# ---------------------------------------------------------------------------- #
# Participants per program, by gender 
db_almp_participants <- f_almp_participants(db_sim)


# ---------------------------------------------------------------------------- #
# Appendix B.3, Figure 6: IATEs by Gender, Violin Plot
# ---------------------------------------------------------------------------- #

fig6_appendix_iates <- ggplot(db_iate_gender, aes(x=interaction(as.factor(female), y=IATE), 
                           y=Value, 
                           fill=as.factor(female))) +
  geom_violin(alpha=0.6) +
  geom_hline(yintercept = 0) + 
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "black", 
               aes(shape = as.factor(female)), 
               size = 3) +
  labs(y = "IATEs") +
  scale_fill_manual(
    labels = c("0" = "Male", "1" = "Female"),
    values = c("0" = "orange", "1" = "blue"),
    name = "Gender") +
  scale_shape_manual(
    labels = c("0" = "Male", "1" = "Female"),
    values = c("0" = shape_male, "1" = shape_female),
    name = "Gender") +
  scale_x_discrete(labels = rep(treatment_names, each=2)) +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig6-appendix-iates.pdf", fig6_appendix_iates, width = 10, height = 8)


# ---------------------------------------------------------------------------- #
# Appendix B.4, Figure 7: Histograms of Risk Scores
# ---------------------------------------------------------------------------- #
# (a) logistic regression
fig7a_risk <- ggplot(data=db_risk_scores, 
       aes(x = risk_score_log, fill = as.factor(female), group = as.factor(female))) +
  geom_histogram(bins=100, position = "identity", alpha = 0.4, colour = "white") +
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +
  scale_fill_manual(
    values = c("0" = "orange", "1" = "blue"),
    labels = c("0" = "Male", "1" = "Female")) +
  labs(y = "Frequency",
       x = "Risk Score",
       fill = "Gender:") +
  xlim(0,1) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig7a-appendix-risk.pdf", fig7a_risk, width = 6, height = 6)


# (b) statistical parity
fig7b_risk <- ggplot(data=db_risk_scores, 
       aes(x = risk_score_sp, fill = as.factor(female))) +
  geom_histogram(bins=75, position = "identity", alpha = 0.4, colour = "white") +
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +
  scale_fill_manual(
    values = c("0" = "orange", "1" = "blue"),
    labels = c("0" = "Male", "1" = "Female")) +
  labs(y = "Frequency",
       x = "Risk Score",
       fill = "Gender:") +
  xlim(0,1) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig7b-appendix-risk.pdf", fig7b_risk, width = 6, height = 6)


# (c) equal opportunity
fig7c_risk <- ggplot(data=db_risk_scores, 
       aes(x = risk_score_eo, fill = as.factor(female))) +
  # geom_density(alpha = 0.3) +
  geom_histogram(bins=75, position = "identity", alpha = 0.4, colour = "white") +
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +
  scale_fill_manual(
    values = c("0" = "orange", "1" = "blue"),
    labels = c("0" = "Male", "1" = "Female")) +
  labs(y = "Frequency",
       x = "Risk Score",
       fill = "Gender:") +
  xlim(0,1) +
  theme_classic() +
  theme(legend.position = c(0.7,0.6), 
        legend.justification = c("left", "bottom"), 
        legend.box.just = "left",
        # legend.box.background = element_rect(color="black", size=2),
        legend.key.size = unit(1.5, 'cm'),
        # legend.key = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "vertical",
        axis.title.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig7c-appendix-risk.pdf", fig7c_risk, width = 6, height = 6)


# ---------------------------------------------------------------------------- #
# Appendix B.6, Figure 8: Citizenship LTU gaps by Belgian/Austrian
# ---------------------------------------------------------------------------- #

# (a) Belgian Upper
fig8a_app_Belgian_optimal <- ggplot(c_long_belgian_upper, aes(x = capacity, 
                                 y = mean_value, 
                                 ymin = swiss_mean, 
                                 ymax = nonswiss_mean, 
                                 fill= policy, 
                                 alpha = as.factor(fairness))) +
  geom_ribbon() +
  geom_line(aes(y = swiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = nonswiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_point(aes(y = nonswiss_mean), color = 'black', size = 4, shape = 17, alpha = 1) +
  geom_point(aes(y = swiss_mean), color = 'black', size = 4, shape = 16, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_non_citizen), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_citizen), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_citizen, yend = pre_ltu_non_citizen),
               linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 0.9, y = 0.48, label = "ex-ante citizen gap"),
            hjust = -0.2, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 1.3, y = 0.479, xend = 0.95, yend = 0.46),
             curvature = -0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash')) +
  ylim(.32,.52) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig8a-Belgian-optimal.pdf", fig8a_app_Belgian_optimal, width = 7, height = 6)


# (c) Austrian Upper
fig8c_app_Austrian_optimal <- ggplot(c_long_austrian_upper, aes(x = capacity, 
                                 y = mean_value, 
                                 ymin = swiss_mean, 
                                 ymax = nonswiss_mean, 
                                 fill= policy, 
                                 alpha = as.factor(fairness))) +
  geom_ribbon() +
  geom_line(aes(y = swiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = nonswiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_point(aes(y = nonswiss_mean), color = 'black', size = 4, shape = 17, alpha = 1) +
  geom_point(aes(y = swiss_mean), color = 'black', size = 4, shape = 16, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_non_citizen), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_citizen), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_citizen, yend = pre_ltu_non_citizen),
               linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash')) +
  ylim(.32,.52) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig8c-Austrian-optimal.pdf", fig8c_app_Austrian_optimal, width = 7, height = 6)


# (b) Belgian Lower
fig8b_app_Belgian_random <- ggplot(c_long_belgian_lower, aes(x = capacity, 
                                  y = mean_value, 
                                  ymin = swiss_mean, 
                                  ymax = nonswiss_mean, 
                                  fill= policy, 
                                  alpha = as.factor(fairness))) +
  geom_ribbon() +
  geom_line(aes(y = swiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = nonswiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_point(aes(y = nonswiss_mean), color = 'black', size = 4, shape = 17, alpha = 1) +
  geom_point(aes(y = swiss_mean), color = 'black', size = 4, shape = 16, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_non_citizen), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_citizen), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_citizen, yend = pre_ltu_non_citizen),
               linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash')) +
  ylim(.32,.52) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig8b-Belgian-random.pdf", fig8b_app_Belgian_random, width = 7, height = 6)


# (d) Austrian Lower
fig8d_app_Austrian_random <- ggplot(c_long_austrian_lower, aes(x = capacity, 
                                 y = mean_value, 
                                 ymin = swiss_mean, 
                                 ymax = nonswiss_mean, 
                                 fill= policy, 
                                 alpha = as.factor(fairness))) +
  geom_ribbon() +
  geom_line(aes(y = swiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_line(aes(y = nonswiss_mean, linetype = as.factor(fairness)), colour = 'black', size = 1, alpha = 0.5) +
  geom_point(aes(y = nonswiss_mean, shape = as.factor(1 - swiss)), color = 'black', size = 4, alpha = 1) +
  geom_point(aes(y = swiss_mean, shape = as.factor(swiss)), color = 'black', size = 4, alpha = 1) +
  geom_point(aes(x = 0.9, y = pre_ltu_non_citizen), size = 4, shape = 17) +
  geom_point(aes(x = 0.9, y = pre_ltu_citizen), size = 4, shape = 16) +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_citizen, yend = pre_ltu_non_citizen),
               linetype = "solid", color = "black", size = 0.75) +
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash'),
                        labels = c("0_log" = "None", "1_sp" = "Stat. Par.", "2_eo" = "Equal Opp.")) +
  scale_shape_manual(values = c("0" = 17, "1" = 16), name = "Citizenship:",
                     labels = c("0" = "Non-Citizen", "1" = "Swiss")) +
  ylim(.32,.52) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate",
       linetype = "Fair:") +
  theme_classic() + 
  guides(fill = FALSE,
         alpha = FALSE) +
  theme(legend.position = "none", 
        legend.justification = c("left", "bottom"), 
        legend.box.just = "left",
        # legend.box.background = element_rect(color="black", size=2),
        legend.key.size = unit(1.5, 'cm'),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "vertical",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) 

ggsave("output/Fig8d-Austrian-random.pdf", fig8d_app_Austrian_random, width = 7, height = 6)


# ---------------------------------------------------------------------------- #
# Appendix B.6, Figure 9: Citizenship Gaps Overall
# ---------------------------------------------------------------------------- #

# (a) optimal/upper
fig9a_app_optimal <- ggplot(c_long_nofair_upper, 
       aes(x = capacity, 
           y = mean_value, 
           ymin = swiss_mean, 
           colour = policy, 
           ymax = nonswiss_mean, 
           fill = policy)) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(y = mean_value), 
            size = 1) +
  geom_line(aes(y = nonswiss_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = swiss_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05))+
  geom_point(size = 6, shape = 18, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = nonswiss_mean), color = 'black', size = 4, shape = 17, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(y = swiss_mean), color = 'black', size = 4, shape = 16, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(x = 0.9, y = pre_ltu_non_citizen), size = 4, shape = 17, color = "black") +
  geom_point(aes(x = 0.9, y = pre_ltu_citizen), size = 4, shape = 16, color = "black") +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_citizen, yend = pre_ltu_non_citizen),
               linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 0.9, y = 0.48, label = "ex-ante citizen gap"),
            hjust = -0.2, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 1.3, y = 0.479, xend = 0.95, yend = 0.46),
             curvature = -0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 2.4, y = 0.44, label = "ex-post population rate"),
            hjust = 0, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 2.39, y = 0.439, xend = 2.4, yend = 0.374),
             curvature = 0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate") +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  ylim(.32, .52) +
  theme_classic() +
  theme(legend.position =  "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig9a-appendix-optimal.pdf", fig9a_app_optimal, width = 7, height = 6)



# (b) radom/lower
fig9b_app_ramdom <- ggplot(c_long_nofair_lower, 
       aes(x = capacity, 
           y = mean_value, 
           ymin = swiss_mean, 
           colour = policy, 
           ymax = nonswiss_mean, 
           fill = policy)) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(y = mean_value), 
            size = 1) +
  geom_line(aes(y = nonswiss_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05)) +
  geom_line(aes(y = swiss_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05))+
  geom_point(size = 6, shape = 18, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = nonswiss_mean, shape = as.factor(1-swiss)), color = 'black', size = 4, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(y = swiss_mean, shape = as.factor(swiss)), color = 'black', size = 4, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(x = 0.9, y = pre_ltu_non_citizen), size = 4, shape = 17, color = "black") +
  geom_point(aes(x = 0.9, y = pre_ltu_citizen), size = 4, shape = 16, color = "black") +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_citizen, yend = pre_ltu_non_citizen),
               linetype = "solid", color = "black", size = 0.75) +
  labs(x = "Capacity Multiplier",
       y = "LTU Rate",
       colour = "Policy:") +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_shape_manual(values = c("0" = 17, "1" = 16), name = "Citizenship:",
                     labels = c("0" = "Non-Citizen", "1" = "Swiss")) +
  ylim(.32,.52) +
  theme_classic() +
  guides(fill = FALSE,
         alpha = FALSE) +
  theme(legend.position = "none", 
        legend.justification = c("left", "bottom"), 
        legend.box.just = "left",
        # legend.box.background = element_rect(color="black", size=2),
        legend.key.size = unit(1.5, 'cm'),
        legend.key = element_rect(fill = "white", colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "vertical",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) 

ggsave("output/Fig9b-appendix-random.pdf", fig9b_app_ramdom, width = 7, height = 6)


# ---------------------------------------------------------------------------- #
# Figure 10: Married-Citizenship 
# ---------------------------------------------------------------------------- #
# (a) Unmarried Non-Citizen
fig10a_app <- ggplot(db_nswiss_nmarried, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
  geom_ribbon(alpha=.25) +
  geom_point(size = 4, shape = 18) +
  geom_point(aes(y = female_mean, shape = as.factor(female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean, shape = as.factor(1-female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  #ylim(.3,.5) +
  geom_line(aes(y = mean_value),size=1) +
  geom_line(aes(y = female_mean), colour = 'black') +
  geom_line(aes(y = male_mean), colour = 'black')+
  labs(#title = "Non-Citizen, Unmarried",
    x = "Capacity Multiplier",
    y = "LTU Share")+
  scale_shape_manual(values = c("0" = 16, "1" = 17), name = "Gender:",
                     labels = c("0" = "Male", "1" = "Female")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  theme_classic()+
  theme(legend.position =  "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig10a-appendix.pdf", fig10a_app, width = 7, height = 6)


# (b) Married Non-Citizen
fig10b_app <- ggplot(db_nswiss_married, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
  geom_ribbon(alpha=.25) +
  geom_point(size = 4, shape = 18) +
  geom_point(aes(y = female_mean, shape = as.factor(female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean, shape = as.factor(1-female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  #ylim(.3,.5) +
  geom_line(aes(y = mean_value),size=1) +
  geom_line(aes(y = female_mean), colour = 'black') +
  geom_line(aes(y = male_mean), colour = 'black')+
  labs(# title = "Non-Swiss, Married",
    x = "Capacity Multiplier",
    y = "LTU Share")+
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_shape_manual(values = c("0" = 16, "1" = 17), name = "Gender:",
                     labels = c("0" = "Male", "1" = "Female")) +
  theme_classic()+
  theme(legend.position =  "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig10b-appendix.pdf", fig10b_app, width = 7, height = 6)



# 3. Unmarried Swiss Citizen
fig10c_app <- ggplot(db_swiss_nmarried, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
  geom_ribbon(alpha=.25) +
  geom_point(size = 4, shape = 18) + 
  geom_point(aes(y = female_mean, shape = as.factor(female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean, shape = as.factor(1-female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  #ylim(.3,.5) +
  geom_line(aes(y = mean_value),size=1) +
  geom_line(aes(y = female_mean), colour = 'black') +
  geom_line(aes(y = male_mean), colour = 'black')+
  labs(# title = "Swiss, Unmarried",
    x = "Capacity Multiplier",
    y = "LTU Share")+
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor),
                    name = "Policy:") +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor),
                      name = "Policy:") +
  scale_shape_manual(values = c("0" = 16, "1" = 17), name = "Gender:",
                     labels = c("0" = "Male", "1" = "Female")) +
  theme_classic() +
  theme(legend.position = c(0.01, 0.01), 
        legend.justification = c("left", "bottom"), 
        legend.box.just = "left",
        # legend.box.background = element_rect(color="black", size=2),
        legend.background = element_rect(fill = alpha("white", 0)),
        legend.key.size = unit(1, 'cm'),
        legend.key = element_rect(colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "horizontal",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) 

ggsave("output/Fig10c-appendix.pdf", fig10c_app, width = 7, height = 6)


# (d) Married Swiss Citizen
fig10d_app <- ggplot(db_swiss_married, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
  geom_ribbon(alpha=.25) +
  geom_point(size = 4, shape = 18) +
  geom_point(aes(y = female_mean, shape = as.factor(female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean, shape = as.factor(1-female)), 
             color = 'black', size = 2, alpha = 1, position = position_dodge(width = 0.05)) +
  #ylim(.3,.5) +
  geom_line(aes(y = mean_value),size=1) +
  geom_line(aes(y = female_mean), colour = 'black') +
  geom_line(aes(y = male_mean), colour = 'black')+
  labs(# title = "Non-Swiss, Married",
    x = "Capacity Multiplier",
    y = "LTU Share") +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_shape_manual(values = c("0" = 16, "1" = 17), name = "Gender:",
                     labels = c("0" = "Male", "1" = "Female")) +
  theme_classic()+
  theme(legend.position =  "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

ggsave("output/Fig10d-appendix.pdf", fig10d_app, width = 7, height = 6)



# ---------------------------------------------------------------------------- #
# Appendix B.8, Figure 11: Risk Scores vs. optimal potential outcomes 
# ---------------------------------------------------------------------------- #

# Spearman's rank correlations
cor(db_risk_vs_po$risk_score_log, db_risk_vs_po$iapo_opt, method = "spearman")
cor(db_risk_vs_po$risk_score_sp, db_risk_vs_po$iapo_opt, method = "spearman")
cor(db_risk_vs_po$risk_score_eo, db_risk_vs_po$iapo_opt, method = "spearman")


# (a) logistic regression
fig11a_app <- ggplot(db_risk_vs_po, aes(x = risk_score_log, y=iapo_opt, colour=as.factor(noprogram6))) +
  geom_point(alpha=0.7, size=0.7) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  xlim(0.1,0.92) +
  labs(x = "Unconstraint Risk Scores",
       y = "Potential outcomes under optimal assignment") +
  scale_colour_manual(values = c("0" = "blue", "1" = "orange"),
                      labels = c("0" = "ALMP", "1" = "No ALMP"),
                      name = "Treatment:"
  ) +
  geom_text(aes(x = 0.2, y = 0.345, label = "x=y"),
            hjust = 0, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 0.23, y = 0.34, xend = 0.3, yend = 0.31),
             curvature = 0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2), 
        legend.justification = c("center"), 
        legend.box.just = "right",
        # legend.box.background = element_rect(color="black", size=2),
        legend.background = element_rect(fill = alpha("white", 0)),
        legend.key.size = unit(1, 'cm'),
        legend.key = element_rect(colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "vertical",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("output/Fig11a-appendix.pdf", fig11a_app, width = 7, height = 6, dpi = 300, device = "pdf")


# (b) statistical parity
fig11b_app <- ggplot(db_risk_vs_po, aes(x = risk_score_sp, y=iapo_opt, colour=as.factor(noprogram6))) +
  geom_point(alpha=0.7, size=0.7) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  xlim(0.1,0.92) +
  labs(x = "Risk Scores under Statistical Parity",
       y = "Potential outcomes under optimal assignment") +
  scale_colour_manual(values = c("0" = "blue", "1" = "orange"),
                      labels = c("0" = "ALMP", "1" = "No ALMP"),
                      name = "Treatment:"
  ) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.25), 
        legend.justification = c("center"), 
        legend.box.just = "right",
        # legend.box.background = element_rect(color="black", size=2),
        legend.background = element_rect(fill = alpha("white", 0)),
        legend.key.size = unit(1, 'cm'),
        legend.key = element_rect(colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "vertical",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("output/Fig11b-appendix.pdf", fig11b_app, width = 7, height = 6, dpi = 300)


# (c) Equal Opportunity
fig11c_app <- ggplot(db_risk_vs_po, aes(x = risk_score_eo, y=iapo_opt, colour=as.factor(noprogram6))) +
  geom_point(alpha=0.7, size=0.7) +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  xlim(0.1,0.92) +
  labs(x = "Risk Scores under Equal Opportunity",
       y = "Potential outcomes under optimal assignment") +
  scale_colour_manual(values = c("0" = "blue", "1" = "orange"),
                      labels = c("0" = "ALMP", "1" = "No ALMP"),
                      name = "Treatment:"
  ) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.25), 
        legend.justification = c("center"), 
        legend.box.just = "right",
        # legend.box.background = element_rect(color="black", size=2),
        legend.background = element_rect(fill = alpha("white", 0)),
        legend.key.size = unit(1, 'cm'),
        legend.key = element_rect(colour = "black"),
        legend.text = element_text(size = 16, colour = "black"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.direction = "vertical",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16)) +
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("output/Fig11c-appendix.pdf", fig11c_app, width = 7, height = 6, dpi = 300)


# ---------------------------------------------------------------------------- #
# Appendix B.9 , Figure 12 
# ---------------------------------------------------------------------------- #

# data in "db_almp_participants", bar plots prepared in Excel.



# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #