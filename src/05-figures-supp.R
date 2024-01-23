# ---------------------------------------------------------------------------- 
# 05 Figures Supplemantary
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
# Preparations
# ---------------------------------------------------------------------------- 
risk_scores <- grep("risk_score", names(db_sim), value = TRUE)[1:3]

db_risk_scores <- db_sim %>%
  group_by(female) %>%
  select(all_of(risk_scores))

# select variables
selected_vars <- grep("iapo_policy_", names(db_sim), value = TRUE)
# drop "individual fairness"
selected_vars <- selected_vars[!grepl("_if_", selected_vars)]

for (var in selected_vars) {
  lo_ineq_averse <- db_sim[,which(colnames(db_sim)==var)]**(1/.75) # Rescale policy outcomes for epsilon = .25
  med_ineq_averse <- db_sim[,which(colnames(db_sim)==var)]**2 # epsilon = .5 
  hi_ineq_averse <- db_sim[,which(colnames(db_sim)==var)]**4 # epsilon = .75
  
  db_sim[ , ncol(db_sim) + 1] <- lo_ineq_averse                  # Create low inequality aversion column
  colnames(db_sim)[ncol(db_sim)] <- paste0(var, "_lo_ineq_averse")  
  
  db_sim[ , ncol(db_sim) + 1] <- med_ineq_averse                  # Create medium inequality aversion column
  colnames(db_sim)[ncol(db_sim)] <- paste0(var, "_med_ineq_averse")   
  
  db_sim[ , ncol(db_sim) + 1] <- hi_ineq_averse                  # Create high inequality aversion column
  colnames(db_sim)[ncol(db_sim)] <- paste0(var, "_hi_ineq_averse")  # 
  
}

expanded_vars <- grep("iapo_policy_", names(db_sim), value = TRUE)
expanded_vars <- expanded_vars[!grepl("_if_", expanded_vars)]

# citizenship
db_citizen <- db_sim %>%
  group_by(swiss) %>%
  summarise(across(all_of(selected_vars), mean))

db_policies <- db_sim %>%
  summarise(across(all_of(selected_vars), mean))

swiss_long <- f_reshape_long(db_citizen)
long <- f_reshape_long(db_policies)

swiss_long[ , ncol(swiss_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(swiss_long)[ncol(swiss_long)] <-"swiss_mean"     # Rename column name
swiss_long$swiss_mean = swiss_long$swiss * swiss_long$mean_value

swiss_long[ , ncol(swiss_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(swiss_long)[ncol(swiss_long)] <-"nonswiss_mean"  # Rename column name
swiss_long$nonswiss_mean = (1-swiss_long$swiss) * swiss_long$mean_value

c_long <- filter(swiss_long, swiss==1)
noncit_long <- filter(swiss_long, swiss==0)

c_long$nonswiss_mean <- noncit_long$nonswiss_mean
c_long$mean_value <- long$mean_value

c_long_belgian_upper <- filter(c_long, policy == "Belgian" & assignment == "upper")
c_long_austrian_upper <- filter(c_long, policy == "Austrian" & assignment == "upper")
c_long_austrian_lower <- filter(c_long, policy == "Austrian" & assignment == "lower")
c_long_belgian_lower <- filter(c_long, policy == "Belgian" & assignment == "lower")

c_long_nofair_upper <- filter(c_long, fairness == "0_log" & assignment == "upper")
c_long_nofair_lower <- filter(c_long, fairness == "0_log" & assignment == "lower")

pre_ltu_citizen <- mean(db_sim[db_sim$swiss == 1, "y_exit12"])
pre_ltu_non_citizen <- mean(db_sim[db_sim$swiss == 0, "y_exit12"])

belgiumcolor="#1704E0"
austriacolor="#E03D2B"

# ---------------------------------------------------------------------------- 
# Figure 6: IATEs by Gender, Violin Plot
# ---------------------------------------------------------------------------- 
db_sim %>% 
  group_by(female) %>%
  select(starts_with('iate_')) %>% 
  colMeans()

# plot IATEs
db_gender <- db_sim %>%
  group_by(female) %>%
  reframe(across(starts_with('iate_')))

db_gender_long <- db_gender %>%
  pivot_longer(cols = starts_with("iate_"), 
               names_to = "IATE",
               values_to = "Value")

db_gender_long <- db_gender_long %>%
  mutate(IATE = sub("iate_", "", IATE)) %>%
  mutate(IATE = sub("_", " ", IATE))

db_gender_long$IATE <- factor(db_gender_long$IATE, treatments_list[-1])

treatment_names <-  c("Vocational", "Computer", "Language", "Job Search", "Employment", "Personality")

ggplot(db_gender_long, aes(x=interaction(as.factor(female), y=IATE), 
                           y=Value, 
                           fill=as.factor(female))) +
  geom_violin() +
  geom_hline(yintercept = 0) + 
  stat_summary(fun.y = "mean", 
               geom = "point", 
               color = "black", 
               aes(shape = as.factor(female)), 
               size = 3) +
  # stat_summary(fun.y = "mean", 
  #              geom="point", color="black", shape = 3, size = 3) +
  labs(y = "IATEs") +
  scale_fill_discrete(
    # values = c("0" = "#D95F02", "1" = "#1B9E77"),
    labels = c("0" = "Male", "1" = "Female"),
    name = "Gender") +
  scale_shape_discrete(
    labels = c("0" = "Male", "1" = "Female"),
    name = "Gender") +
  scale_x_discrete(labels = rep(treatment_names, each=2)) +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x=element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))



# ---------------------------------------------------------------------------- 
# Figure 7: Density Plots Risk Scores
# ---------------------------------------------------------------------------- 
# logistic regression
ggplot(data=db_risk_scores, 
       aes(x = risk_score_log, fill = as.factor(female), group = as.factor(female))) +
  geom_histogram(bins=75, position = "identity", alpha = 0.4, colour = "white") +
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +
  scale_fill_discrete(# values = c("0" = "#D95F02", "1" = "#1B9E77"),
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


# statistical parity
ggplot(data=db_risk_scores, 
       aes(x = risk_score_sp, fill = as.factor(female))) +
  geom_histogram(bins=75, position = "identity", alpha = 0.4, colour = "white") +
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +
  scale_fill_discrete(# values = c("0" = "#D95F02", "1" = "#1B9E77"),
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

# equal opportunity
ggplot(data=db_risk_scores, 
       aes(x = risk_score_eo, fill = as.factor(female))) +
  # geom_density(alpha = 0.3) +
  geom_histogram(bins=75, position = "identity", alpha = 0.4, colour = "white") +
  geom_vline(xintercept = 0.5, colour = "black", size = 0.5) +
  scale_fill_discrete(# values = c("0" = "#D95F02", "1" = "#1B9E77"),
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


# ---------------------------------------------------------------------------- 
# Figure 8: Citizenship by Belgian/Austrian
# ---------------------------------------------------------------------------- 

# Belgian Upper
ggplot(c_long_belgian_upper, aes(x = capacity, 
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

# Austrian Upper
ggplot(c_long_austrian_upper, aes(x = capacity, 
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

# Belgian Lower
ggplot(c_long_belgian_lower, aes(x = capacity, 
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

# Austrian Lower
ggplot(c_long_austrian_lower, aes(x = capacity, 
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

# copy-paste legend into Overleaf!

# ---------------------------------------------------------------------------- 
# Figure 9: Citizenship Overall
# ---------------------------------------------------------------------------- 
# Upper
ggplot(c_long_nofair_upper, 
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

# Lower
ggplot(c_long_nofair_lower, 
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

# ---------------------------------------------------------------------------- 
# Figure 10: Married-Citizenship 
# ---------------------------------------------------------------------------- 


# ---------------------------------------------------------------------------- 
# Figure 11: Welfare Analysis and Results
# ---------------------------------------------------------------------------- 
db_welfare <- db_sim %>%
  summarise_at(expanded_vars, mean)

welfare_long <-reshape_long(db_welfare)

# no ineq aversion
upperlim<-.42
no_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==0)#,welfare_long$policy=="Belgian")

ggplot(data=no_welfare_long, 
       aes(x = capacity, 
           y = mean_value, 
           colour = interaction(policy, assignment), 
           group = interaction(policy, fairness, assignment))) +
  geom_line(aes(linetype=as.factor(fairness)), size = 1) +
  geom_point() +
  #ylim(0.35,.4) +
  labs(x = "Capacity Multiplier",
       y = "Average Risk of LTU") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

# 1 ineq aversion
lo_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==1)#,welfare_long$policy=="Belgian")

ggplot(data=lo_welfare_long, 
       aes(x=capacity, 
           y=mean_value, 
           colour=interaction(policy,assignment), 
           group=interaction(policy,fairness,assignment))) +
  geom_line(aes(linetype=as.factor(fairness)), size = 1) +
  geom_point() +
  #ylim(0.35,.4) +
  labs(x = "Capacity Multiplier",
       y = "Average Risk of LTU") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

# 2 ineq aversion
med_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==2)#,welfare_long$policy=="Belgian")

ggplot(data=med_welfare_long, 
       aes(x=capacity, 
           y=mean_value, 
           colour=interaction(policy,assignment), 
           group=interaction(policy,fairness,assignment))) +
  geom_line(aes(linetype=as.factor(fairness)), size = 1) +
  geom_point() +
  #ylim(0.35,.4) +
  labs(x = "Capacity Multiplier",
       y = "Average Risk of LTU") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

# 3 ineq aversion
hi_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==3)#,welfare_long$policy=="Belgian")
ggplot(data=hi_welfare_long, 
       aes(x=capacity, 
           y=mean_value, 
           colour=interaction(policy,assignment), 
           group=interaction(policy,fairness,assignment))) +
  geom_line(aes(linetype=as.factor(fairness)), size = 1) +
  geom_point() +
  #ylim(0.35,.4) +
  labs(x = "Capacity Multiplier",
       y = "Average Risk of LTU") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))