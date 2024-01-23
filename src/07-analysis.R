library(tidyverse)
library(gridExtra)
library(grid)

file="data/1203_ALMP_effects_risk_fairFemale_sim.csv"
test_db = read.csv(file)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#################################################
#
# Create inequality-aversion-weighted outcomes
#
################################################

selected_vars <- grep("iapo_policy_", names(test_db), value = TRUE)
selected_vars <- selected_vars[!grepl("_if_", selected_vars)]

for (var in selected_vars) {
  lo_ineq_averse <- test_db[,which(colnames(test_db)==var)]**(1/.75) # Rescale policy outcomes for epsilon = .25
  med_ineq_averse <- test_db[,which(colnames(test_db)==var)]**2 # epsilon = .5 
  hi_ineq_averse <- test_db[,which(colnames(test_db)==var)]**4 # epsilon = .75
  
  test_db[ , ncol(test_db) + 1] <- lo_ineq_averse                  # Create low inequality aversion column
  colnames(test_db)[ncol(test_db)] <- paste0(var, "_lo_ineq_averse")  
  
  test_db[ , ncol(test_db) + 1] <- med_ineq_averse                  # Create medium inequality aversion column
  colnames(test_db)[ncol(test_db)] <- paste0(var, "_med_ineq_averse")   
  
  test_db[ , ncol(test_db) + 1] <- hi_ineq_averse                  # Create high inequality aversion column
  colnames(test_db)[ncol(test_db)] <- paste0(var, "_hi_ineq_averse")  # 
  
}

expanded_vars <- grep("iapo_policy_", names(test_db), value = TRUE)

############################################################################################

###################################
#
# Reshape Function
#
###################################

reshape_long <- function(df) {
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

###################################################
#
#  Welfare Analysis
#
###################################################



welfare_df <- test_db %>%
  summarise_at(expanded_vars, mean)

welfare_long <-reshape_long(welfare_df)

upperlim<-.42
no_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==0)#,welfare_long$policy=="Belgian")
welf_no_aversion<-ggplot(data=no_welfare_long, aes(x=capacity, y=mean_value, colour=interaction(policy,assignment), group=interaction(policy,fairness,assignment))) +
  geom_line(aes(linetype=as.factor(fairness)))+
  geom_point()+
  #ylim(0.35,.4) +
  labs(title = " No Inequality Aversion",
       x = "Capacity Multiplier",
       y = "Average Risk of Long-Term Unemployment")

lo_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==1)#,welfare_long$policy=="Belgian")
welf_lo_aversion <- ggplot(data=lo_welfare_long, aes(x=capacity, y=mean_value, colour=interaction(policy,assignment), group=interaction(policy,fairness,assignment))) +
  geom_line(aes(linetype=as.factor(fairness)))+
  geom_point()+
  #ylim(0.35,.4) +
  labs(title = "Low Inequality Aversion",
       x = "Capacity Multiplier",
       y = "Average Risk of Long-Term Unemployment")

med_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==2)#,welfare_long$policy=="Belgian")
welf_med_aversion <- ggplot(data=med_welfare_long, aes(x=capacity, y=mean_value, colour=interaction(policy,assignment), group=interaction(policy,fairness,assignment))) +
  geom_line(aes(linetype=as.factor(fairness)))+
  geom_point()+
  #ylim(0.35,.4) +
  labs(title = "Med Inequality Aversion",
       x = "Capacity Multiplier",
       y = "Average Risk of Long-Term Unemployment")

hi_welfare_long <- welfare_long %>% filter(welfare_long$ineq_aversion==3)#,welfare_long$policy=="Belgian")
welf_hi_aversion <- ggplot(data=hi_welfare_long, aes(x=capacity, y=mean_value, colour=interaction(policy,assignment), group=interaction(policy,fairness,assignment))) +
  geom_line(aes(linetype=as.factor(fairness)))+
  geom_point()+
  #ylim(0.35,.4) +
  theme(legend.position = "bottom")+
  labs(title = "Hi Inequality Aversion",
       x = "Capacity Multiplier",
       y = "Average Risk of Long-Term Unemployment")


legend<-g_legend(welf_hi_aversion)
grid.arrange(arrangeGrob(welf_no_aversion + theme(legend.position="none"),
                         welf_lo_aversion + theme(legend.position="none"),
                         welf_med_aversion + theme(legend.position="none"),
                         welf_hi_aversion + theme(legend.position="none"),
                         nrow=1),
             legend, nrow=2,heights=c(10, 1))


####################################################
#
# LTU Gender Gap Analysis
#
##################################################
gender_df <-test_db %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

df <-test_db %>%
  summarise(across(all_of(selected_vars), mean))

gender_long <- reshape_long(gender_df)
long <- reshape_long(df)

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

pre_ltu_female <- mean(test_db[test_db$female==1, "y_exit12"])
pre_ltu_male <- mean(test_db[test_db$female==0, "y_exit12"])

belgiumcolor="#1704E0"
austriacolor="#E03D2B"

##########################################

### Upper
gendergap_upper <- ggplot(f_long_nofair_upper, 
                        aes(x = capacity, 
                            y = mean_value, 
                            ymin = male_mean, 
                            colour = policy, 
                            ymax = female_mean, 
                            fill = policy)) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(y = mean_value), 
            size = 1) +
  # geom_line(aes(y = female_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05)) +
  # geom_line(aes(y = male_mean), colour = 'black', size = 0.5, position = position_dodge(width = 0.05))+
  geom_point(size = 6, shape = 18, position = position_dodge(width = 0.05)) +
  geom_point(aes(y = female_mean), color = 'black', size = 4, shape = 17, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(y = male_mean), color = 'black', size = 4, shape = 16, alpha = 1,
             position = position_dodge(width = 0.05)) +
  geom_point(aes(x = 0.9, y = pre_ltu_female), size = 4, shape = 17, color = "black") +
  geom_point(aes(x = 0.9, y = pre_ltu_male), size = 4, shape = 16, color = "black") +
  geom_segment(aes(x = 0.9, xend = 0.9, y = pre_ltu_male, yend = pre_ltu_female),
               linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 0.9, y = 0.425, label = "ex ante gender gap"),
            hjust = -0.2, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 1.3, y = 0.423, xend = 0.95, yend = 0.415),
             curvature = -0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  geom_text(aes(x = 2.7, y = 0.395, label = "ex post population share"),
            hjust = 0, vjust = 0,
            color = "black", size = 8) +
  geom_curve(aes(x = 2.65, y = 0.395, xend = 2.45, yend = 0.373),
             curvature = 0.3, arrow = arrow(length = unit(0.1, "inches")),
             linetype = "solid", color = "black", size = 0.75) +
  labs(x = "Capacity Multiplier",
       y = "LTU Share") +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  ylim(.34,.44) +
  theme_classic() +
  theme(legend.position =  "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

  
### Lower
gendergap_lower <- ggplot(f_long_nofair_lower, 
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
       y = "LTU Share",
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

# legend<-g_legend(gendergap_lower)
# 
# grid.arrange(arrangeGrob(gendergap_upper + theme(legend.position="none"),
#                          gendergap_lower + theme(legend.position="none"),
#                          #gendergap_emp + theme(legend.position="none"),
#                          nrow=1),
#              legend, nrow=2,heights=c(10, 1))


##########################################

fairness_belgian_upper <- ggplot(f_long_belgian_upper, aes(x = capacity, 
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
  geom_text(aes(x = 0.9, y = 0.425, label = "ex ante gender gap"),
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
       y = "LTU Share") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))



### Austrian Upper
fairness_austrian_upper <- ggplot(f_long_austrian_upper, aes(x = capacity, 
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
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.66, "2_eo" = .33),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "EO")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_linetype_manual(values = c('0_log' = 'solid', '1_sp' = 'longdash', '2_eo' = 'dotdash')) +
  ylim(.33,.44) +
  labs(x = "Capacity Multiplier",
       y = "LTU Share") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))



### Belgian Lower
fairness_belgian_lower <- ggplot(f_long_belgian_lower, 
                                 aes(x = capacity, 
                                     y=mean_value, 
                                     ymin = male_mean, 
                                     ymax = female_mean, 
                                     fill=policy, 
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
       y = "LTU Share") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

### Austrian Lower
fairness_austrian_lower <- ggplot(f_long_austrian_lower, 
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
                        labels = c("0_log" = "No", "1_sp" = "Indep.", "2_eo" = "Equal Opp."))  +
  ylim(.33,.44) +
  labs(x = "Capacity Multiplier",
       y = "LTU Share",
       linetype = "Fairness:") +
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


# ggplot(f_long_austrian_lower, aes(x = capacities, y = mean_value, shape = shape)) +
#   geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = NULL), alpha = 0.3, show.legend = "both") +
#   geom_point(aes(y = ymin), size = 3) +
#   geom_point(aes(y = ymax), size = 3) +
#   scale_shape_manual(name = "Point Shapes",
#                      values = c(16, 17, 18)) +
#   theme_minimal()
# 
# 
# legend<-g_legend(fairness_austrian_lower)
# 
# grid.arrange(arrangeGrob(fairness_belgian_upper + theme(legend.position="none"),
#                          fairness_belgian_lower+ theme(legend.position="none"),
#                          fairness_austrian_upper + theme(legend.position="none"),
#                          fairness_austrian_lower + theme(legend.position="none"),
#                          nrow=2),
#              legend, nrow=2,heights=c(10, 1))







####################################################
#
# LTU Citizen Gap Analysis
#
##################################################
swiss_df <-test_db %>%
  group_by(swiss) %>%
  summarise(across(all_of(selected_vars), mean))

df <-test_db %>%
  summarise(across(all_of(selected_vars), mean))

swiss_long <- reshape_long(swiss_df)
long <- reshape_long(df)

swiss_long[ , ncol(swiss_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(swiss_long)[ncol(swiss_long)] <-"swiss_mean"  # Rename column name
swiss_long$swiss_mean = swiss_long$swiss * swiss_long$mean_value

swiss_long[ , ncol(swiss_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(swiss_long)[ncol(swiss_long)] <-"nonswiss_mean"  # Rename column name
swiss_long$nonswiss_mean = (1-swiss_long$swiss) * swiss_long$mean_value

f_long <- filter(swiss_long, swiss==1)
m_long <- filter(swiss_long, swiss==0)

f_long$nonswiss_mean<-m_long$nonswiss_mean
f_long$mean_value<-long$mean_value

f_long_belgian_upper <- filter(f_long,policy=="Belgian" & assignment=="upper")
f_long_austrian_upper <- filter(f_long,policy=="Austrian" & assignment=="upper")
f_long_austrian_lower <- filter(f_long,policy=="Austrian" & assignment=="lower")
f_long_belgian_lower <- filter(f_long,policy=="Belgian" & assignment=="lower")

f_long_nofair_upper <- filter(f_long,fairness=="0_log" & assignment=="upper")
f_long_nofair_lower <- filter(f_long,fairness=="0_log" & assignment=="lower")

belgiumcolor="#1704E0"
austriacolor="#E03D2B"


swissgap_upper<-ggplot(f_long_nofair_upper, aes(x = capacity, y=mean_value, ymin = nonswiss_mean, colour=policy, ymax = swiss_mean, fill=policy,group=policy)) +
  geom_ribbon(alpha=.25) +
  geom_point()+
  ylim(.325,.485) +
  geom_line(aes(y = mean_value),size=1) +
  geom_line(aes(y = swiss_mean), colour = 'black') +
  geom_line(aes(y = nonswiss_mean), colour = 'black')+
  labs(title = "Optimal Program",
       x = "Capacity Multiplier",
       y = "LTU Share")+
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  theme(legend.position = "bottom")

swissgap_lower<-ggplot(f_long_nofair_lower, aes(x = capacity, y=mean_value, ymin = nonswiss_mean, colour=policy, ymax = swiss_mean, fill=policy,group=policy)) +
  geom_ribbon(alpha=.25) +
  geom_point()+
  ylim(.325,.485) +
  geom_line(aes(y = mean_value),size=1) +
  geom_line(aes(y = swiss_mean), colour = 'black') +
  geom_line(aes(y = nonswiss_mean), colour = 'black')+
  labs(title = "Random Program",
       x = "Capacity Multiplier",
       y = "LTU Share")+
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  theme(legend.position = "bottom")

legend<-g_legend(swissgap_lower)

grid.arrange(arrangeGrob(swissgap_upper + theme(legend.position="none"),
                         swissgap_lower + theme(legend.position="none"),
                         #swissgap_emp + theme(legend.position="none"),
                         nrow=1),
             legend, nrow=2,heights=c(10, 1))


fairness_belgian_upper <- ggplot(f_long_belgian_upper, aes(x = capacity, y=mean_value, ymin = nonswiss_mean, ymax = swiss_mean, fill=policy, alpha =fairness,group=fairness)) +
  geom_ribbon() +
  ylim(.32,.485)+
  #geom_line(aes(y = mean_value), colour = 'black') +
  geom_line(aes(y = swiss_mean), colour = 'black') +
  geom_line(aes(y = nonswiss_mean), colour = 'black')+
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.5, "2_eo" = .25, "3_if"=.1),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "Separation", "3_if" = "Individual Fairness")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  labs(title = "Belgian Prioritization and Optimal Program",
       x = "Capacity Multiplier",
       y = "LTU Share")+
  theme(legend.position = "bottom")


fairness_austrian_upper <-ggplot(f_long_austrian_upper, aes(x = capacity, y=mean_value, ymin = nonswiss_mean, ymax = swiss_mean, fill=policy, alpha =fairness,group=fairness)) +
  geom_ribbon() +
  ylim(.32,.485)+
  #geom_line(aes(y = mean_value), colour = 'black') +
  geom_line(aes(y = swiss_mean), colour = 'black') +
  geom_line(aes(y = nonswiss_mean), colour = 'black')+
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.5, "2_eo" = .25, "3_if"=.1),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "Separation", "3_if" = "Individual Fairness")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  labs(title = "Austrian Prioritization and Optimal Program",
       x = "Capacity Multiplier",
       y = "LTU Share")+
  theme(legend.position = "bottom")

fairness_belgian_lower <-ggplot(f_long_belgian_lower, aes(x = capacity, y=mean_value, ymin = nonswiss_mean, ymax = swiss_mean, fill=policy, alpha =fairness,group=fairness)) +
  geom_ribbon() +
  ylim(.32,.485)+
  #geom_line(aes(y = mean_value), colour = 'black') +
  geom_line(aes(y = swiss_mean), colour = 'black') +
  geom_line(aes(y = nonswiss_mean), colour = 'black')+
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.5, "2_eo" = .25, "3_if"=.1),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "Separation", "3_if" = "Individual Fairness")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  labs(title = "Belgian Prioritization and Random Program",
       x = "Capacity Multiplier",
       y = "LTU Share")+
  theme(legend.position = "bottom")


fairness_austrian_lower <-ggplot(f_long_austrian_lower, aes(x = capacity, y=mean_value, ymin = nonswiss_mean, ymax = swiss_mean, fill=policy, alpha =fairness,group=fairness)) +
  geom_ribbon() +
  ylim(.32,.485)+
  #geom_line(aes(y = mean_value), colour = 'black') +
  geom_line(aes(y = swiss_mean), colour = 'black') +
  geom_line(aes(y = nonswiss_mean), colour = 'black')+
  scale_alpha_manual(values = c("0_log" = 1, "1_sp" = 0.5, "2_eo" = .25, "3_if"=.1),
                     labels = c("0_log" = "Logistic Regression", "1_sp" = "Independence", "2_eo" = "Separation", "3_if" = "Individual Fairness")) +
  scale_fill_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  scale_colour_manual(values = c("Belgian" = belgiumcolor, "Austrian" = austriacolor)) +
  labs(title = "Austrian Prioritization and Random Program",
       x = "Capacity Multiplier",
       y = "LTU Share")+
  theme(legend.position = "bottom")


legend<-g_legend(fairness_austrian_lower)

grid.arrange(arrangeGrob(fairness_belgian_upper + theme(legend.position="none"),
                         fairness_belgian_lower+ theme(legend.position="none"),
                         fairness_austrian_upper + theme(legend.position="none"),
                         fairness_austrian_lower + theme(legend.position="none"),
                         nrow=2),
             legend, nrow=2,heights=c(10, 1))


###################################################
#
#  LTU Gender Gap Analysis by (non)Swiss and (non)Married
#
###################################################
test_db <- db_sim
#NONSWISS NONMARRIED

gender_df <-test_db %>%
  filter(test_db$swiss==0 & test_db$married==0 ) %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

df <-test_db %>%
  filter(test_db$swiss==0 & test_db$married==0 ) %>%
  summarise(across(all_of(selected_vars), mean))

gender_long <- f_reshape_long(gender_df)
long <- f_reshape_long(df)

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"female_mean"  # Rename column name
gender_long$female_mean = gender_long$female * gender_long$mean_value

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"male_mean"  # Rename column name
gender_long$male_mean = (1-gender_long$female) * gender_long$mean_value

f_long <- filter(gender_long, female==1)
m_long <- filter(gender_long, female==0)

f_long$male_mean<-m_long$male_mean
f_long$mean_value<-long$mean_value

f_long_belgian_upper <- filter(f_long,policy=="Belgian" & assignment=="upper")
f_long_austrian_upper <- filter(f_long,policy=="Austrian" & assignment=="upper")
f_long_austrian_lower <- filter(f_long,policy=="Austrian" & assignment=="lower")
f_long_belgian_lower <- filter(f_long,policy=="Belgian" & assignment=="lower")

f_long_nofair_upper <- filter(f_long,fairness=="0_log" & assignment=="upper")
f_long_nofair_lower <- filter(f_long,fairness=="0_log" & assignment=="lower")

belgiumcolor="#1704E0"
austriacolor="#E03D2B"


ggplot(f_long_nofair_upper, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
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


#NONSWISS MARRIED

gender_df <-test_db %>%
  filter(test_db$swiss==0 & test_db$married==1 ) %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

df <-test_db %>%
  filter(test_db$swiss==0 & test_db$married==1 ) %>%
  summarise(across(all_of(selected_vars), mean))

gender_long <- f_reshape_long(gender_df)
long <- f_reshape_long(df)

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"female_mean"  # Rename column name
gender_long$female_mean = gender_long$female * gender_long$mean_value

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"male_mean"  # Rename column name
gender_long$male_mean = (1-gender_long$female) * gender_long$mean_value

f_long <- filter(gender_long, female==1)
m_long <- filter(gender_long, female==0)

f_long$male_mean<-m_long$male_mean
f_long$mean_value<-long$mean_value

f_long_belgian_upper <- filter(f_long,policy=="Belgian" & assignment=="upper")
f_long_austrian_upper <- filter(f_long,policy=="Austrian" & assignment=="upper")
f_long_austrian_lower <- filter(f_long,policy=="Austrian" & assignment=="lower")
f_long_belgian_lower <- filter(f_long,policy=="Belgian" & assignment=="lower")

f_long_nofair_upper <- filter(f_long,fairness=="0_log" & assignment=="upper")
f_long_nofair_lower <- filter(f_long,fairness=="0_log" & assignment=="lower")

belgiumcolor="#1704E0"
austriacolor="#E03D2B"


ggplot(f_long_nofair_upper, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
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



#SWISS MARRIED

gender_df <-test_db %>%
  filter(test_db$swiss==1 & test_db$married==1 ) %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

df <-test_db %>%
  filter(test_db$swiss==1 & test_db$married==1 ) %>%
  summarise(across(all_of(selected_vars), mean))

gender_long <- f_reshape_long(gender_df)
long <- f_reshape_long(df)

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"female_mean"  # Rename column name
gender_long$female_mean = gender_long$female * gender_long$mean_value

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"male_mean"  # Rename column name
gender_long$male_mean = (1-gender_long$female) * gender_long$mean_value

f_long <- filter(gender_long, female==1)
m_long <- filter(gender_long, female==0)

f_long$male_mean<-m_long$male_mean
f_long$mean_value<-long$mean_value

f_long_belgian_upper <- filter(f_long,policy=="Belgian" & assignment=="upper")
f_long_austrian_upper <- filter(f_long,policy=="Austrian" & assignment=="upper")
f_long_austrian_lower <- filter(f_long,policy=="Austrian" & assignment=="lower")
f_long_belgian_lower <- filter(f_long,policy=="Belgian" & assignment=="lower")

f_long_nofair_upper <- filter(f_long,fairness=="0_log" & assignment=="upper")
f_long_nofair_lower <- filter(f_long,fairness=="0_log" & assignment=="lower")

belgiumcolor="#1704E0"
austriacolor="#E03D2B"


ggplot(f_long_nofair_upper, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
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

#SWISS NONMARRIED

gender_df <-test_db %>%
  filter(test_db$swiss==1 & test_db$married==0 ) %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

df <-test_db %>%
  filter(test_db$swiss==1 & test_db$married==0 ) %>%
  summarise(across(all_of(selected_vars), mean))

gender_long <- f_reshape_long(gender_df)
long <- f_reshape_long(df)

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"female_mean"  # Rename column name
gender_long$female_mean = gender_long$female * gender_long$mean_value

gender_long[ , ncol(gender_long) + 1] <- 0                  # Create column for upper/lower/empirical assignment strategy
colnames(gender_long)[ncol(gender_long)] <-"male_mean"  # Rename column name
gender_long$male_mean = (1-gender_long$female) * gender_long$mean_value

f_long <- filter(gender_long, female==1)
m_long <- filter(gender_long, female==0)

f_long$male_mean<-m_long$male_mean
f_long$mean_value<-long$mean_value

f_long_belgian_upper <- filter(f_long,policy=="Belgian" & assignment=="upper")
f_long_austrian_upper <- filter(f_long,policy=="Austrian" & assignment=="upper")
f_long_austrian_lower <- filter(f_long,policy=="Austrian" & assignment=="lower")
f_long_belgian_lower <- filter(f_long,policy=="Belgian" & assignment=="lower")

f_long_nofair_upper <- filter(f_long,fairness=="0_log" & assignment=="upper")
f_long_nofair_lower <- filter(f_long,fairness=="0_log" & assignment=="lower")

belgiumcolor="#1704E0"
austriacolor="#E03D2B"


ggplot(f_long_nofair_upper, aes(x = capacity, y=mean_value, ymin = male_mean, colour=policy, ymax = female_mean, fill=policy,group=policy)) +
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



legend<-g_legend(unmarried_swiss)

grid.arrange(arrangeGrob(unmarried_nonswiss + theme(legend.position="none"),
                         married_nonswiss + theme(legend.position="none"),
                         unmarried_swiss + theme(legend.position="none"),
                         married_swiss + theme(legend.position="none"),
                         nrow=2),
             legend, nrow=2,heights=c(10, 1))


#grid.arrange(arrangeGrob(unmarried_nonswiss,married_nonswiss,ncol=2),arrangeGrob(unmarried_swiss,married_swiss,ncol=2),nrow=2)



###################################################
#
#  LTU Citizen Gap Analysis by (non)Female and (non)Married
#
###################################################


# Calculate female means for selected variables
means_female_df <- test_db %>%
  filter(test_db$female==1) %>%
  group_by(married,swiss) %>%
  summarise(across(all_of(selected_vars), mean))

# Calculate non-swiss means for selected variables
means_nonfemale_df <- test_db %>%
  filter(test_db$female==0) %>%
  group_by(married,swiss) %>%
  summarise(across(all_of(selected_vars), mean))

female_means_long <- means_female_df %>%
  tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                      names_to = "variable",
                      values_to = "mean_value") %>%
  mutate(factor_variable = factor(substring(variable, nchar(variable), nchar(variable))))

nonfemale_means_long <- means_nonfemale_df %>%
  tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                      names_to = "variable",
                      values_to = "mean_value") %>%
  mutate(factor_variable = factor(substring(variable, nchar(variable), nchar(variable))))

#Make Graphs for (un)Married Natives
female_means_long$assignment<- substr(female_means_long$variable,1,nchar(female_means_long$variable)-2 )
female_means_long_upper_married<-filter(female_means_long, married==1)
female_means_long_upper_married<-filter(female_means_long_upper_married, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

married_female<-ggplot(data=female_means_long_upper_married, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(swiss,assignment))) +
  geom_line(aes(linetype=as.factor(swiss)))+
  geom_point()+
  ylim(.31,.52) +
  theme(legend.position = "none")+
  labs(title = "Married female",
       x = "Capacity Multiplier",
       y = "LTU Share")


female_means_long$assignment<- substr(female_means_long$variable,1,nchar(female_means_long$variable)-2 )
female_means_long_upper_unmarried<-filter(female_means_long, married==0)
female_means_long_upper_unmarried<-filter(female_means_long_upper_unmarried, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

unmarried_female<-ggplot(data=female_means_long_upper_unmarried, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(swiss,assignment))) +
  geom_line(aes(linetype=as.factor(swiss)))+
  geom_point()+
  ylim(.31,.52)+
  theme(legend.position = "none")+
  labs(title = "unMarried female",
       x = "Capacity Multiplier",
       y = "LTU Share")


#Make Graphs for (un)Married non-Natives
nonfemale_means_long$assignment<- substr(nonfemale_means_long$variable,1,nchar(nonfemale_means_long$variable)-2 )
nonfemale_means_long_upper_married<-filter(nonfemale_means_long, married==1)
nonfemale_means_long_upper_married<-filter(nonfemale_means_long_upper_married, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

married_nonfemale<-ggplot(data=nonfemale_means_long_upper_married, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(swiss,assignment))) +
  geom_line(aes(linetype=as.factor(swiss)))+
  geom_point()+
  ylim(.31,.52)+
  theme(legend.position = "none")+
  labs(title = "Married non-female",
       x = "Capacity Multiplier",
       y = "LTU Share")


nonfemale_means_long_upper_unmarried<-filter(nonfemale_means_long, married==0)
nonfemale_means_long_upper_unmarried<-filter(nonfemale_means_long_upper_unmarried, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

unmarried_nonfemale<-ggplot(data=nonfemale_means_long_upper_unmarried, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(swiss,assignment))) +
  geom_line(aes(linetype=as.factor(swiss)))+
  geom_point()+
  ylim(.31,.52)+
  theme(legend.position = "bottom")+
  labs(title = "unMarried non-female",
       x = "Capacity Multiplier",
       y = "LTU Share")



legend<-g_legend(unmarried_nonfemale)

grid.arrange(arrangeGrob(unmarried_nonfemale + theme(legend.position="none"),
                         married_nonfemale + theme(legend.position="none"),
                         unmarried_female + theme(legend.position="none"),
                         married_female + theme(legend.position="none"),
                         nrow=2),
             legend, nrow=2,heights=c(10, 1))

grid.arrange(arrangeGrob(unmarried_nonfemale,married_nonfemale,ncol=2),arrangeGrob(unmarried_female,married_female,ncol=2),nrow=3)


#############################################################








