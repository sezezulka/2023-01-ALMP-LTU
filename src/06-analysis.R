library(gridExtra)

file="~/data/1203_ALMP_Sample_Simulations_Test.csv"
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

selected_vars <- grep("iapo_policy_", names(db), value = TRUE)

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

f_reshape_long <- function(df) {
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
  
  long_df[ , ncol(long_df) + 1] <- "emp"                  # Create column for upper/lower/empirical assignment strategy
  colnames(long_df)[ncol(long_df)] <-"assignment"  # Rename column name
  long_df[grep("upper",long_df$variable),]$assignment<-"upper"
  long_df[grep("lower",long_df$variable),]$assignment<-"lower"
  
  long_df[ , ncol(long_df) + 1] <- "Log Reg"                  # Create column for risk score
  colnames(long_df)[ncol(long_df)] <-"risk"  # Rename column name
  long_df[grep("risk_score_sp",long_df$variable),]$risk<-"SP"
  long_df[grep("risk_score_eo",long_df$variable),]$risk<-"EO"
  long_df[grep("risk_score_if",long_df$variable),]$risk<-"IF"
  
  long_df[ , ncol(long_df) + 1] <- "Unconstraint"
  colnames(long_df)[ncol(long_df)] <-"fairness"
  long_df[grep("risk_score_sp",long_df$variable),]$fairness<-"Fair"
  long_df[grep("risk_score_eo",long_df$variable),]$fairness<-"Fair"
  long_df[grep("risk_score_if",long_df$variable),]$fairness<-"Fair"
  
  
 
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
upper_welfare_long <- welfare_long %>% filter(welfare_long$assignment=="upper")
welfare_upper<-ggplot(data=upper_welfare_long, aes(x=capacity, y=mean_value, colour=policy, group=interaction(policy,ineq_aversion))) +
  geom_line(aes(linetype=as.factor(ineq_aversion)))+
  geom_point()+
  ylim(0,upperlim) +
  labs(title = "Total Welfare (Upper)",
       x = "Capacity Multiplier",
       y = "Welfare")

lower_welfare_long <- welfare_long %>% filter(welfare_long$assignment=="lower")

welfare_lower<-ggplot(data=lower_welfare_long, aes(x=capacity, y=mean_value, colour=policy, group=interaction(policy,ineq_aversion))) +
  geom_line(aes(linetype=as.factor(ineq_aversion)))+
  geom_point()+
  ylim(0,upperlim) +
  labs(title = "Total Welfare (Lower)",
       x = "Capacity Multiplier",
       y = "Welfare")

emp_welfare_long <- welfare_long %>% filter(welfare_long$assignment=="emp")

welfare_emp<-ggplot(data=emp_welfare_long, aes(x=capacity, y=mean_value, colour=policy, group=interaction(policy,ineq_aversion))) +
  geom_line(aes(linetype=as.factor(ineq_aversion)))+
  geom_point()+
  ylim(0,upperlim) +
  theme(legend.position = "bottom")+
  labs(title = "Total Welfare (Emp)",
       x = "Capacity Multiplier",
       y = "Welfare")



legend<-g_legend(welfare_emp)
grid.arrange(arrangeGrob(welfare_upper + theme(legend.position="none"),
                         welfare_lower + theme(legend.position="none"),
                         welfare_emp + theme(legend.position="none"),
                         nrow=1),
             legend, nrow=2,heights=c(10, 1))


####################################################
#
# LTU Gender Gap Analysis
#
##################################################
gender_df <-db %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

gender_long <- f_reshape_long(gender_df)

#, group=interaction(female,policy,risk))


gender_long_upper <- filter(gender_long, assignment=="upper")
gendergap_upper <- ggplot(data=gender_long_upper, aes(x=capacity, y=mean_value, colour=policy, shape=risk, alpha=fairness, group=interaction(female, risk, policy))) +
  geom_line(aes(linetype=as.factor(risk)), size=1)+
  geom_point(size = 3, position = position_dodge(width = 0.1))+
  ylim(.33,.44) +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme(legend.position = "none")+
  labs(title = "LTU Gender Gap (Assignment=Upper)",
       x = "Capacity Multiplier",
       y = "LTU Share")


gender_long_lower <- filter(gender_long, assignment=="lower")
gendergap_lower <- ggplot(data=gender_long_lower, aes(x=capacity, y=mean_value, colour=policy, shape=risk, alpha=fairness, group=interaction(female, risk, policy))) +
  geom_line(aes(linetype=as.factor(risk)), size=1)+
  geom_point(size = 3, position = position_dodge(width = 0.1))+
  ylim(.33,.44) +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme(legend.position = "bottom")+
  labs(title = "LTU Gender Gap (Assignment=Lower)",
       x = "Capacity Multiplier",
       y = "LTU Share")


legend<-g_legend(gendergap_lower)

grid.arrange(arrangeGrob(gendergap_upper + theme(legend.position="none"),
                               gendergap_lower + theme(legend.position="none"),
                               # gendergap_emp + theme(legend.position="none"),
                               nrow=1),
                   legend, nrow=2,heights=c(10, 1))


####################################################
#
# LTU Citizen Gap Analysis
#
##################################################

citizen_df <-test_db %>%
  group_by(swiss) %>%
  summarise(across(all_of(selected_vars), mean))

citizen_long <- reshape_long(citizen_df)

citizen_long_upper <- filter(citizen_long, assignment=="upper")
citizengap_upper <- ggplot(data=citizen_long_upper, aes(x=capacity, y=mean_value, colour=policy, group=interaction(swiss,policy,assignment))) +
  geom_line(aes(linetype=as.factor(swiss)))+
  geom_point()+
  ylim(.31,.52) +
  theme(legend.position = "none")+
  labs(title = "LTU Citizen Gap (Assignment=Upper)",
       x = "Capacity Multiplier",
       y = "LTU Share")


citizen_long_lower <- filter(citizen_long, assignment=="lower")
citizengap_lower <- ggplot(data=citizen_long_lower, aes(x=capacity, y=mean_value, colour=policy, group=interaction(swiss,policy,assignment))) +
  geom_line(aes(linetype=as.factor(swiss)))+
  geom_point()+
  ylim(.31,.52) +
  theme(legend.position = "none")+
  labs(title = "LTU Citizen Gap (Assignment=Lower)",
       x = "Capacity Multiplier",
       y = "LTU Share")

citizen_long_emp <- filter(citizen_long, assignment=="emp")
citizengap_emp <- ggplot(data=citizen_long_emp, aes(x=capacity, y=mean_value, colour=policy, group=interaction(swiss,policy,assignment))) +
  geom_line(aes(linetype=as.factor(swiss)))+
  geom_point()+
  ylim(.31,.52) +
  theme(legend.position = "bottom")+
  labs(title = "LTU Citizen Gap (Assignment=Empirical)",
       x = "Capacity Multiplier",
       y = "LTU Share")

legend<-g_legend(citizengap_emp)

grid.arrange(arrangeGrob(citizengap_upper + theme(legend.position="none"),
                         citizengap_lower + theme(legend.position="none"),
                         citizengap_emp + theme(legend.position="none"),
                         nrow=1),
             legend, nrow=2,heights=c(10, 1))

###################################################
#
#  LTU Gender Gap Analysis by (non)Swiss and (non)Married
#
###################################################


# Calculate swiss means for selected variables
means_swiss_df <- test_db %>%
  filter(test_db$swiss==1) %>%
  group_by(married,female) %>%
  summarise(across(all_of(selected_vars), mean))

# Calculate non-swiss means for selected variables
means_nonswiss_df <- test_db %>%
  filter(test_db$swiss==0) %>%
  group_by(married,female) %>%
  summarise(across(all_of(selected_vars), mean))

swiss_means_long <- means_swiss_df %>%
  tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                      names_to = "variable",
                      values_to = "mean_value") %>%
  mutate(factor_variable = factor(substring(variable, nchar(variable), nchar(variable))))

nonswiss_means_long <- means_nonswiss_df %>%
  tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                      names_to = "variable",
                      values_to = "mean_value") %>%
  mutate(factor_variable = factor(substring(variable, nchar(variable), nchar(variable))))

#Make Graphs for (un)Married Natives
swiss_means_long$assignment<- substr(swiss_means_long$variable,1,nchar(swiss_means_long$variable)-2 )
swiss_means_long_upper_married<-filter(swiss_means_long, married==1)
swiss_means_long_upper_married<-filter(swiss_means_long_upper_married, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

married_swiss<-ggplot(data=swiss_means_long_upper_married, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(female,assignment))) +
  geom_line(aes(linetype=as.factor(female)))+
  geom_point()+
  ylim(.31,.52) +
  theme(legend.position = "none")+
  labs(title = "Married Swiss",
       x = "Capacity Multiplier",
       y = "LTU Share")


swiss_means_long$assignment<- substr(swiss_means_long$variable,1,nchar(swiss_means_long$variable)-2 )
swiss_means_long_upper_unmarried<-filter(swiss_means_long, married==0)
swiss_means_long_upper_unmarried<-filter(swiss_means_long_upper_unmarried, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

unmarried_swiss<-ggplot(data=swiss_means_long_upper_unmarried, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(female,assignment))) +
  geom_line(aes(linetype=as.factor(female)))+
  geom_point()+
  ylim(.31,.52)+
  theme(legend.position = "none")+
  labs(title = "unMarried Swiss",
       x = "Capacity Multiplier",
       y = "LTU Share")


#Make Graphs for (un)Married non-Natives
nonswiss_means_long$assignment<- substr(nonswiss_means_long$variable,1,nchar(nonswiss_means_long$variable)-2 )
nonswiss_means_long_upper_married<-filter(nonswiss_means_long, married==1)
nonswiss_means_long_upper_married<-filter(nonswiss_means_long_upper_married, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

married_nonswiss<-ggplot(data=nonswiss_means_long_upper_married, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(female,assignment))) +
  geom_line(aes(linetype=as.factor(female)))+
  geom_point()+
  ylim(.31,.52)+
  theme(legend.position = "none")+
  labs(title = "Married non-Swiss",
       x = "Capacity Multiplier",
       y = "LTU Share")


nonswiss_means_long_upper_unmarried<-filter(nonswiss_means_long, married==0)
nonswiss_means_long_upper_unmarried<-filter(nonswiss_means_long_upper_unmarried, assignment %in% c("iapo_policy_Belgian_rlog_upper","iapo_policy_Austrian_rlog_upper"))

unmarried_nonswiss<-ggplot(data=nonswiss_means_long_upper_unmarried, aes(x=factor_variable, y=mean_value, colour=assignment, group=interaction(female,assignment))) +
  geom_line(aes(linetype=as.factor(female)))+
  geom_point()+
  ylim(.31,.52)+
  theme(legend.position = "bottom")+
  labs(title = "unMarried non-Swiss",
       x = "Capacity Multiplier",
       y = "LTU Share")


legend<-g_legend(unmarried_nonswiss)

grid.arrange(arrangeGrob(unmarried_nonswiss + theme(legend.position="none"),
                         married_nonswiss + theme(legend.position="none"),
                         unmarried_swiss + theme(legend.position="none"),
                         married_swiss + theme(legend.position="none"),
                         nrow=2),
             legend, nrow=2,heights=c(10, 1))


grid.arrange(arrangeGrob(unmarried_nonswiss,married_nonswiss,ncol=2),arrangeGrob(unmarried_swiss,married_swiss,ncol=2),nrow=2)



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








