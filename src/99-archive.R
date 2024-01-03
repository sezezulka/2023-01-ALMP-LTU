# ---------------------------------------------------------------------------- #
# 99-Archive
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
# 02
# ---------------------------------------------------------------------------- #

# TODO differen to using 'nuisance_dss_e' and 'nuisance_dss_m'?

# estimate propensity scores
em = nuisance_e(list(forest),
                wm, 
                x, 
                cfm, 
                cv=no_cv, 
                path=NULL, 
                quiet=FALSE)
# summary(em)

# estimate outcomes
mm = nuisance_m(list(forest),
                y_exit12, 
                wm, 
                x, 
                cfm, 
                cv=no_cv, 
                path=NULL, 
                quiet=FALSE, 
                weights=FALSE)
# summary(mm)

# estimate DR scores
dr_scores <- f_dr_scores(wm, 
                         em, 
                         mm, 
                         y_exit12)
ipw <- dr_scores$ipw
gamma <- dr_scores$gamma

# estimate smoothed IAPOs
theta <- f_smoothed_iapos(wm, 
                          gamma, 
                          cv=no_cv, 
                          seed=seed, 
                          bound_outcomes=TRUE, 
                          up=1, 
                          low=0)



# ---------------------------------------------------------------------------- #
# analyse 

# TODO why not plot against gamma? what does this tell?
# gamma and mm are perfectly aligned! regression works...
# for theta, mm:
# 6, 7 are negatively correlated!
# 1, 4, 5 seem ok
# 2, 3 fan out for higher values 

for (i in 1:7) {
  x_label <- paste0("theta_", i)
  y_label <- paste0("mm_", i)
  title_plot <- paste0("IAPOs vs. Nuisance Outcome, program ", i)
  title_hist <- paste0("Histogram IAPO scores, program ", i)
  
  plot(theta[,i], mm[,i],
       xlab=x_label, 
       ylab=y_label,
       main=title_plot)
  
  hist(theta[,i], breaks=100, 
       xlim=c(0,1),
       main=title_hist,
       xlab=x_label)
  # TODO not nice, plots on top of each other!
}



# ---------------------------------------------------------------------------- #

# ggplot(as.data.frame(theta), aes(x=V1)) +
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) +
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_np.png", width = 6, height = 4, units = 'in')
# 
# ggplot(as.data.frame(theta_original), aes(x=V2)) + 
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) + 
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_pc.png", width = 6, height = 4, units = 'in')
# 
# ggplot(as.data.frame(theta_original), aes(x=V3)) + 
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) + 
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_lg.png", width = 6, height = 4, units = 'in')
# 
# ggplot(as.data.frame(theta_original), aes(x=V4)) + 
#   geom_histogram(color="black", fill="#ffffff", bins = 100) +
#   theme_bw() + xlab("risk") + ylab("") + theme(text = element_text(size=16)) +
#   geom_vline(xintercept = 0, color = "#ff9933", lwd = .75) + 
#   geom_vline(xintercept = 1, color = "#ff9933", lwd = .75)
# 
# ggsave(file = "plots/appendix/iapo_vc.png", width = 6, height = 4, units = 'in')


# ---------------------------------------------------------------------------- #
# add results to data

# potential outcomes
db$iapo_no_program  <- theta[,1]
db$iapo_vocational  <- theta[,2]
db$iapo_computer  <- theta[,3] # not in Knaus
db$iapo_language  <- theta[,4]
db$iapo_job_search    <- theta[,5]
db$iapo_employment <- theta[,6]
db$iapo_personality <- theta[,7] # not in Knaus

# IATEs against no_program
db$iate_vocational  <- theta[,2] - theta[,1]
db$iate_computer  <- theta[,3] - theta[,1]
db$iate_language    <- theta[,4] - theta[,1]
db$iate_job_search  <- theta[,5] - theta[,1]
db$iate_employment    <- theta[,6] - theta[,1]
db$iate_personality <- theta[,7] - theta[,1]

# propensity scores
db$em_no_program <- em[,1]
db$em_vocational <- em[,2]
db$em_computer <- em[,3]
db$em_language <- em[,4]
db$em_job_search <- em[,5]
db$em_employment <- em[,6]
db$em_personality <- em[,7]


# TODO estimate IATEs from IAPO scores and compare with ndr_learner results as check! 


# ---------------------------------------------------------------------------- #
# potential outcomes and treatment effects
## Run the main function that outputs nuisance parameters, APO and ATE



# comparision
summary(cDML$APO)
plot(cDML$APO)

print(colMeans(gamma))
print(colMeans(theta))

# 
summary(cDML$ATE)

## (N)DR-learner
ndr = ndr_learner(y_exit12,
                  w,
                  x,
                  ml_w = list(forest),
                  ml_y = list(forest),
                  ml_tau = list(forest),
                  quiet=FALSE,
                  compare_all = FALSE)

# # get APO estimates from one fold?
# # ndr$list[[1]]$APO$m_mat
# 
# ndr_cates <- ndr$cates[1:6, , 2]
# 
# # bound the outcome
# for (i in 1:ncol(wm)) {
#   ndr$cates[i,,2][ndr$cates[i,,2] > 1] <- 1
#   ndr$cates[i,,2][ndr$cates[i,,2] < 0] <- 0
# }
# 
# # Plot the results
# label_w = levels(w)
# df_box = NULL
# for (i in 1:6) {
#   df = data.frame("DRL" = ndr$cates[i,,1], "NDRL" = ndr$cates[i,,2])
#   df = gather(df)
#   df = cbind(label_w[i+1],df)
#   colnames(df)[1] = "label"
#   df_box = rbind(df_box,df)
# }
# ggplot(data=df_box) + geom_boxplot( aes(x=factor(label,label_w[-1]),y=value,fill=key)) +
#   theme_bw() + theme(axis.title.x=element_blank(),legend.title = element_blank()) +
#   ylab("Individualized average treatment effect") + geom_hline(yintercept = 0) + geom_hline(yintercept = -1,linetype="dashed") +
#   geom_hline(yintercept = 1,linetype="dashed") +
#   theme(text=element_text(family="serif",size = 16, colour="black"),axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   scale_fill_grey(start = 0.9,end=0.4)



# ---------------------------------------------------------------------------- #
# 04
# ---------------------------------------------------------------------------- #

# if (data[[most_effective_program]][i] <= 0) {
#   # test if program is at least as effective as baseline ("no program")
#   
#   return(most_effective_program)
# } else {
#   
#   most_effective_program <- 'iate_no_program'
#   return(most_effective_program)
# }

f_effective_program <- function(data, temp_program_names, effect_type) {
  
  # ERROR! needs to select most effective program from available ones!
  
  #############################################################
  #
  # select most effective treatment from available programs
  #
  # data :                dataset
  # temp_program_names :  list, program names
  # i :                   row number
  # effect_type :         string, {min, max} 
  #
  #############################################################
  
  if (effect_type == "min") {
    # effect that minimises target variable
    
    data$most_effective_program <- apply(data[temp_program_names], 1, function(row) {
      selected_columns[which.min(row)]
    }) 
  } else if (effect_type == "max") {
    
    data$most_effective_program <- apply(data[temp_program_names], 1, function(row) {
      selected_columns[max.col(row)]
    })
  }
}


# number of place to be allocated
db %>% 
  group_by(treatment6) %>%
  summarise(N = n())

colSums(wm)

# 1 computer      905
# 2 employment    611
# 3 job search  11610
# 4 language     1504
# 5 no program  47631
# 6 personality  1177
# 7 vocational    858

# list with ALMPs and capacities
programs_list <- list(iapo_no_program = 47631,
                      iapo_vocational = 858,
                      iapo_computer = 905,
                      iapo_language = 1504,
                      iapo_job_search = 11610,
                      iapo_employment = 611,
                      iapo_personality = 1177)


# Greedy allocation
for (i in 1:nrow(db)) {
  
  # test for capacity
  if (all(programs_list > 0)) {
    
    # select available program names, exclude "no program" (IATE basecase) 
    program_names_temp <- names(programs_list) %>%
      discard(., . %in% "iate_no_program")
    
    if (!any(nzchar(program_names_temp))) {
      # breaks loop if only "no program" is left
      # TODO does not work yet!
      break
    }
    
    # select most effective treatment from available programs
    most_effective_program <- db %>%
      slice(i) %>%
      select(all_of(program_names_temp)) %>%
      # TODO decide whether min. or max.col  
      #mutate(best_iate = names(.)[max.col(.)]) %>%
      mutate(best_iate = names(.)[which.min(.)]) %>%
      select(best_iate) %>%
      pull()
    
    # Assign the individual to the program
    if (db[[most_effective_program]][i] <= 0) {
      
      # Assignment of programme with IATE <= 0
      db$p11_almp[i] <- most_effective_program
      
      # Update the program capacity
      programs_list[[most_effective_program]] <- programs_list[[most_effective_program]] - 1
      
    } else if ("iate_no_program" %in% names(programs_list)) {
      # else, assign "no program"
      db$p11_almp[i] <- 'iate_no_program'
      
      # Update the program capacity
      programs_list$iate_no_program <- programs_list$iate_no_program - 1
    }
    
    # test if any program is at zero capacity: keep only those with capacity
    if (any(programs_list <= 0)) {
      programs_list <- keep(programs_list, ~ . > 0)
      
      # TODO weird NA saves me here,  
      # it's because list gets empty before assignment can happen... 
    }
    
    # print no of iteration and available programs
    if (i %% 1e3 == 0) {
      cat('Iteration:', i, '\n')
      print(programs_list)
    }
  } 
} 

# assign "no program" to the remaining individuals
db$p11_almp <- ifelse(is.na(db$p11_almp), "iate_no_program", db$p11_almp)


# TODO translate program assignment into pseudo-outcome by selecting the relevant IAPOs
# take program from p11_almp, cut "iate_" part, add "iapo_" part and use that to select outcome variable

db$p11_almp <- sub("iate_", "iapo_", db$p11_almp)
db$p11_y_exit12 <- NA

for (i in 1:nrow(db)) {
  # TODO very inefficient! map-function!
  
  p11_program <- db$p11_almp[i]
  db$p11_y_exit12[i] <- db[[p11_program]][i]
}


# evaluation
summary(db$p11_y_exit12)

mean(db$y_exit12[db$female==0])
mean(db$y_exit12[db$female==1])

mean(db$p11_y_exit12[db$female==0])
mean(db$p11_y_exit12[db$female==1])

# ---------------------------------------------------------------------------- #
# 1.2 lower bound: assign random (until capacity is reached) 
# high risk people have larger option pool

# ---------------------------------------------------------------------------- #
# 1.3 empirical assignment: assign per highest propensity
# TODO exclude no program until capacity for other programs is reached?
# TODO how to deal with negative effects? lock in effects?


# ---------------------------------------------------------------------------- #
# Policy 2: Efficiency (Austria)
# ---------------------------------------------------------------------------- #
# rank by risk score, focus on group between .7 and .3 first

# list with ALMPs and capacities
programs_list <- list(iate_computer = 905, 
                      iate_employment = 611,
                      iate_job_search = 11610,
                      iate_language = 1504,
                      iate_no_program = 47631,
                      iate_personality = 1177,
                      iate_vocational = 858)
# db$p21_almp  <- NA

# first, get only those with middle risk scores
db_austrian_middle <- db %>% 
  filter(risk_log >= 0.3, risk_log <= 0.7) %>%
  arrange(desc(risk_log)) %>%
  mutate(p21_almp = NA)

# second, Greedy allocation
for (i in 1:nrow(db_austrian_middle)) {
  
  # test for capacity
  if (all(programs_list > 0)) {
    
    # select available program names, exclude "no program" (IATE basecase) 
    program_names_temp <- names(programs_list) %>%
      discard(., . %in% "iate_no_program")
    
    if (!any(nzchar(program_names_temp))) {
      # breaks loop if only "no program" is left
      # TODO does not work yet!
      break
    }
    
    # select most effective treatment from available programs
    most_effective_program <- db_austrian_middle %>%
      slice(i) %>%
      select(all_of(program_names_temp)) %>%
      # TODO decide whether min. or max.col  
      #mutate(best_iate = names(.)[max.col(.)]) %>%
      mutate(best_iate = names(.)[which.min(.)]) %>%
      select(best_iate) %>%
      pull()
    
    # Assign the individual to the program
    if (db_austrian_middle[[most_effective_program]][i] <= 0) {
      
      # Assignment of programme with IATE <= 0
      db_austrian_middle$p21_almp[i] <- most_effective_program
      
      # Update the program capacity
      programs_list[[most_effective_program]] <- programs_list[[most_effective_program]] - 1
      
    } else if ("iate_no_program" %in% names(programs_list)) {
      # else, assign "no program"
      db_austrian_middle$p21_almp[i] <- 'iate_no_program'
      
      # Update the program capacity
      programs_list$iate_no_program <- programs_list$iate_no_program - 1
    }
    
    # test if any program is at zero capacity: keep only those with capacity
    if (any(programs_list <= 0)) {
      programs_list <- keep(programs_list, ~ . > 0)
      
      # TODO weird NA saves me here,  
      # it's because list gets empty before assignment can happen... 
    }
    
    # print no of iteration and available programs
    if (i %% 1e3 == 0) {
      cat('Iteration:', i, '\n')
      print(programs_list)
    }
  } 
} 

# merge back into dataset 
# TODO really necessary to split data?

db_austrian_middle <- db_austrian_middle %>%
  select(ID, p21_almp)

db_test <- db %>%
  left_join(db_austrian_middle, by="ID")

# assign "no program" to the remaining individuals
db_test$p21_almp <- ifelse(is.na(db_test$p21_almp), "iate_no_program", db_test$p21_almp)

# estimate potential outcome under p21
db_test$p21_almp <- sub("iate_", "iapo_", db_test$p21_almp)
db_test$p21_y_exit12 <- NA

for (i in 1:nrow(db_test)) {
  # TODO very inefficient! map-function!
  
  p21_program <- db_test$p21_almp[i]
  db_test$p21_y_exit12[i] <- db_test[[p21_program]][i]
}


# evaluation
summary(db_test$p21_y_exit12)

mean(db_test$y_exit12[db_test$female==0])
mean(db_test$y_exit12[db_test$female==1])

mean(db_test$p21_y_exit12[db_test$female==0])
mean(db_test$p21_y_exit12[db_test$female==1])


# %>%
#   #mutate(max_iate_id = max.col(across(starts_with('iate_')))) %>%
#   
#   mutate(sec_iate = pmap_dbl(across(starts_with('iate_')), 
#                              ~sort(., decreasing=T)[2])
#          ) %>%
#   select(sec_iate)
# 
#   rowwise() %>%
#   mutate(sec_iate = sort(c_across(starts_with('iate_')), decreasing=TRUE)[2])
# 
#  # Function to get the column name with the second-largest value
#  get_second_largest_column <- function(row) {
#    sorted_indices <- order(row, decreasing = TRUE)
#    return(names(row)[sorted_indices[2]])
#  }
#   
# db_test <- db %>%
#   select(starts_with('iate_')) %>%
#   rowwise() %>%
#   mutate(sec_iate = order(., decreasing = TRUE)[2])
# 
# 
# 
# counts <- list(iate_computer = 0,
#                iate_employment = 0,
#                iate_job_search = 0,
#                iate_language = 0,
#                iate_no_program = 0,
#                iate_personality = 0,
#                iate_vocational = 0)
# count_max <- list(iate_computer = 905,
#                   iate_employment = 611,
#                   iate_job_search = 11610,
#                   iate_language = 1504,
#                   iate_no_program = 47631,
#                   iate_personality = 1177,
#                   iate_vocational = 858)
# 
# db$p11_allocation <- NA
# # count_computer <- 0
# # count_employment <- 0
# # count_job_search <- 0
# # count_language <- 0 
# # count_no_program <- 0
# # count_personality <- 0
# # count_vocational <- 0
# 
# 
# for (i in 1:nrow(db)) {
#   
#   # case one, all courses still available
#   if (!any(mapply(identical, counts, count_max))) {
#     
#     db$p11_allocation[i] <- db$iate_max[i]
#     counts[[db$p11_allocation[i]]] <- counts[[db$p11_allocation[i]]] + 1
#     
#   } else {
#     
#     
#     
#   } 
#   
# }

# programs <- data.frame(ProgramID = c("iate_computer", "iate_employment", "iate_job_search",
#                                      "iate_language", "iate_no_program", "iate_personality", 
#                                      "iate_vocational"), 
#                        Capacity = c(905, 611, 11610, 1504, 47631, 1177, 858))

# # sort db by risk score in decreasing order
# db <- db %>% 
#   arrange(desc(risk_log))
# 
# programs_list <- list(iate_computer = 905, 
#                  iate_employment = 611,
#                  iate_job_search = 11610,
#                  iate_language = 1504,
#                  iate_no_program = 47631,
#                  iate_personality = 1177,
#                  iate_vocational = 858)
# db$AssignedProgram  <- NA
# 
# # program_names <- programs$ProgramID %>%
# #   discard(., . %in% "iate_no_program")
# 
# # Greedy allocation
# for (i in 1:nrow(db)) {
#   # max_effect_program <- db$max_iate[i]
# 
#   # test for capacity
#   if (all(programs_list >= 0)) {
#   #if (any programs$Capacity >= 0 | all(is.na(programs$Capacity))) {
#   #if (programs$Capacity[programs$ProgramID == max_effect_program] > 0 | is.na(programs$Capacity[programs$ProgramID == max_effect_program])) {
#     
#     # select available programs, 
#     program_names_temp <- names(programs_list) %>%
#       discard(., . %in% "iate_no_program")
#     
#     # select most effective treatment from available programs
#     most_effective_program <- db %>%
#       slice(i) %>%
#       select(all_of(program_names_temp)) %>%
#       mutate(max_iate = names(.)[max.col(.)]) %>%
#       select(max_iate) %>%
#       pull()
#     
#     # Assign the individual to the program
#     db$AssignedProgram[i] <- most_effective_program
#     
#     # Update the program capacity
#     programs_list[[most_effective_program]] <- programs_list[[most_effective_program]] - 1
#     # programs$Capacity[programs$ProgramID == max_effect_program] <- programs$Capacity[programs$ProgramID == max_effect_program] - 1
#   
#     if (any(programs_list <= 0)) {
#       programs_list <- kepp(programs_list, ~ . > 0)
#     }
#     
#     if (i %% 1000 == 0) {
#       cat('Iteration: ', i, '\n')
#       print(programs_list)
#     }
#     
#   } else {
#     # assign no_program to the remaining individuals
#     db$AssignedProgram[i] <- "iate_no_program"
#   }
# }  

#   else {
#     # entries_to_remove <- programs$ProgramID[programs$Capacity==0]
#     # programs <- programs %>% modify_at(., programs$ProgramID %in% entries_to_remove)
#     
#     # delete program with zero capacity
#     programs_list <- discard(programs_list, ~ . == 0)
#   } 
#   if (all(programs_list >=0)) {
#     
#     # select available programs
#     program_names_temp <- list(programs_list) 
#     
#     sec_effect_program <- db %>%
#       slice(i) %>%
#       select(all_of(program_names_temp)) %>%
#       #mutate(sec_iate = names(across(starts_with('iate_')))[max.col(across(starts_with('iate_')))]) %>%
#       mutate(sec_iate = names(.)[max.col(.)]) %>%
#       select(sec_iate)
#     
#     # Assign the individual to the program
#     db$AssignedProgram[i] <- sec_effect_program
#     
#     # Update the program capacity
#     programs$Capacity[programs$ProgramID == sec_effect_program] <- programs$Capacity[programs$ProgramID == sec_effect_program] - 1
#   }
# }



# ---------------------------------------------------------------------------- #
# Polcy 2: Efficiency (Austria)


