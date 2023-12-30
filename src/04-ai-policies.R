# ---------------------------------------------------------------------------- #
# algorithmically-informed policies
# ---------------------------------------------------------------------------- #

library(tidyverse)


# ---------------------------------------------------------------------------- #

# TODO change if necessary 
setwd("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")
seed = 12345

# ---------------------------------------------------------------------------- #

f_alg_policy <- function(data, policy, risk_score, effect_type, assignment_type, new_outcome_name) {
  
  #############################################################
  #
  # performs algorithmically informed policy for allocation
  # into ALMPs
  #
  # data :              dataset, including treatment variable ("treatment6"),
  #                     estimated IAPOs, propensity scores, ...
  # policy :            string, {Beglian, Austrian}
  # risk_score :        string, name of risk score to be used
  # effect_type :       string, {min, max} 
  # assignment_type :   string, {upper, emp, lower}
  # new_outcome_name :  string, name of new variable
  #
  #############################################################
  
  # get program capacities
  program_capacities <- f_get_capacities(data, treatment6)
  
  # create new outcome variable
  data <- f_create_outcome_var(data, new_outcome_name)
  
  # sort data according to policy by risk_score
  data <- f_risk_2_policy(data, policy, risk_score)
  
  # greedy allocation
  data <- f_greedy_allocation(data, program_capacities, effect_type, assignment_type, new_outcome_name)
  
  # add potential outcomes for program allocation
  data <- f_program_2_iapo(data, new_outcome_name)
  
  return(data)
}

f_get_capacities <- function(data, name_treatment_var) {
  
  #############################################################
  # 
  # Get program capacities.
  #
  # data :                dataset
  # name_treatment_var :  name of treatment variable
  #
  # Returns list.
  #
  #############################################################
  
  # program_capacities <- colSums(wm)
  # table(data$treatment6)
  
  program_capacities <- data %>% 
    group_by({{name_treatment_var}}) %>%
    summarise(N = n())
  
  list_programs <- as.list(setNames(program_capacities$N, program_capacities %>% pull({{ name_treatment_var }})))
  
  # change names in list 
  # TODO fix requirement 
  new_names <- paste0("iapo_", gsub(" ", "_", names(list_programs)))
  names(list_programs) <- new_names
  
  return(list_programs)
  
}

f_create_outcome_var <- function(data, new_outcome_name) {
  
  #############################################################
  #
  # Creates new outcome variable for algorithmically informed policy
  #
  # data :              dataset
  # new_outcome_name :  string, name of new variable
  #
  #############################################################
  
  if (!is.character(new_outcome_name)) {
    stop("Error: Name for new outcome variable must be given as string.")
  }
  
  data[[new_outcome_name]] <- NA
  
  # data <- data%>%
  #   mutate( {{new_outcome_name}} := NA_character_)
  
  return(data)
}

f_risk_2_policy <- function(data, policy, risk_score) {
  
  #############################################################
  #
  # sort dataset by risk score according to given policy
  #
  # data :        dataset
  # policy :      string, {Belgian, Austrian}
  # risk_score :  string, name of risk score to be used
  #
  #############################################################
  
  if (policy == "Belgian") {
    
    # sort by risk_score in descending order
    data <- data %>% 
      arrange(desc(!!sym(risk_score)))
    
    return(data)
    
  } else if (policy == "Austrian") {
    
    # first select individuals with middle risk_scores, then rest
    data_middle <- data %>% 
      arrange(desc( !!sym(risk_score) )) %>%
      filter( !!sym(risk_score)  <= 0.7,  !!sym(risk_score)  >= 0.3)
    
    data_rest <- data %>%
      arrange(desc( !!sym(risk_score) )) %>%
      filter( !!sym(risk_score)  > 0.7 |  !!sym(risk_score)  < 0.3) %>%
      # random order across non-selected individuals
      sample_n(nrow(.))
    
    data <- rbind(data_middle, data_rest)
    
    return(data)
    
  } else {
    
    stop("Error: Policy must be either \"Belgian\" or \"Austrian\".")
  }
}

f_find_effective_program_i <- function(data, temp_program_names, effect_type, i) {
  
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
    
    most_effective_program <- data %>%
      slice(i) %>%
      select(all_of(temp_program_names)) %>%
      mutate(best_program = names(.)[which.min(.)]) %>%
      select(best_program) %>%
      pull()
    
    # if (data[[most_effective_program]][i] <= 0) {
    #   # test if program is at least as effective as baseline ("no program")
    #   
    #   return(most_effective_program)
    # } else {
    #   
    #   most_effective_program <- 'iate_no_program'
    #   return(most_effective_program)
    # }
    return(most_effective_program)
    
  } else if (effect_type == "max") {
    # effect that maximises target variable
    
    most_effective_program <- data %>%
      slice(i) %>%
      select(all_of(temp_program_names)) %>%
      mutate(best_program = names(.)[max.col(.)]) %>%
      select(best_program) %>%
      pull()
    
    # if (data[[most_effective_program]][i] >= 0) {
    #   # test if program is at least as effective as baseline ("no program")
    #   
    #   return(most_effective_program)
    # } else {
    #   
    #   most_effective_program <- 'iate_no_program'
    #   return(most_effective_program)
    # }
    return(most_effective_program)
    
  } else {

    stop("Error: Effect type is miss-specified. Must be max or min.")
  }
}

f_find_propensity_program_i <- function() {
 
  return(assigned_program_name) 
}

f_find_random_program_i <- function() {
 
  
  return(assigned_program_name) 
}

f_temp_program_names <- function(list_programs) {
  
  #############################################################
  #
  # select available program names and exclude "no program"
  #
  # list_programs : list of available ALMPs and capacities
  #
  #############################################################
  
  temp_program_names <- names(list_programs)
  
  return(temp_program_names)
  
}

f_update_capacities <- function(list_programs, assigned_program_name) {

  #############################################################
  #
  # Update program capacities after assignment.
  #
  # list_programs :         list, available ALMPs and capacities 
  # assigned_program_name : string, name of assigned program
  #
  #############################################################
  
  # update program capacity 
  list_programs[[assigned_program_name]] <- list_programs[[assigned_program_name]] - 1
  
  # test if any program is at zero capacity, keep those with capacity
  if (any(list_programs <= 0)) {
    list_programs <- keep(list_programs, ~ . > 0)
  }
  
  return(list_programs)
  
}

f_greedy_allocation <- function(data, list_programs, effect_type, assignment_type, new_outcome_name) {
  
  #############################################################
  #
  # performs greedy allocation of individuals into ALMPs
  # according to capacity
  #
  # data :              dataset
  # list_programs :     list of available ALMPs and capacities 
  # effect_type :       string, {min, max} 
  # assignment_type :   string, {upper, emp, lower}
  # new_outcome_name :  string, name of new variable
  #
  #############################################################
  
  n <- nrow(data)
  
  for (i in 1:n) {
    
    if (all(list_programs > 0)) {
      # test availability of capacities
      
      # select available program names 
      temp_program_names <- f_temp_program_names(list_programs)
      
      if (!any(nzchar(temp_program_names))) {
        # test if any non-zero character (= only "no program" left)
        warning("Warning: Some program name has zero-length character.")
      }
      
      # select program name
      if (assignment_type == "upper") {
        # upper bound, assign most effective available program
        
        assigned_program_name <- f_find_effective_program_i(data, temp_program_names, effect_type, i)
        
      } else if (assignment_type == "lower") {
        # lower bound, assign random available program
        
        assigned_program_name <- f_find_random_program_i()
        
      } else if (assignment_type == "emp") {
        # empirical strategy, assign program by propensity score
        
        assigned_program_name <- f_find_propensity_program_i()
          
      } else {
        
        stop("Error: Assignment type must be \"upper\", \"lower\", or \"emp\".")
      }
      
      # assign program
      data[[new_outcome_name]][i] <- assigned_program_name
      
      # update program capacities
      list_programs <- f_update_capacities(list_programs, assigned_program_name)
    } else {
      
        stop("Error: No program capacity before loop has closed.") 
    }
    
    if (i %% 1e3 == 0) {
      # print #iteration and program capacities
      cat('Iteration:', i, '\n')
      print(list_programs)
    }
  }
  
  return(data)
}

f_program_2_iapo <- function(data, new_outcome_name) {
  
  #############################################################
  #
  # Translate program assignment into potential outcome under policy.
  #
  # data :              dataset
  # new_outcome_name :  string, name of new variable
  #
  # Returns dataset.
  #
  #############################################################
  
  new_potential_outcome_name <- paste0("iapo_", new_outcome_name)
  
  data[[new_potential_outcome_name]] <- map(seq(nrow(data)), function(i) {
    iapo_program <- data[[new_outcome_name]][i]
    data[[iapo_program]][i]
  })
  
  return(data)
}

test <- f_alg_policy(db, "Belgian", "risk_log", "min", "upper", "p11_almp")







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