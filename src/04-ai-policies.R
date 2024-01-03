# ---------------------------------------------------------------------------- #
# 04-Algorithmically informed policies
# ---------------------------------------------------------------------------- #

library(tidyverse)

# ---------------------------------------------------------------------------- #
# set working directory
wd_path <- c("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")
setwd(wd_path)

# load config 
source("src/00-utils.R")

set.seed(seed)

db = read.csv(effect_data_path)

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_alg_policy <- function(df, policy, risk_score, effect_type, assignment_type, new_outcome_name, seed=1234) {
  
  #############################################################
  #
  # Performs algorithmically informed policy for allocation
  # into ALMPs. Returns dataset with policy allocation.
  #
  # df :                dataset, including treatment variable ("treatment6"),
  #                     estimated IAPOs, propensity scores, ...
  # policy :            string, {Beglian, Austrian}
  # risk_score :        string, name of risk score to be used
  # effect_type :       string, {min, max} 
  # assignment_type :   string, {upper, emp, lower}
  # new_outcome_name :  string, name of new variable
  # seed :              set seed
  #
  #############################################################
  
  # get program capacities
  program_capacities <- f_get_capacities(df, treatment6)
  
  # create new outcome variable
  df <- f_create_outcome_var(df, new_outcome_name)
  
  # sort data according to policy by risk_score
  df <- f_risk_2_policy(df, policy, risk_score)
  
  # greedy allocation
  df <- f_greedy_allocation(df, program_capacities, effect_type, assignment_type, new_outcome_name, seed=1234)
  
  # add potential outcomes for program allocation
  df <- f_program_2_iapo(df, new_outcome_name)
  
  return(df)
}

f_get_capacities <- function(df, name_treatment_var) {
  
  #############################################################
  # 
  # Get program capacities.
  #
  # df :                  dataset
  # name_treatment_var :  name of treatment variable
  #
  # Returns list.
  #
  #############################################################
  
  # program_capacities <- colSums(wm)
  # table(df$treatment6)
  
  program_capacities <- df %>% 
    group_by({{name_treatment_var}}) %>%
    summarise(N = n())
  
  list_programs <- as.list(setNames(program_capacities$N, program_capacities %>% pull({{ name_treatment_var }})))
  
  # change names in list 
  # TODO fix requirement 
  new_names <- paste0("iapo_", gsub(" ", "_", names(list_programs)))
  names(list_programs) <- new_names
  
  return(list_programs)
  
}

f_create_outcome_var <- function(df, new_outcome_name) {
  
  #############################################################
  #
  # Creates new outcome variable for algorithmically informed policy
  #
  # df :              dataset
  # new_outcome_name :  string, name of new variable
  #
  #############################################################
  
  if (!is.character(new_outcome_name)) {
    stop("Error: Name for new outcome variable must be given as string.")
  }
  
  df[[new_outcome_name]] <- NA
  
  # df <- df%>%
  #   mutate( {{new_outcome_name}} := NA_character_)
  
  return(df)
}

f_risk_2_policy <- function(df, policy, risk_score) {
  
  #############################################################
  #
  # sort dataset by risk score according to given policy
  #
  # df :        dataset
  # policy :      string, {Belgian, Austrian}
  # risk_score :  string, name of risk score to be used
  #
  #############################################################
  
  if (policy == "Belgian") {
    
    # sort by risk_score in descending order
    df <- df %>% 
      arrange(desc(!!sym(risk_score)))
    
    return(df)
    
  } else if (policy == "Austrian") {
    
    # first select individuals with middle risk_scores, then rest
    df_middle <- df %>% 
      arrange(desc( !!sym(risk_score) )) %>%
      filter( !!sym(risk_score)  <= 0.7,  !!sym(risk_score)  >= 0.3)
    
    df_rest <- df %>%
      arrange(desc( !!sym(risk_score) )) %>%
      filter( !!sym(risk_score)  > 0.7 |  !!sym(risk_score)  < 0.3) %>%
      # random order across non-selected individuals
      sample_n(nrow(.))
    
    df <- rbind(df_middle, df_rest)
    
    return(df)
    
  } else {
    
    stop("Error: Policy must be either \"Belgian\" or \"Austrian\".")
  }
}

f_find_effective_program_i <- function(df, temp_program_names, effect_type, i) {
  
  #############################################################
  #
  # select most effective treatment from available programs
  #
  # df :                dataset
  # temp_program_names :  list, program names
  # i :                   row number
  # effect_type :         string, {min, max} 
  #
  # Returns name of most effective program.
  #
  #############################################################
  
  if (effect_type == "min") {
    # effect that minimises target variable
    
    assigned_program_name <- df %>%
      slice(i) %>%
      select(all_of(temp_program_names)) %>%
      mutate(best_program = names(.)[which.min(.)]) %>%
      select(best_program) %>%
      pull()
    

    return(assigned_program_name)
    
  } else if (effect_type == "max") {
    # effect that maximises target variable
    
    assigned_program_name <- df %>%
      slice(i) %>%
      select(all_of(temp_program_names)) %>%
      mutate(best_program = names(.)[max.col(.)]) %>%
      select(best_program) %>%
      pull()
    
    return(assigned_program_name)
    
  } else {

    stop("Error: Effect type is miss-specified. Must be max or min.")
  }
}

f_draw_program_assignment <- function(programs, propensities) {
  
  #############################################################
  # 
  # programs :      list of programs, length n
  # propensities :  list of propensities, length n
  #
  # Returns string with sampled program.
  #
  #############################################################
  
  sampled_program <- sample(programs, 1, prob = propensities)
  return(sampled_program)
}

f_find_propensity_program_i <- function(df, temp_program_names, i, seed=1234) {
  
  #############################################################
  # 
  # select program by propensity score, propensities as weights
  #
  # df : 
  # temp_program_names :
  # i :
  # seed : 
  #
  # Returns name of selected program.
  #
  #############################################################
  
  temp_em_program_names <- sub('iapo', 'em', temp_program_names)
  
  # # deterministic assignment of program with highest propensity
  # assigned_program_name <- df %>%
  #   slice(i) %>%
  #   select(all_of(temp_em_program_names)) %>%
  #   mutate(best_program = names(.)[max.col(.)]) %>%
  #   select(best_program) %>%
  #   pull()
  
  # probabilistic assignment using propensities as weights
  assigned_program_name <- df %>%
    slice(i) %>%
    select(all_of(temp_em_program_names)) %>% 
    mutate(selected_program = f_draw_program_assignment(names(.), .)) %>%
    select(selected_program) %>%
    pull()
  
  # change name back to "iapo_" for later 
  assigned_program_name <- sub('em', 'iapo', assigned_program_name)
    
  return(assigned_program_name) 
}

f_find_random_program_i <- function(df, temp_program_names, i, seed=1234) {
 
  
  #############################################################
  # 
  # select random (uniformly) program from available 
  #
  # df : 
  # temp_program_names :
  # i :
  # seed : 
  #
  # Returns name of selected program.
  #
  #############################################################
  
  # uniform probabilities for assignment
  n <- length(temp_program_names)
  prob_uniform <- rep(1/n, n)
  
  assigned_program_name <- df %>%
    slice(i) %>%
    select(all_of(temp_program_names)) %>% 
    mutate(selected_program = f_draw_program_assignment(names(.), prob_uniform)) %>%
    select(selected_program) %>%
    pull()
  
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

f_greedy_allocation <- function(df, list_programs, effect_type, assignment_type, new_outcome_name, seed=1234) {
  
  #############################################################
  #
  # performs greedy allocation of individuals into ALMPs
  # according to capacity
  #
  # df :              dataset
  # list_programs :     list of available ALMPs and capacities 
  # effect_type :       string, {min, max} 
  # assignment_type :   string, {upper, emp, lower}
  # new_outcome_name :  string, name of new variable
  #
  #############################################################
  
  n <- nrow(df)
  
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
        
        assigned_program_name <- f_find_effective_program_i(df, temp_program_names, effect_type, i)
        
      } else if (assignment_type == "lower") {
        # lower bound, assign random available program
        
        assigned_program_name <- f_find_random_program_i(df, temp_program_names, i, seed)
        
      } else if (assignment_type == "emp") {
        # empirical strategy, assign program by propensity score
        
        assigned_program_name <- f_find_propensity_program_i(df, temp_program_names, i, seed)
          
      } else {
        
        stop("Error: Assignment type must be \"upper\", \"lower\", or \"emp\".")
      }
      
      # assign program
      df[[new_outcome_name]][i] <- assigned_program_name
      
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
  
  return(df)
}

f_program_2_iapo <- function(df, new_outcome_name) {
  
  #############################################################
  #
  # Translate program assignment into potential outcome under policy.
  #
  # df :              dataset
  # new_outcome_name :  string, name of new variable
  #
  # Returns dataset.
  #
  #############################################################
  
  new_potential_outcome_name <- paste0("iapo_", new_outcome_name)
  
  df[[new_potential_outcome_name]] <- map_dbl(seq(nrow(df)), function(i) {
    iapo_program <- df[[new_outcome_name]][i]
    df[[iapo_program]][i]
  })
  
  return(df)
}

# ---------------------------------------------------------------------------- #

for (i in 1:6) {
  
  policy <- paste0("policy", i)
  db <- f_alg_policy(db, policy[1], policy[2], "min", policy[4], policy[5])
}


# TODO test what happens if function is called again on exact same parameters 
# TODO does random/prop-score weighted assignment required several runs and averaging? 
policy_style <- c("Belgian", "Austrian")
risk_scores <- c("risk_score_logistic")
assignment_style <- c("upper", "emp", "lower")
policy_names <- 


policy_list <- list(
  policy01 <- c("Belgian", "risk_score_logistic", "min", "upper", "p_b_log_min_upper"),
  policy02 <- c("Belgian", "risk_score_logistic", "min", "emp", "p_b_log_min_emp"),
  policy03 <- c("Belgian", "risk_score_logistic", "min", "lower", "p_b_log_min_lower"),
  
  policy04 <- c("Austrian", "risk_score_logistic", "min", "upper", "p_a_log_min_upper"),
  policy05 <- c("Austrian", "risk_score_logistic", "min", "emp", "p_a_log_min_emp"),
  policy06 <- c("Austrian", "risk_score_logistic", "min", "lower", "p_a_log_min_lower"),
)


test <- f_alg_policy(test, policy05[1], policy05[2], "min", policy05[4], policy05[5])

write.csv(test, file="data/1203_ALMP_Sample_Simulations_Test.csv")

# ---------------------------------------------------------------------------- #
# analysis
# ---------------------------------------------------------------------------- #
mean(test$y_exit12)

# gender
# ---------------------------------------------------------------------------- #
mean(test$y_exit12[test$female==0])
mean(test$y_exit12[test$female==1])

# Belgian
mean(test$iapo_p_b_log_min_upper[test$female==0])
mean(test$iapo_p_b_log_min_upper[test$female==1])

mean(test$iapo_p_b_log_min_emp[test$female==0])
mean(test$iapo_p_b_log_min_emp[test$female==1])

mean(test$iapo_p_b_log_min_lower[test$female==0])
mean(test$iapo_p_b_log_min_lower[test$female==1])

# Austrian
mean(test$iapo_p_a_log_min_upper[test$female==0])
mean(test$iapo_p_a_log_min_upper[test$female==1])

mean(test$iapo_p_a_log_min_emp[test$female==0])
mean(test$iapo_p_a_log_min_emp[test$female==1])

mean(test$iapo_p_a_log_min_lower[test$female==0])
mean(test$iapo_p_a_log_min_lower[test$female==1])


# citizenship
# ---------------------------------------------------------------------------- #
mean(test$y_exit12[test$swiss==0])
mean(test$y_exit12[test$swiss==1])

# Belgian
mean(test$iapo_p_b_log_min_upper[test$swiss==0])
mean(test$iapo_p_b_log_min_upper[test$swiss==1])

mean(test$iapo_p_b_log_min_emp[test$swiss==0])
mean(test$iapo_p_b_log_min_emp[test$swiss==1])

mean(test$iapo_p_b_log_min_lower[test$swiss==0])
mean(test$iapo_p_b_log_min_lower[test$swiss==1])

# Austrian
mean(test$iapo_p_a_log_min_upper[test$swiss==0])
mean(test$iapo_p_a_log_min_upper[test$swiss==1])

mean(test$iapo_p_a_log_min_emp[test$swiss==0])
mean(test$iapo_p_a_log_min_emp[test$swiss==1])

mean(test$iapo_p_a_log_min_lower[test$swiss==0])
mean(test$iapo_p_a_log_min_lower[test$swiss==1])


# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #