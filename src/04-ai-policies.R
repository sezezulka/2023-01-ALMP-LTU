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

f_alg_policy <- function(df, policy_name, risk_score_name, assignment_type, new_outcome_name, capacity_mult=1) {
  
  #############################################################
  #
  # Performs algorithmically informed policy for allocation
  # into ALMPs. Returns dataset with new program allocation.
  #
  # df :                Dataset.
  # policy_name :       String, {Beglian, Austrian}.
  # risk_score_name :   String, name of risk score to be used.
  # --x-- effect_type --x-- : string, {min, max} 
  # assignment_type :   String, {upper, emp, lower}.
  # new_outcome_name :  String, name of program assignment variable.
  # capacity_mult :     Multiplier of capacities, default = 1.
  #
  #############################################################
  
  # get program capacities
  program_capacities <- f_get_capacities(df, capacity_mult)
  
  # create new outcome variable
  df <- f_create_outcome_var(df, new_outcome_name)
  
  # sort data according to policy by risk_score
  df <- f_risk_2_policy(df, policy_name, risk_score_name)
  
  # greedy allocation
  df <- f_greedy_allocation(df, program_capacities, "min", assignment_type, new_outcome_name)
  
  # add potential outcomes for program allocation
  df <- f_program_2_iapo(df, new_outcome_name)
  
  return(df)
}

f_get_capacities <- function(df, capacity_mult=1) {
  
  #############################################################
  # 
  # Get program capacities. Returns list.
  #
  # df :            Dataset.
  # capacity_mult : Multiplier of capacities, default = 1.
  #
  #############################################################
  
  # checks
  if (!(is.numeric(capacity_mult))) {
    stop("Error: Capacitiy multiplier must be numeric.")
  }
  
  # get observed capacities
  list_programs <- as.list(table(df$treatment6))
  
  # multiply capacities
  list_programs <- lapply(list_programs, function(x) x * capacity_mult)
  
  # clean program names in list 
  names(list_programs) <- gsub(" ", "_", names(list_programs))
  
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
  
  # check
  if (!is.character(new_outcome_name)) {
    stop("Error: Name for new outcome variable must be given as string.")
  }
  
  df[[new_outcome_name]] <- NA
  
  # df <- df%>%
  #   mutate( {{new_outcome_name}} := NA_character_)
  
  return(df)
}

f_risk_2_policy <- function(df, policy_name, risk_score_name) {
  
  #############################################################
  #
  # Sort dataset by risk score according to given policy.
  #
  # df :              Dataset.
  # policy_name :     String, {Belgian, Austrian}.
  # risk_score_name : String, name of risk score to be used.
  #
  #############################################################
  
  # checks
  test <- c("Belgian", "Austrian")
  if (!(policy_name %in% test)) {
    stop("Error: Policy name must be either \"Belgian\" or \"Austrian\".")
  }
  
  # policy sorting
  if (policy_name == "Belgian") {
    
    # sort by risk_score in descending order
    df <- df %>% 
      arrange(desc(!!sym(risk_score_name)))
    
  } else if (policy_name == "Austrian") {
    
    # first select individuals with middle risk_scores, then rest
    df_middle <- df %>% 
      arrange(desc( !!sym(risk_score_name) )) %>%
      filter( !!sym(risk_score_name)  <= 0.7,  !!sym(risk_score_name)  >= 0.3)
    
    df_rest <- df %>%
      arrange(desc( !!sym(risk_score_name) )) %>%
      filter( !!sym(risk_score_name)  > 0.7 |  !!sym(risk_score_name)  < 0.3) %>%
      # random order across non-selected individuals
      sample_n(nrow(.))
    
    df <- rbind(df_middle, df_rest)
  }
  
  return(df)
}

f_find_effective_program_i <- function(df, temp_program_names, effect_type, i) {
  
  #############################################################
  #
  # Select most effective program from available programs.
  #
  # df :                  Dataset.
  # temp_program_names :  List of available program names.
  # effect_type :         String, {min, max}.
  # i :                   Row index.
  #
  # Returns name of most effective program.
  #
  #############################################################
  
  # check
  test <- c("min", "max")
  if (!(effect_type %in% test)) {
    stop("Error: Effect type is miss-specified. Must be max or min.")
  }
  
  temp_program_names <- paste0('iapo_', temp_program_names)
  
  if (effect_type == "min") {
    # for treatment to minimise target variable
    
    assigned_program_name <- df %>%
      slice(i) %>%
      select(all_of(temp_program_names)) %>%
      mutate(best_program = names(.)[which.min(.)]) %>%
      select(best_program) %>%
      pull()
    
  } else if (effect_type == "max") {
    # for treatment to maximise target variable
    
    assigned_program_name <- df %>%
      slice(i) %>%
      select(all_of(temp_program_names)) %>%
      mutate(best_program = names(.)[max.col(.)]) %>%
      select(best_program) %>%
      pull()
  }
  
  assigned_program_name <- sub('iapo_', '', assigned_program_name)
  
  return(assigned_program_name)
}

f_select_propensity_program_i <- function(df, temp_program_names, i) {
  
  #############################################################
  # 
  # Sample program assignment weighted by propensities, returns
  # program name.
  #
  # df :                  Dataset.
  # temp_program_names :  List of available program names.
  # i :                   Row index.
  #
  #############################################################
  
  temp_program_names <- paste0('em_', temp_program_names)
  
  # sample program assignment weighted by propensities 
  assigned_program_name <- df %>%
    slice(i) %>%
    select(all_of(temp_program_names)) %>% 
    mutate(selected_program = sample(names(.), size=1, replace=FALSE, .)) %>%
    select(selected_program) %>%
    pull()
  
  # clean program name
  assigned_program_name <- sub('em_', '', assigned_program_name)
    
  return(assigned_program_name) 
}

f_select_random_program <- function(df, temp_program_names) {
  
  #############################################################
  # 
  # Sample program assignment uniformly from available programs.
  #
  # df :                  Dataset.
  # temp_program_names :  List of available program names. 
  #
  #############################################################
  
  # sample uniformly from available programs
  assigned_program_name <- sample(temp_program_names, 1)
  
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

f_greedy_allocation <- function(df, list_programs, effect_type, assignment_type, new_outcome_name) {
  
  #############################################################
  #
  # Greedy allocation of individuals into ALMPs according to capacity and
  # chosen strategy. 
  #
  # df :                Dataset.
  # list_programs :     List of available ALMPs and capacities.
  # effect_type :       String, {min, max}.
  # assignment_type :   String, {upper, emp, lower}.
  # new_outcome_name :  String, name of new variable.
  #
  #############################################################
  
  # checks
  test <- c("upper", "emp", "lower")
  if (!(assignment_type %in% test)) {
    stop("Error: Assignment type must be \"upper\", \"lower\", or \"emp\".")
  }
  
  n <- nrow(df)
  for (i in 1:n) {
    
    # checks
    if (all(list_programs > 0)) {
      # test availability of capacities
      
      # select available program names 
      temp_program_names <- f_temp_program_names(list_programs)
      
      if (!any(nzchar(temp_program_names))) {
        # test if any non-zero character (= only "no program" left)
        warning("Warning: Some program name has zero-length character.")
      }
      
      # select name of assigned program
      if (assignment_type == "upper") {
        # upper bound, assign most effective available program
        
        assigned_program_name <- f_find_effective_program_i(df, temp_program_names, effect_type, i)
        
      } else if (assignment_type == "lower") {
        # lower bound, sample assignment uniformly from available programs
        
        assigned_program_name <- f_select_random_program(df, temp_program_names)

      } else if (assignment_type == "emp") {
        # "empirical" strategy, sample assignment weighted by propensity score
        
        assigned_program_name <- f_select_propensity_program_i(df, temp_program_names, i)

      }
      
      # assign program
      df[[new_outcome_name]][i] <- assigned_program_name
      
      # update program capacities
      list_programs <- f_update_capacities(list_programs, assigned_program_name)
    } else {
      
        stop("Error: No program capacity before loop has closed.") 
    }
    
    # print #iteration and program capacities
    if (i %% 1e3 == 0) {
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
  # df :                Dataset
  # new_outcome_name :  String, name of new program variable.
  #
  #############################################################
  
  # name of iapo variable under policy assignment
  new_potential_outcome_name <- paste0("iapo_", new_outcome_name)
  
  # add "iapo_" to assigned program names for look up
  df[[new_outcome_name]] <- paste0("iapo_", df[[new_outcome_name]])
  
  # look up IAPOs of assigned programs 
  df[[new_potential_outcome_name]] <- map_dbl(seq(nrow(df)), function(i) {
    program_name <- df[[new_outcome_name]][i]
    df[[program_name]][i]
  })
  
  return(df)
}

# ---------------------------------------------------------------------------- #
# Simulations
# ---------------------------------------------------------------------------- #

test_db <- db %>% 
  group_by(treatment6) %>%
  slice_sample(prop=0.1) %>%
  ungroup()

# policy choices
policy_style <- c("Belgian", "Austrian")
risk_scores <- c("risk_score_logistic")
assignment_style <- c("upper", "emp", "lower")
capacitiy_multiplier <- seq(1,5)

for (p in policy_style) {
  for (a in assignment_style) {
    for (c in capacitiy_multiplier) {
      
      policy_name <- paste("policy", p, "rlog", a, c, sep="_")
      print(paste0("Next policy: ", policy_name))
      
      test_db <- f_alg_policy(test_db, p, "risk_score_logistic", a, policy_name, c)
    }
  }
}

# visualise 

# Select variables with a specific prefix
selected_vars <- grep("iapo_policy_Belgian_rlog_upper", names(test_db), value = TRUE)

# Calculate mean for selected variables
means_df <- test_db %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

# Convert the data to long format for ggplot
means_long <- means_df %>%
  tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                      names_to = "variable",
                      values_to = "mean_value") %>%
  mutate(factor_variable = factor(substring(variable, nchar(variable), nchar(variable))))

# Create a ggplot with points connected by lines
pre_LTU_f <- mean(test_db$y_exit12[test_db$female==1])
pre_LTU_m <- mean(test_db$y_exit12[test_db$female==0])
pre_LTU <- mean(test_db$y_exit12)

ggplot(means_long, aes(x = factor_variable, y = mean_value, color = as.factor(female))) +
  geom_line(aes(group = female), size = 1) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_point(aes(x = 0, y = pre_LTU_f), color = "#00AFBB", size = 3) +  # Custom point
  geom_point(aes(x = 0, y = pre_LTU_m), color = "#D55E00", size = 3) +  # Custom point
  geom_point(aes(x = 0, y = pre_LTU), color = "black", size = 3) +  # Custom point
  labs(title = "Mean of LTU after Belgian/rlog/upper, by Gender",
       x = "Capacity Multiplier",
       y = "LTU Share",
       color = "Gender") +
  scale_x_discrete(breaks = c("0", levels(means_long$factor_variable)),
                   labels = c("0", levels(means_long$factor_variable))) +
  theme_minimal()

# TODO add pre-LTU gap
# TODO how to combine some of these plots in one?


# ---------------------------------------------------------------------------- #
write.csv(test_db, file="data/1203_ALMP_Sample_Simulations_Test.csv")

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