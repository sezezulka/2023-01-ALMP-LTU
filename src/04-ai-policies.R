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

# db = read.csv(effect_risk_data_path)
db <- read.csv("data/1203_ALMP_effects_risk_fairFemale.csv")


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
      filter( !!sym(risk_score_name) <= 0.66 & !!sym(risk_score_name)  >= 0.25)
    
    df_rest <- df %>%
      arrange(desc( !!sym(risk_score_name) )) %>%
      filter( !!sym(risk_score_name) > 0.66 | !!sym(risk_score_name) < 0.25) %>%
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
      
      # TODO if only one program left, assign everyone this program
      
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

f_average_random_runs <- function(df, policy_names, n_iter = 10) {
  
  #############################################################
  #
  # df : 
  # policy_name_later : Vector of Policy names
  # n_iter :            Number of iterations per policy.
  #
  #############################################################
  
  # select all iapo results from random runs 
  df_runs <- df %>%
    select(matches("(run.*iapo)|(iapo.*run)")) 
  
  count <- ncol(df_runs) / n_iter
  df_average_random <- matrix(NA, nrow(df_runs), count)
  df_sd_random <- matrix(NA, nrow(df_runs), count)
  
  # average over runs from one policy
  for (i in 1:count) {
    start_col <- (i - 1) * 10 + 1
    end_col <- i * 10
    
    average_iapo <- rowMeans(df_runs[,start_col:end_col], na.rm = TRUE)
    sd_iapo <- apply(df_runs[,start_col:end_col], 1, sd)
    
    df_average_random[,i] <- average_iapo
    df_sd_random[,i] <- sd_iapo
  }
  
  colnames(df_average_random) <- paste0("iapo_", policy_names)
  colnames(df_sd_random) <- paste0("sd_", policy_names)
  
  # drop run variables 
  df <- df %>%
    select(-matches("run"))
  
  df <- cbind.data.frame(df, df_average_random, df_sd_random)
  return(df)
}


# ---------------------------------------------------------------------------- #
# Simulations
# ---------------------------------------------------------------------------- #

# policy choices
policy_style <- c("Austrian") # , "Belgian"
risk_scores <- c("risk_score_log", "risk_score_sp", "risk_score_eo") #, "risk_score_if")
assignment_style <- c("upper", "lower") # , "emp"
capacitiy_multiplier <- seq(1,5)

policy_name_later <- c()

for (p in policy_style) {
  for (a in assignment_style) {
    for (r in risk_scores) {
      for (c in capacitiy_multiplier) {
          
        if (a=="lower") {
          # save policy names for averaging 
          p_name <- paste("policy", p, r, a, c, sep="_")
          policy_name_later <- c(policy_name_later, p_name)
            
          for (i in seq(1:10)) {
            # run random assignment 10x
            policy_name <- paste("policy", p, r, a, "run", i, c, sep="_")
            print(paste0("Next policy: ", policy_name))
              
            db <- f_alg_policy(db, p, r, a, policy_name, c)
          }
        } else {
          # assign most efficient program  
          policy_name <- paste("policy", p, r, a, c, sep="_")
          print(paste0("Next policy: ", policy_name))
            
          db <- f_alg_policy(db, p, r, a, policy_name, c)
        }
      }
    }
  }
}

# average over iteration of random assignments and add mean/sd to data
db <- f_average_random_runs(db, policy_name_later, n_iter=10)


# ---------------------------------------------------------------------------- #
# visualise 
# ---------------------------------------------------------------------------- #

f_vis_LTU <- function(df, policy_name, s_attribute, fair_attribute) {
  
  #############################################################
  #
  # Plots LTU share under a policy by risk scores
  #
  # df :            Dataset.
  # policy_name :   String, "Belgian" or "Austrian"
  # s_attribute :   String, "swiss" or "female" for analysis
  # fair_attribute : String, variable for which fairness risk scores are estimated.
  #
  #############################################################
  
  # Select variables with a specific prefix
  name_ <- paste0('iapo_policy_', policy_name, '_')
  selected_vars <- grep(name_, names(df), value = TRUE)
  
  # Calculate mean for selected variables
  df_means <- df %>%
    group_by( !!sym(s_attribute) ) %>%
    summarise(across(all_of(selected_vars), mean))
  
  # Convert the data to long format, add factors
  df_means_long <- df_means %>%
    tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                        names_to = "policy",
                        values_to = "LTU_share") %>%
    mutate(multiplier = factor(substring(policy, nchar(policy), nchar(policy))),
           risk_score = factor(rep(rep(1:4, each = 5), times = n() / 5)[1:n()]),
           groups = factor(rep(1:(n() %/% 5 + 1), each = 5)[1:n()]),
           strategy = factor(rep(rep(1:2, each = 20), times = n() / 2)[1:n()])
    )
  
  # pre-intervention LTU shares
  pre_LTU_s <- mean(df$y_exit12[df[,s_attribute]==1])
  pre_LTU_non_s <- mean(df$y_exit12[df[,s_attribute]==0])
  pre_LTU <- mean(df$y_exit12)
  
  # title and labels
  title <- paste0("Share of LTU under ", policy_name, " policy.")
  subtitle <- paste0("Fair risk scores for ", fair_attribute)
  if (s_attribute == "female") {
    label_s <- c("0" = "Male", "1" = "Female")
    y_limits <- c(0.33, 0.45)
  } else if (s_attribute == "swiss") {
    label_s <- c("0" = "Non-Citizen", "1" = "Citizen")
    y_limits <- c(0.33, 0.52)
  } else {
    stop("Error: Sensitive attribute must be female or swiss.")
  }
  

  # plot
  p <- ggplot(df_means_long, aes(x = multiplier, 
                         y = LTU_share, 
                         color = as.factor(risk_score), 
                         shape = as.factor(!!sym(s_attribute) ), 
                         alpha=as.factor(strategy))) +
    geom_line(aes(group = groups), size = 1) +
    geom_point(size = 4, position = position_dodge(width = 0.1)) +
    geom_hline(yintercept = pre_LTU_s, linetype = "dashed") +
    geom_hline(yintercept = pre_LTU_non_s, linetype = "dashed") +
    geom_hline(yintercept = pre_LTU, linetype = "dashed") +
    labs(title = title,
         subtitle = subtitle,
         x = "Capacity Multiplier",
         y = "LTU Share",
         color = "Risk Scores",
         shape = paste0("Sensitive Attribute: ", s_attribute),
         alpha = "Assignment strategy") +
    scale_color_manual(
      values = c(1,2,3,4),
      labels = c("1" = "Logistic Regression", "2" = "Independence", "3" = "Separation", "4" = "Individual Fairness")
    ) +
    scale_shape_manual(
      values = c(16, 17),
      labels = label_s
    ) + 
    scale_alpha_manual(
      values = c(1, 0.5),
      labels = c("1" = "Upper", "2" = "Lower")
    ) +
    coord_cartesian(ylim = y_limits) +
    theme_gray()
  
  print(p)
  return(df_means_long)
}


f_vis_LTU(db, "Belgian", "female", "Gender")
f_vis_LTU(db, "Austrian", "female", "Gender")

f_vis_LTU(db, "Belgian", "swiss", "Gender")
f_vis_LTU(db, "Austrian", "swiss", "Gender")





write.csv(db, file="data/1203_ALMP_effects_risk_fairFemale_sim.csv")



# ---------------------------------------------------------------------------- #
# Archive
# ---------------------------------------------------------------------------- #

# Select variables with a specific prefix
selected_vars <- grep("iapo_policy_Belgian_", names(db), value = TRUE)

# Calculate mean for selected variables
means_df <- db %>%
  group_by(female) %>%
  summarise(across(all_of(selected_vars), mean))

# Convert the data to long format for ggplot
means_long <- means_df %>%
  tidyr::pivot_longer(cols = starts_with("iapo_policy_"),
                      names_to = "variable",
                      values_to = "mean_value") %>%
  mutate(multiplier = factor(substring(variable, nchar(variable), nchar(variable))),
         risk_score = factor(rep(rep(1:4, each = 5), times = n() / 5)[1:n()]),
         groups = factor(rep(1:(n() %/% 5 + 1), each = 5)[1:n()]),
         strategy = factor(rep(rep(1:2, each = 20), times = n() / 2)[1:n()])
         )

# Create a ggplot with points connected by lines
pre_LTU_c <- mean(db$y_exit12[db$female==1])
pre_LTU_nc <- mean(db$y_exit12[db$female==0])
pre_LTU <- mean(db$y_exit12)

ggplot(means_long, aes(x = multiplier, 
                       y = mean_value, 
                       color = as.factor(risk_score), 
                       shape = as.factor(female), 
                       alpha=as.factor(strategy))) +
  geom_line(aes(group = groups), size = 1) +
  geom_point(size = 4, position = position_dodge(width = 0.01)) +
  geom_hline(yintercept = pre_LTU_c, linetype = "dashed") +
  geom_hline(yintercept = pre_LTU_nc, linetype = "dashed") +
  geom_hline(yintercept = pre_LTU, linetype = "dashed") +
  labs(title = "Share of LTU under Belgian policy",
       x = "Capacity Multiplier",
       y = "LTU Share",
       color = "Type Risk Score",
       shape = "Sensitive Attribute: Gender",
       alpha = "Assignment strategy") +
  scale_color_manual(
    values = c(1,2,3,4),
    labels = c("1" = "Logistic Regression", 
               "2" = "Independence", 
               "3" = "Separation", 
               "4" = "Individual Fairness")
  ) +
  scale_shape_manual(
    values = c(16, 17),
    labels = c("0" = "Male", 
               "1" = "Female")
  ) + 
  scale_alpha_manual(
    values = c(1, 0.5),
    labels = c("1" = "Upper", 
               "2" = "Lower")
  ) +
  theme_gray()



# ---------------------------------------------------------------------------- #

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