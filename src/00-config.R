# ---------------------------------------------------------------------------- #
# 00-config
# ---------------------------------------------------------------------------- #
# 
# First, specify the location of the working directory and execute this script.
# It installs the required packages (except "devtools" and "causalDML"), and 
# specifies the seed, the required path-variables and variable lists.
#
# Requirements: 
# "R version 4.2.2 (2022-10-31 ucrt)"
#
# Libraries:
# 01: tidyverse 2.0.0, grf 2.3.1
# 02: devtools 2.4.5, causalDML 0.1.0, policytree 1.2.2
# 03: causalDML 0.1.0, fairml 0.8, glmnet 4.1-8
# 04: tidyverse 2.0.0
# 05: tidyverse 2.0.0 , gt 0.10.0, Hmisc 5.1-1
# 06: tidyverse, 2.0.0, readxl 1.4.3, readr 2.1.4
# 07: tidyverse, 2.0.0, readxl 1.4.3, readr 2.1.4
#
# ---------------------------------------------------------------------------- #
# Author: Sebastian Zezulka
# 2024-04-18
#
# ---------------------------------------------------------------------------- #
# TODO set working directory 
wd_path <- c("C:/Users/Zezulka/Documents/01_PhD/030-Projects/2023-01_ALMP_LTU")
setwd(wd_path)

# ---------------------------------------------------------------------------- #
# Set values
# ---------------------------------------------------------------------------- #

seed = 12345

package_list <- c("tidyverse", "grf", "devtools", "policytree", "fairml", "glmnet",
                  "gt", "Hmisc", "readxl", "readr")

belgiumcolor="#1704E0"
austriacolor="#E03D2B"

shape_male <- 17
shape_female <- 16

# ---------------------------------------------------------------------------- #
# Path variables
# ---------------------------------------------------------------------------- #

data_path_raw <- c("data/swissubase_1203_1_0/1203_ALMP_Data_E_v1.0.0.csv")

data_path_pre <- c("data/1203_ALMP.csv")

data_path_effects <- c("data/1203_ALMP_effects.csv")

data_path_risk <- c("data/1203_ALMP_effects_riskFemale001.csv")

data_path_sim <- c("data/1203_ALM_simulation_results.csv")

# ---------------------------------------------------------------------------- #
# Variable lists
# ---------------------------------------------------------------------------- #

effects_var_list <- c("age","canton_moth_tongue","city_big","city_medium","city_no",
                      "cw_age","cw_cooperative","cw_educ_above_voc","cw_educ_tertiary",
                      "cw_female","cw_missing","cw_own_ue","cw_tenure","cw_voc_degree",
                      "emp_share_last_2yrs","emp_spells_5yrs","employability",
                      "female","foreigner_b","foreigner_c","gdp_pc","married","other_mother_tongue","past_income",
                      "prev_job_manager","prev_job_sec_mis","prev_job_sec1","prev_job_sec2",
                      "prev_job_sec3","prev_job_self","prev_job_skilled","prev_job_unskilled",
                      "qual_semiskilled","qual_degree","qual_unskilled","qual_wo_degree",
                      "swiss","ue_cw_allocation1","ue_cw_allocation2","ue_cw_allocation3","ue_cw_allocation4",
                      "ue_cw_allocation5","ue_cw_allocation6","ue_spells_last_2yrs","unemp_rate"
                      )

risk_var_list <- c("age","canton_moth_tongue","city_big","city_medium","city_no",
                   "emp_share_last_2yrs","emp_spells_5yrs",
                   "female","foreigner_b","foreigner_c","gdp_pc","married","other_mother_tongue","past_income",
                   "prev_job_manager","prev_job_sec_mis","prev_job_sec1","prev_job_sec2",
                   "prev_job_sec3","prev_job_self","prev_job_skilled","prev_job_unskilled",
                   "qual_semiskilled","qual_degree","qual_unskilled","qual_wo_degree",
                   "swiss","ue_spells_last_2yrs","unemp_rate", "foreigner_married"
                   )

treatments_list <- c("no program", "vocational", "computer", "language", "job search", "employment", "personality")

# ---------------------------------------------------------------------------- #
# Functions
# ---------------------------------------------------------------------------- #

f_install_packages <- function(package_list) {
  
  #############################################################
  # 
  # Install missing packages.
  #
  # package_list :  List of required packages.
  #
  #############################################################
  
  for (package in package_list) {
    
    if (!requireNamespace(package, quietly = TRUE)) {
      
      if (package %in% c("devtools", "causalDML")) {
        warning("Warning: Install packages \"devtools\" and/or \"causalDML\".")
        
      } else {
        install.packages(package)
      }
    }
  }
  
  cat('All packages installed. See warning for exceptions.')
}


# ---------------------------------------------------------------------------- #
# Load required packages
# ---------------------------------------------------------------------------- #

f_install_packages(package_list)

# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #
