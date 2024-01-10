# ---------------------------------------------------------------------------- #
# 00-Config and Utilities 
# ---------------------------------------------------------------------------- #

# Requirements

# "R version 4.2.2 (2022-10-31 ucrt)"

# Libraries:
# 01: tidyverse, grf
# 02: devtools, causalDML, policytree
# 03: causalDML
# 04: tidyverse
# 05: tidyverse, gt, Hmisc

# ---------------------------------------------------------------------------- #
# Set values
# ---------------------------------------------------------------------------- #

seed = 12345

package_list <- c("tidyverse", "grf", "devtools", "policytree", "gt", "Hmisc")

raw_data_path <- c("data/swissubase_1203_1_0/1203_ALMP_Data_E_v1.0.0.csv")

effect_data_path <- c("data/1203_ALMP_Sample_IATEs.csv")

effect_risk_data_path <- c("data/1203_ALMP_Sample_IATEs_fair_risk.csv")

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
                   "swiss","ue_spells_last_2yrs","unemp_rate"
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
# Packages
# ---------------------------------------------------------------------------- #

f_install_packages(package_list)

# ---------------------------------------------------------------------------- #
# End
# ---------------------------------------------------------------------------- #
