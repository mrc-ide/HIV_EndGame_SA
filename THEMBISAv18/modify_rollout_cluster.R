source("support_modify_inputs_cluster.R")

# Read in input in Thembisa format
data <- readLines("Rollout_Original.txt")

# Create data dictionary and save to manually add names
rollout_dictionary <- create_data_dictionary(data)

dir.create("data_dictionaries", FALSE, TRUE)
filepath <- "data_dictionaries/rollout.csv"

# Manually add in descriptive names for each row of data
# vector of names
dict_names <- c("rate_first_test_neg_fem_under_25",
"adults_tested",
"kids_5_to_14_yr_tested",
"prop_pos_oi_diag",
"rate_reg_test_sex_workers",
"rate_reg_test_15_to_19_yr",
"rate_reg_test_20_to_24_yr",
"rate_reg_test_25_to_49_yr",
"rate_reg_test_over_50_yr",
"rate_reg_test_preg_women",
"pct_inf_pos_mother_6_wk_scrn",
"pct_inf_pos_mother_pcr",
"prop_kids_test_18_mths",
"rate_hct_home_based",
"self_kits_dist_strat_1",
"self_kits_dist_strat_2",
"self_kits_dist_strat_3",
"self_kits_dist_strat_4",
"self_kits_dist_strat_5",
"self_kits_dist_strat_6",
"pct_preg_women_tested",
"pct_pmtct_offering_azt_nvp",
"pct_clinics_rescrn_late_preg",
"pct_women_rescrn_6_week_visit",
"pct_diag_untreated_nvp_during_bf",
"pct_preg_women_eligible_opt_b",
"pct_hiv_diag_no_bf",
"pct_incr_art_dur_if_art_in_preg",
"males_over_15_yr_start_art",
"females_over_15_yr_start_art",
"kids_under_15_yr_start_art",
"rate_art_init_fem_cd4_under_200",
"rate_art_init_kids_adv_hiv",
"rel_rate_art_by_year",
"pct_pulm_tb_cd4_low_elig_art",
"pct_pulm_tb_cd4_med_elig_art",
"pct_who_stage_3_elig_art",
"pct_preg_women_cd4_low_elig_art",
"pct_preg_women_cd4_med_elig_art",
"pct_asymp_non_preg_cd4_low_eilg_art",
"pct_asymp_non_preg_cd4_med_eilg_art",
"pct_asymp_non_preg_cd4_high_eilg_art",
"pct_pos_inf_elig_art",
"pct_pos_kids_1_to_4_elig_art",
"pct_pos_kids_5_to_14_elig_art",
"pct_new_diag_preg_women_start_art",
"pct_new_diag_oi_start_art",
"pct_new_diag_other_adult_start_art",
"pct_new_diag_kids_start_art_soon",
"pct_adults_cd4_low_start_art",
"pct_kids_on_art_vir_supp",
"total_start_prep",
"rel_rate_prep_msm_over_20",
"rel_rate_prep_women_over_20",
"prop_msm_elig_prep",
"prop_agyw_elig_prep",
"prop_other_risk_grps_elig_prep",
"rate_prep_init_preg_women",
"rate_vm_init_sex_workers",
"rate_vm_init_15_to_19_yr",
"rate_vm_init_20_to_24_yr",
"rate_vm_init_24_to_49_yr",
"rate_vm_init_over_50_yr",
"rate_vm_init_preg_women",
"mmc_performed",
"rel_rate_mmc_10_to_14_yr",
"rel_rate_mmc_15_to_19_yr",
"rel_rate_mmc_20_to_24_yr",
"rel_rate_mmc_25_to_49_yr",
"rel_rate_mmc_over_50_yr",
"pct_early_inf_mc",
"reduction_condom_fsw",
"reduction_condom_st",
"reduction_condom_lt")

# assign names to dictionary 
for (i in 1:length(rollout_dictionary$name)) {
  rollout_dictionary$name[i] <- dict_names[i]
}

# save data dictionary with names
save_dictionary(rollout_dictionary, filepath)

# Read in dictionary with data names
dictionary <- read.csv(filepath)

# Format data for easy modification in R
formatted_data <- format_data(data, dictionary)

## Make whatever changes to the input that you want here
# e.g. formatted_data <- edit_formatted_data("adults_tested", 10000) 

## Convert back to Thembisa format
rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)

write(rollout,"Rollout.txt")



