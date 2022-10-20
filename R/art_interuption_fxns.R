reduce_art_interuption <- function(art_interuption_rate, art_increase_year){
  ## read in input parameter file
  data <- readLines(here("THEMBISAv18/Rollout_Original.txt"))
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  formatted_data <- edit_formatted_data("rel_rate_art_by_year", 
                                                    new_values = art_interuption_rate, 
                                                    starting_year = art_increase_year)
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, here("THEMBISAv18/Rollout.txt"))
}
  
run_thembisa_scenario <- function(intervention_year, output_names, base_rate_reduction){
  data <- readLines(here("THEMBISAv18/Rollout.txt"))
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  if (!is.na(intervention_year)){
  formatted_data <- edit_formatted_data_incremental("rate_first_test_neg_fem_under_25", 
                                                      new_values = 0.2877 * base_rate_reduction, 
                                                      starting_year = intervention_year)
  }
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, here("THEMBISAv18/Rollout.txt"))
  ## compile and model
  run_thembisa()
  read_thembisa_scenario(output_names)
}


