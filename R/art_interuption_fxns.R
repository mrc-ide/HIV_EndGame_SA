run_thembisa_scenario_future_variable <- function(intervention_year, output_names, base_rate_reduction){
  data <- readLines(here("THEMBISAv18/Rollout_Original.txt"))
  formatted_data2 <- format_data(data, dictionary)
  formatted_data3 <- edit_formatted_data(formatted_data2, "rel_rate_art_by_year", 
                                        new_values = art_interuption_rate, 
                                        starting_year = art_increase_year)
  if (!is.na(intervention_year)){
  # data2 <- readLines(here("THEMBISAv18/Rollout.txt"))
  formatted_data3 <- edit_formatted_data_incremental(formatted_data3, 
                                                    "rate_first_test_neg_fem_under_25", 
                                                      new_values = 0.2877 * base_rate_reduction, 
                                                      starting_year = intervention_year)
  }
  rollout <- convert_to_thembisa_format(formatted_data3, data, dictionary)
  write(rollout, here("THEMBISAv18/Rollout.txt"))
  ## compile and model
  run_thembisa()
  read_thembisa_scenario(output_names)
}


