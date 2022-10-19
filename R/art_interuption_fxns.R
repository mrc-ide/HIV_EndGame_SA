edit_formatted_data_incremental <- function(parameter_name, new_values, starting_year=1985, final_year=2070){
  # select parameter using dictionary
  parameter <- formatted_data$data[,which(dictionary$name == parameter_name)]
  # Edit value
  for (i in ((starting_year+1)-1985):((starting_year+1)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.2)
  }
  for (i in ((starting_year+2)-1985):((starting_year+2)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.4)
  }
  for (i in ((starting_year+3)-1985):((starting_year+3)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.6)
  }
  for (i in ((starting_year+4)-1985):((starting_year+4)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.8)
  }
  for (i in ((starting_year+5)-1985):((final_year+1)-1985)){
    parameter[i] <- new_values
  }
  # reassign to formatted data
  parameter -> formatted_data$data[,which(dictionary$name == parameter_name)]
  return(formatted_data)
}

reduce_art_interuption <- function(art_interuption_rate, art_increase_year){
  ## read in input parameter file
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  formatted_data <- edit_formatted_data("rel_rate_art_by_year", 
                                                    new_values = art_interuption_rate, 
                                                    starting_year = art_increase_year)
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
}
  
run_thembisa_scenario <- function(intervention_year, output_names, base_rate_reduction){
  data <- readLines("THEMBISAv18/Rollout.txt")
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  if (!is.na(intervention_year)){
  formatted_data <- edit_formatted_data_incremental("rate_first_test_neg_fem_under_25", 
                                                      new_values = 0.2877 * base_rate_reduction, 
                                                      starting_year = intervention_year)
  }
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  ## compile and model
  run_thembisa()
  read_thembisa_scenario(output_names)
}


