
library(dplyr)
library(tidyr)
library(ggplot2)


# Compiles and runs the Thembisa model
run_thembisa <- function(){
  system("./thembisa.exe")
}

# Read output from Thembisa and assign column headers
# read_output <- function(output_name){
#   # Import output txt
#   output_txt <- paste(output_name, "txt", sep = ".")
#   output <- read.delim(output_txt, header=FALSE, row.names = 1)
#   names(output)[2:87] <- seq(1985, 2100)
#   # names(output)[1] <- "Simulation"
#   output <- output %>% select(-V2) 
#   t_output <- as.data.frame(t(output))
#   output <- as.data.frame(as_tibble(t_output, rownames = "Year"))
#   output$Year <- as.numeric(output$Year)
#   names(output)[2] <- "Simulation_1"
#   return(output$Simulation_1)
# }

edit_formatted_data <- function(formatted_data, parameter_name, new_values, starting_year=1985, final_year=2100){
  # select parameter using dictionary
  parameter <- formatted_data$data[,which(dictionary$name == parameter_name)]
  # Edit value
  for (i in ((starting_year+1)-1985):((final_year+1)-1985)){
    parameter[i] <- new_values
  }
  # reassign to formatted data
  parameter -> formatted_data$data[,which(dictionary$name == parameter_name)]
  return(formatted_data)
}

edit_formatted_data_incremental <- function(formatted_data, parameter_name, new_values, starting_year=1985, final_year=2100){
  # select parameter using dictionary
  parameter <- formatted_data$data[,which(dictionary$name == parameter_name)]
  # Edit value
  for (i in ((starting_year+1)-1985):((starting_year+1)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.5)
  }
  for (i in ((starting_year+2)-1985):((final_year+1)-1985)){
    parameter[i] <- new_values
  }
  # reassign to formatted data
  parameter -> formatted_data$data[,which(dictionary$name == parameter_name)]
  return(formatted_data)
}


read_thembisa_output <- function(output_name){
  output_txt <- paste(output_name, "txt", sep = ".")
  # output_txt <- paste("THEMBISAv18", output_txt, sep = "/")
  output <- read.delim(output_txt, header=FALSE, row.names = 1)
  output <- output %>% select(-V2)
  t_output <- t(output)
  output_new <- as.data.frame(as_tibble(t_output))
  names(output_new) <- seq_along(output_new)
  output_new$year <- seq(1985, 2100)
  pivot_longer(output_new, -year, names_to = "parameter_set")
}


read_thembisa_scenario <- function(output_names){
  temp <- lapply(output_names, read_thembisa_output)
  names(temp) <- output_names
  bind_rows(temp, .id = "indicator")
}

# run_thembisa_scenario <- function(pitc_reduction_year, output_names, base_rate_reduction){
#   ## read in input parameter file
#   data <- readLines("Rollout_Original.txt")
#   ## write unedited input parameter file
#   formatted_data <- format_data(data, dictionary)
#   if (!is.na(pitc_reduction_year)){
#     formatted_data <- edit_formatted_data_incremental(formatted_data, "rate_first_test_neg_fem_under_25", 
#                                                       new_values = 0.2877 * base_rate_reduction, 
#                                                       starting_year = pitc_reduction_year)
#   }
#   rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
#  write(rollout, "THEMBISAv18/Rollout.txt")
#   ## compile and model
#   run_thembisa()
#  read_thembisa_scenario(output_names)
# }

# run_thembisa_scenario_future_variable <- function(pitc_reduction_year, output_names, base_rate_reduction, 
#                                                   future_var_parameter, future_var_value, future_var_year){
#   data <- readLines("Rollout_Original.txt")
#   formatted_data2 <- format_data(data, dictionary)
#   formatted_data3 <- edit_formatted_data(formatted_data2, future_var_parameter, 
#                                          new_values = future_var_value/100, 
#                                          starting_year = future_var_year)
#   if (!is.na(pitc_reduction_year)){
#     # data2 <- readLines(here("THEMBISAv18/Rollout.txt"))
#     formatted_data3 <- edit_formatted_data_incremental(formatted_data3, 
#                                                        "rate_first_test_neg_fem_under_25", 
#                                                        new_values = 0.2877 * base_rate_reduction, 
#                                                        starting_year = pitc_reduction_year)
#   }
#   rollout <- convert_to_thembisa_format(formatted_data3, data, dictionary)
#   write(rollout, here("THEMBISAv18/Rollout.txt"))
#   ## compile and model
#   run_thembisa()
#   read_thembisa_scenario(output_names)
# }

# incremental reductions in condom usage
# to be included in run_thembisa_scenario_future_variables
# reduce_condom_usage_incremental  <- function(output_names, 
#                                              fsw_condom_usage_init,
#                                              st_condom_usage_init,
#                                              lt_condom_usage_init,
#                                              fsw_condom_usage_decrease, 
#                                              st_condom_usage_decrease,
#                                              lt_condom_usage_decrease,
#                                              condom_incr_years){
#   data <- readLines("Rollout_Original.txt")
#   formatted_data2 <- format_data(data, dictionary) # reads in original value in editable format
#   for (year in condom_incr_years){ #loops over each condom usage increase year and changes the proportion reduction of probability
#     formatted_data2 <- edit_formatted_data(formatted_data2, "reduction_condom_fsw", 
#                                            new_values = (1-fsw_condom_usage_decrease), 
#                                            starting_year = year)
#     formatted_data2 <- edit_formatted_data(formatted_data2, "reduction_condom_st", 
#                                            new_values = (1-st_condom_usage_decrease), 
#                                            starting_year = year)
#     formatted_data2 <- edit_formatted_data(formatted_data2, "reduction_condom_lt", 
#                                            new_values = (1-lt_condom_usage_decrease), 
#                                            starting_year = year)
#     fsw_condom_usage_decrease = fsw_condom_usage_init + fsw_condom_usage_decrease
#     # if condition prevents negative values for probabilities
#     if (fsw_condom_usage_decrease >= 1){
#       fsw_condom_usage_decrease <- 1 
#     }
#     st_condom_usage_decrease = st_condom_usage_init + st_condom_usage_decrease
#     if (st_condom_usage_decrease >= 1){
#       st_condom_usage_decrease <- 1
#     }
#     lt_condom_usage_decrease = lt_condom_usage_init + lt_condom_usage_decrease
#     if (lt_condom_usage_decrease >= 1){
#       lt_condom_usage_decrease <- 1
#     }
#   }
#   return(formatted_data2)
# }

# function to do reduction or increase of previous years probability / rate

edit_formatted_data_prev_year <- function(formatted_data, parameter_name, new_values, starting_year=1985, final_year=2100){
  # select parameter using dictionary
  parameter <- formatted_data$data[,which(dictionary$name == parameter_name)]
  # Edit value
  for (i in ((starting_year+1)-1985):((final_year+1)-1985)){
    parameter[i] <- parameter[i-1] * new_values
  }
  # reassign to formatted data
  parameter -> formatted_data$data[,which(dictionary$name == parameter_name)]
  return(formatted_data)
}

# to be included in run_thembisa_scenario_prev_year - reduces condom usage by a percentage of the previous year's
reduce_condom_usage_prev_year  <- function(formatted_data, 
                                           output_names, 
                                           fsw_condom_usage_decrease, 
                                           st_condom_usage_decrease,
                                           lt_condom_usage_decrease,
                                           condom_incr_years){
  data <- readLines("Rollout_Original.txt")
  formatted_data2 <- format_data(data, dictionary) # reads in original value in editable format
  for (year in condom_incr_years){ #loops over each condom usage increase year and changes the proportion reduction of probability
    formatted_data2 <- edit_formatted_data_prev_year(formatted_data2, "reduction_condom_fsw", 
                                                     new_values = (1-fsw_condom_usage_decrease), 
                                                     starting_year = year)
    formatted_data2 <- edit_formatted_data_prev_year(formatted_data2, "reduction_condom_st", 
                                                     new_values = (1-st_condom_usage_decrease), 
                                                     starting_year = year)
    formatted_data2 <- edit_formatted_data_prev_year(formatted_data2, "reduction_condom_lt", 
                                                     new_values = (1-lt_condom_usage_decrease), 
                                                     starting_year = year)
  }
  return(formatted_data2)
}

# reduces ART interruption rate by a percentage of the previous year's rate
reduce_art_interruption_prev_year  <- function(formatted_data2, output_names, art_interrupt_rate_decrease, art_incr_years){
  data <- readLines("Rollout_Original.txt")
  formatted_data2 <- format_data(data, dictionary)
  for (year in art_incr_years){
    formatted_data2 <- edit_formatted_data_prev_year(formatted_data2, "rel_rate_art_by_year", 
                                                     new_values = (1-art_interrupt_rate_decrease), 
                                                     starting_year = year)
    if (art_interrupt_rate_decrease >= 1){
      art_interrupt_rate_decrease <- 1 
    }
  }
  return(formatted_data2)
}

# increases ART interruption rate by a percentage of the previous year's rate
increase_art_interruption_prev_year  <- function(formatted_data2, output_names, art_interrupt_rate_increase, art_decr_years){
  data <- readLines("Rollout_Original.txt")
  formatted_data2 <- format_data(data, dictionary)
  for (year in art_decr_years){
    formatted_data2 <- edit_formatted_data_prev_year(formatted_data2, "rel_rate_art_by_year", 
                                                     new_values = (1+art_interrupt_rate_increase ), 
                                                     starting_year = year)
  }
  return(formatted_data2)
}

edit_formatted_data_maintain <- function(formatted_data, parameter_name, starting_year, final_year=2100){
  # select parameter using dictionary
  parameter <- formatted_data$data[,which(dictionary$name == parameter_name)]
  # Edit value
  for (i in ((starting_year)-1985):((final_year)-1985)){
    parameter[i+1] <- parameter[i]
  }
  # reassign to formatted data
  parameter -> formatted_data$data[,which(dictionary$name == parameter_name)]
  return(formatted_data)
}

# to be included in run_thembisa_scenario_prev_year
# maintains art interruption rates after reducing it temporariliy
maintain_art_retention <- function(formatted_data2, 
                                  output_names, 
                                  art_maintenance_years){
  for (year in art_maintenance_years){ #loops over each condom usage maintenance year and makes all years the same value as last reduction
    formatted_data2 <- edit_formatted_data_maintain(formatted_data2, "rel_rate_art_by_year", 
                                                    starting_year = year)
    
  }
  return(formatted_data2)
}

# maintains condom usage after reducing it temporariliy
maintain_condom_usage <- function(formatted_data2, 
                                  output_names, 
                                  condom_maintenance_years){
  for (year in condom_maintenance_years){ #loops over each condom usage maintenance year and makes all years the same value as last reduction
    formatted_data2 <- edit_formatted_data_maintain(formatted_data2, "reduction_condom_fsw", 
                                                    starting_year = year)
    formatted_data2 <- edit_formatted_data_maintain(formatted_data2, "reduction_condom_st", 
                                                    starting_year = year)
    formatted_data2 <- edit_formatted_data_maintain(formatted_data2, "reduction_condom_lt",
                                                    starting_year = year)
  }
  return(formatted_data2)
}

run_thembisa_scenario_prev_year <- function(pitc_reduction_year,
                                            condom_usage_reduction,
                                            fsw_condom_usage_decrease, 
                                            st_condom_usage_decrease,
                                            lt_condom_usage_decrease,
                                            condom_incr_years,
                                            condom_maintenance_years,
                                            art_coverage_increase,
                                            art_interrupt_rate_decrease,
                                            art_incr_years,
                                            art_coverage_decrease,
                                            art_interrupt_rate_increase,
                                            art_decr_years,
                                            art_maintenance_years,
                                            output_names, 
                                            base_rate_reduction){
  data <- readLines("Rollout_Original.txt")
  formatted_data2 <- format_data(data, dictionary)
  if (art_coverage_increase == TRUE){
    formatted_data2 <- reduce_art_interruption_prev_year(formatted_data2,
                                                         output_names, 
                                                           art_interrupt_rate_decrease, 
                                                           art_incr_years)
    formatted_data2 <- maintain_art_retention(formatted_data2, output_names, 
                                              art_maintenance_years)
  }
  if (art_coverage_decrease == TRUE){
    formatted_data2 <- increase_art_interruption_prev_year(formatted_data2,
                                                           output_names,
                                                           art_interrupt_rate_increase,
                                                           art_decr_years)
    formatted_data2 <- maintain_art_retention(formatted_data2, output_names, 
                                              art_maintenance_years)
  }
  if (condom_usage_reduction == TRUE){
    formatted_data2 <- reduce_condom_usage_prev_year(formatted_data2, output_names,
                                                     fsw_condom_usage_decrease, 
                                                     st_condom_usage_decrease,
                                                     lt_condom_usage_decrease,
                                                     condom_incr_years)
    formatted_data2 <- maintain_condom_usage(formatted_data2, output_names,
                                                     condom_maintenance_years)
  }
  if (!is.na(pitc_reduction_year)){
    formatted_data2 <- edit_formatted_data_incremental(formatted_data2, 
                                                       "rate_first_test_neg_fem_under_25", 
                                                       new_values = 0.2877 * base_rate_reduction, 
                                                       starting_year = pitc_reduction_year)
  }
  rollout <- convert_to_thembisa_format(formatted_data2, data, dictionary)
  write(rollout, "Rollout.txt")
  ## compile and model
  run_thembisa()
  read_thembisa_scenario(output_names)
}

read_thembisa_results_cluster <- function(pitc_reduction_years, pitc_reduction_percentage, scenarios, 
                                          scenario_names, baseline){
  names(scenarios) <- scenario_names
  bind_rows(scenarios, .id = "pitc_reduction_year") %>% 
    dplyr::rename(intervention = value) %>% 
    dplyr::left_join(baseline) %>% 
    dplyr::rename(baseline = value) %>% 
    separate(pitc_reduction_year, c("pitc_reduction_year", "test_reduction")) %>% 
    tidyr::pivot_longer(c(intervention, baseline), names_to = "scenario")
}


# incremental reductions in art interruption rate
# to be included in run_thembisa_scenario_future_variables
# reduce_art_interruption_incremental  <- function(output_names, art_interrupt_init, art_interrupt_rate_decrease, art_incr_years){
#   data <- readLines("Rollout_Original.txt")
#   formatted_data2 <- format_data(data, dictionary)
#   for (year in art_incr_years){
#     formatted_data2 <- edit_formatted_data(formatted_data2, "rel_rate_art_by_year", 
#                                            new_values = (1-art_interrupt_rate_decrease), 
#                                            starting_year = year)
#     art_interrupt_rate_decrease = art_interrupt_init + art_interrupt_rate_decrease
#     if (art_interrupt_rate_decrease >= 1){
#       art_interrupt_rate_decrease <- 1 
#     }
#   }
#   return(formatted_data2)
# }
# 
# run_thembisa_scenario_future_variables <- function(pitc_reduction_year, 
#                                                    fsw_condom_usage_init,
#                                                    st_condom_usage_init,
#                                                    lt_condom_usage_init,
#                                                    fsw_condom_usage_decrease, 
#                                                    st_condom_usage_decrease,
#                                                    lt_condom_usage_decrease,
#                                                   condom_incr_years,
#                                                   art_interrupt_rate_decrease,
#                                                   art_interrupt_init,
#                                                   art_incr_years,
#                                                   output_names, 
#                                                   base_rate_reduction){
#   data <- readLines("Rollout_Original.txt")
#   formatted_data2 <- format_data(data, dictionary)
#   if (!is.na(art_interrupt_rate_decrease)){
#     formatted_data2 <- reduce_art_interruption_incremental(output_names,
#                                                            art_interrupt_init, 
#                                                            art_interrupt_rate_decrease, 
#                                                            art_incr_years)
#   }
#   if (!is.na(condom_incr_years)){
#     formatted_data2 <- reduce_condom_usage_incremental(output_names,
#                                                        fsw_condom_usage_init,
#                                                        st_condom_usage_init,
#                                                        lt_condom_usage_init,
#                                                        fsw_condom_usage_decrease, 
#                                                        st_condom_usage_decrease,
#                                                        lt_condom_usage_decrease,
#                                                       condom_incr_years)
#     
#   }
#   if (!is.na(pitc_reduction_year)){
#     formatted_data2 <- edit_formatted_data_incremental(formatted_data2, 
#                                                        "rate_first_test_neg_fem_under_25", 
#                                                        new_values = 0.2877 * base_rate_reduction, 
#                                                        starting_year = pitc_reduction_year)
#   }
#   rollout <- convert_to_thembisa_format(formatted_data2, data, dictionary)
#   write(rollout, here("THEMBISAv18/Rollout.txt"))
#   ## compile and model
#   run_thembisa()
#   read_thembisa_scenario(output_names)
# }

read_thembisa_results <- function(pitc_reduction_years){
  filepaths <- paste0("results/scenario_", pitc_reduction_years, ".csv")
  temp <- lapply(filepaths, read.csv)
  names(temp) <- pitc_reduction_years
  baseline <- read.csv("results/baseline.csv")
  bind_rows(temp, .id = "pitc_reduction_year") %>% 
    dplyr::rename(intervention = value) %>% 
    dplyr::left_join(baseline) %>% 
    dplyr::rename(baseline = value) %>% 
    tidyr::pivot_longer(c(intervention, baseline), names_to = "scenario")
}

read_thembisa_results_sliding_scale <- function(pitc_reduction_years, pitc_reduction_percentage){
  for (i in 1: length(pitc_reduction_percentage)){
    if (i == 1){
      filepaths <- paste0("results/scenario_", pitc_reduction_years, "_", pitc_reduction_percentage[i], ".csv")
    }
    if (i > 1){
      filepaths <- append(filepaths, paste0("results/scenario_", pitc_reduction_years, "_", pitc_reduction_percentage[i], ".csv"))
    }
  }
  temp <- lapply(filepaths, read.csv)
  
  for (i in 1: length(pitc_reduction_percentage)){
    if (i == 1){
      temp_names <- paste0(pitc_reduction_years, "_", pitc_reduction_percentage[i])
    }
    if (i > 1){
      temp_names <- append(temp_names, paste0(pitc_reduction_years, "_", pitc_reduction_percentage[i]))
    }
  }
  names(temp) <- temp_names
  baseline <- read.csv("results/baseline.csv")
  bind_rows(temp, .id = "pitc_reduction_year") %>% 
    dplyr::rename(intervention = value) %>% 
    dplyr::left_join(baseline) %>% 
    dplyr::rename(baseline = value) %>% 
    separate(pitc_reduction_year, c("pitc_reduction_year", "test_reduction")) %>% 
    tidyr::pivot_longer(c(intervention, baseline), names_to = "scenario")
}

calc_cumulative <- function(start_year, follow_up_years, df){
  end_year <- start_year + follow_up_years
  df %>% filter(indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
                scenario != "percent_change", indicator != "ARTcoverageAdult",
                indicator != "CondomUsage", indicator !="FSWCondomUsage", 
                indicator !="NonFSWCondomUsage", indicator !="PctProtSexActsSW", 
                indicator !="PctSexActsSW", indicator!= "HIVinc15to49",
                year >= start_year,
                year <= end_year,
                pitc_reduction_year == start_year)  %>% 
    group_by(indicator, pitc_reduction_year, scenario, parameter_set, test_reduction) %>% 
    summarise(cumulative = sum(value))
}

calc_all_cumulatives <- function(pitc_reduction_years, follow_up_years, df){
  cumulatives <- lapply(pitc_reduction_years, calc_cumulative, follow_up_years, df)
  names(cumulatives) <- pitc_reduction_years
  all_cumulatives <- bind_rows(cumulatives, .id = "pitc_reduction_year")
}


