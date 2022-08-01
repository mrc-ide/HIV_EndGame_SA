library(dplyr)
library(ggplot2)
library(gridExtra)

# Compiles and runs the Thembisa model
run_thembisa <- function(){
  setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA/THEMBISAv18")
  system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")
  system("./thembisa")
  setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA")
}

# Read output from Thembisa and assign column headers
read_output <- function(output_name){
  # Import output txt
  output_txt <- paste(output_name, "txt", sep = ".")
  output_txt <- paste("THEMBISAv18", output_txt, sep = "/")
  output <- read.delim(output_txt, header=FALSE, row.names = 1)
  names(output)[2:87] <- seq(1985, 2070)
  # names(output)[1] <- "Simulation"
  output <- output %>% select(-V2) 
  t_output <- as.data.frame(t(output))
  output <- as.data.frame(as_tibble(t_output, rownames = "Year"))
  output$Year <- as.numeric(output$Year)
  names(output)[2] <- "Simulation_1"
  return(output$Simulation_1)
}

edit_formatted_data <- function(parameter_name, new_values, starting_year=1985, final_year=2070){
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


## create outputs_df 
## output_names needs to be a vector/list of outputs as characters

create_outputs_df <- function(output_parameter_names){
  years <- formatted_data[['year']]
  ## Make dataframe with same number of rows as years
  outputs_df <- data.frame(years)
  # calculate number of scenarios if there are 10 per output
  
  n_outputs <- length(output_parameter_names)
  n_scenarios <- n_outputs * 10 + 1
  
  ## Add columns for all the outputs
  outputs_df[2:n_scenarios] <- rep(NA, 86)
  
  ## Empty list for each output name
  col_names <- rep(NA, n_scenarios)
  
  # make a vector of the scenarios including baseline
  # reveresed so its in the right order in the next loop 
  five_years <- c("2065", "2060", "2055", "2050", "2045", "2040", "2035", "2030",
                  "2025", "baseline")
  # makes a list of all the scenario namnes 
  for (j in 1:length(five_years)){
    for (i in 1:n_outputs){
      col_names[((i+1)+(i*9)-j)+1] <- paste(output_parameter_names[i], five_years[j], sep = "_")
    }
  }
  # assigns scenario names to columns in dataframe
  for (i in 2:n_scenarios){
    names(outputs_df)[i] <- col_names[i]
  }
  return(outputs_df)
}

save_baseline_outputs <- function(output_parameter_names = output_names, df = outputs_df){
  #### Outputs with baseline testing rates ####
  ## read in input parameter file
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  n_outputs <- length(output_names)
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  ## compile and model
  run_thembisa()
  ## Read all baseline outputs to columns 
  # this gives a warning but it works
  for (i in 1:n_outputs-1){
    outputs_df[2] <- read_output(output_names[1])
    outputs_df[2+i*10] <- read_output(output_names[i+1])
  }
  return(outputs_df)
}

change_testing_rate <- function(output_parameter_names = output_names, df = outputs_df){
  #### Output with changed testing rates ####
  ## loop for each 5-year interval between 2025 and 2065
  ## make new output for each year intervention implemented
  for (i in 1:9){
    data <- readLines("THEMBISAv18/Rollout_Original.txt")
    formatted_data <- format_data(data, dictionary)
    formatted_data <- edit_formatted_data("rate_first_test_neg_fem_under_25", 
                                          new_values = 0.0, starting_year = (2020+(i*5)))
    rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
    write(rollout, "THEMBISAv18/Rollout.txt")
    run_thembisa()
    n_outputs <- length(output_names)
    for (j in 1:n_outputs-1){
      outputs_df[2+i] <- read_output(output_names[1])
      outputs_df[2+j*10+i] <- read_output(output_names[j+1])
    }
  }
  return(outputs_df)
}  

make_long_df <- function(df_csv_name = "outputs_df.csv"){
  
  #### changing format of df to be used with facet_wrap ####
  
  df <- read_csv(df_csv_name)
  
  df_long <- df %>%
    pivot_longer(-years,
                 names_to = c("indicator", "intervention_year"),
                 names_pattern = "(.+)_(baseline|20[0-9]{2})")
  
  #' create a 'baseline' entry for each intervention year
  #' 
  
  df_long_facet <- df_long %>%
    pivot_wider(names_from = "intervention_year", values_from = "value") %>%
    pivot_longer(`2025`:last_col(), names_to = "intervention_year", values_to = "intervention") %>%
    pivot_longer(c(baseline, intervention), names_to = "scenario")
  
  return(df_long_facet)
}

# Plots the outputs over time 
plot_outputs <- function(df = df, output_name, title_of_plot = output_name, value=value) {
  df %>%
    filter(
      indicator == output_name,
      years >= 2020
    ) %>%
    ggplot(aes(years, value, color = scenario)) +
    geom_line() + ggtitle(title_of_plot) +
    facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw() -> plot
  return(plot)
}

plot_pct_trend <- function(df=intervention_only, output_name, title_of_plot = output_name, 
                           percent_change = percent_change) {
  df %>%
    filter(
      indicator == output_name,
      years >= 2020
    ) %>%
    ggplot(aes(years, percent_change)) +
    geom_line() + ggtitle(title_of_plot) + ylab("Change from baseline (%)") +
    facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()
}


