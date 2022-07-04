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

# Plots the outputs over time 
plot_outputs <- function(output_df = output_df, output_column, output_name, title_of_plot) {
  ggplot(output_df, aes(Year, output_column)) + 
    geom_line() + 
    theme_bw() + 
    ggtitle(title_of_plot) + 
    ylab(as.character(output_name))
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


  
  
