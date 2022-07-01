library(dplyr)
library(ggplot2)

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
  output <- read.delim(output_txt, header=FALSE, row.names = 1)
  names(output)[2:87] <- seq(1985, 2070)
  # names(output)[1] <- "Simulation"
  output <- output %>% select(-V2) 
  t_output <- as.data.frame(t(output))
  output <- as.data.frame(as_tibble(t_output, rownames = "Year"))
  output$Year <- as.numeric(output$Year)
  names(output)[2] <- "Simulation_1"
  return(output)
}

# Plots the outputs over time 
plot_outputs <- function(output, output_name, title_of_plot) {
  ggplot(output, aes(Year, Simulation_1)) + 
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

