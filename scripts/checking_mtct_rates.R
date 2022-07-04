#### Set working directory ####

setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA")

#### Load packages/functions ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

source("R/support_modify_inputs.R")
source("scripts/modify_rollout.R")
source("R/read_and_run.R")

#### Make empty dataframe for outputs ####

### Make empty dataframe 
## Make years column
years <- formatted_data[['year']]
## Make dataframe with same number of rows as years
outputs_df <- data.frame(years)

# names of all the outputs of interest

output_names <- c("MTCTrateAtBirth", "MTCTrateBirthDiag")

# calculate number of scenarios if there are 10 per output

n_outputs <- length(output_names)
n_scenarios <- n_outputs * 10 + 1

## Add columns for all the outputs
outputs_df[2:n_scenarios] <- rep(NA, 86)

## Empty list for each output name
col_names <- rep(NA, n_scenarios)

# make a vector of the scenarios including baseline
# reversed so its in the right order in the next loop 
five_years <- c("2065", "2060", "2055", "2050", "2045", "2040", "2035", "2030",
                "2025", "baseline")
# makes a list of all the scenario names 
for (j in 1:length(five_years)){
  for (i in 1:n_outputs){
    col_names[((i+1)+(i*9)-j)+1] <- paste(output_names[i], five_years[j], sep = "_")
  }
}
# assigns scenario names to columns in dataframe
for (i in 2:n_scenarios){
  names(outputs_df)[i] <- col_names[i]
}

#### Outputs with baseline testing rates ####
## read in input parameter file
data <- readLines("THEMBISAv18/Rollout_Original.txt")
## write unedited input parameter file
formatted_data <- format_data(data, dictionary)
rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
write(rollout, "THEMBISAv18/Rollout.txt")
## compile and model
run_thembisa()
## Read all baseline outputs to columns 
# this gives a warning but it works
for (i in 0:n_outputs){
  outputs_df[2+i*10] <- read_output(output_names[i+1])
}

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
  outputs_df[2+i] <- read_output("MTCTrateAtBirth")
  outputs_df[12+i] <- read_output("MTCTrateBirthDiag")
}

### write csv of outputs_df ####

write.csv(outputs_df, "MTCTrateAtBirth.csv", row.names = FALSE)

#### changing format of df to be used with facet_wrap ####

df <- read_csv("MTCTrateAtBirth.csv")

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

# make plots of ANC / preganancy related outputs in reduced testing scenarios
df_long_facet %>%
  filter(
    indicator == output_names[1],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[1]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[2],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[2]) +
  facet_wrap(~intervention_year)
