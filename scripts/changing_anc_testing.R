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

output_names <- c("TotalHIVtests", "BirthsDiagHIV", "TotBirthsHIV", 
                  "TotBirthDiagnosed", "VertTransmKnownPos", 
                  "PrevPreg15to49")

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
  outputs_df[2+i] <- read_output("TotalHIVtests")
  outputs_df[12+i] <- read_output("BirthsDiagHIV")
  outputs_df[22+i] <- read_output("TotBirthsHIV")
  outputs_df[32+i] <- read_output("TotBirthDiagnosed")
  outputs_df[42+i] <- read_output("VertTransmKnownPos")
  outputs_df[52+i] <- read_output("PrevPreg15to49")
}

## calculate percentage of vertical transmission from known pos of total HIV births

outputs_df <- outputs_df %>%
  mutate(pct_vert_trans_baseline = 
           VertTransmKnownPos_baseline/TotBirthsHIV_baseline * 100) %>% 
  mutate(pct_vert_trans_2025 = 
           VertTransmKnownPos_2025/TotBirthsHIV_2025 * 100) %>%
  mutate(pct_vert_trans_2030 = 
           VertTransmKnownPos_2030/TotBirthsHIV_2030 * 100) %>%
  mutate(pct_vert_trans_2035 = 
           VertTransmKnownPos_2035/TotBirthsHIV_2035 * 100) %>%
  mutate(pct_vert_trans_2040 = 
           VertTransmKnownPos_2040/TotBirthsHIV_2040 * 100) %>%
  mutate(pct_vert_trans_2045 = 
           VertTransmKnownPos_2045/TotBirthsHIV_2045 * 100) %>%
  mutate(pct_vert_trans_2050 = 
           VertTransmKnownPos_2050/TotBirthsHIV_2050 * 100) %>%
  mutate(pct_vert_trans_2055 = 
           VertTransmKnownPos_2055/TotBirthsHIV_2055 * 100) %>%
  mutate(pct_vert_trans_2060 = 
           VertTransmKnownPos_2060/TotBirthsHIV_2060 * 100) %>%
  mutate(pct_vert_trans_2065 = 
           VertTransmKnownPos_2065/TotBirthsHIV_2065 * 100)

### write csv of outputs_df ####

write.csv(outputs_df, "anc_df.csv", row.names = FALSE)

#### changing format of df to be used with facet_wrap ####

df <- read_csv("anc_df.csv")

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

df_long_facet %>%
  filter(
    indicator == output_names[3],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[3]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[4],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[4]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[5],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[5]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[6],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[6]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == "pct_vert_trans",
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle("% vertical transmission from known positive pregnant women of total hiv+ births") +
  facet_wrap(~intervention_year)

#### changing Annual rate of initiation into regular testing: pregnant women to 0.2877 ####

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
  formatted_data <- edit_formatted_data("rate_reg_test_preg_women", 
                                        new_values = 0.2877, starting_year = (2020+(i*5)))
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  run_thembisa()
  outputs_df[2+i] <- read_output("TotalHIVtests")
  outputs_df[12+i] <- read_output("BirthsDiagHIV")
  outputs_df[22+i] <- read_output("TotBirthsHIV")
  outputs_df[32+i] <- read_output("TotBirthDiagnosed")
  outputs_df[42+i] <- read_output("VertTransmKnownPos")
  outputs_df[52+i] <- read_output("PrevPreg15to49")
}

## calculate percentage of vertical transmission from known pos of total HIV births

outputs_df <- outputs_df %>%
  mutate(pct_vert_trans_baseline = 
           VertTransmKnownPos_baseline/TotBirthsHIV_baseline * 100) %>% 
  mutate(pct_vert_trans_2025 = 
           VertTransmKnownPos_2025/TotBirthsHIV_2025 * 100) %>%
  mutate(pct_vert_trans_2030 = 
           VertTransmKnownPos_2030/TotBirthsHIV_2030 * 100) %>%
  mutate(pct_vert_trans_2035 = 
           VertTransmKnownPos_2035/TotBirthsHIV_2035 * 100) %>%
  mutate(pct_vert_trans_2040 = 
           VertTransmKnownPos_2040/TotBirthsHIV_2040 * 100) %>%
  mutate(pct_vert_trans_2045 = 
           VertTransmKnownPos_2045/TotBirthsHIV_2045 * 100) %>%
  mutate(pct_vert_trans_2050 = 
           VertTransmKnownPos_2050/TotBirthsHIV_2050 * 100) %>%
  mutate(pct_vert_trans_2055 = 
           VertTransmKnownPos_2055/TotBirthsHIV_2055 * 100) %>%
  mutate(pct_vert_trans_2060 = 
           VertTransmKnownPos_2060/TotBirthsHIV_2060 * 100) %>%
  mutate(pct_vert_trans_2065 = 
           VertTransmKnownPos_2065/TotBirthsHIV_2065 * 100)

### write csv of outputs_df ####

write.csv(outputs_df, "changed_rate_reg_test_preg_women_only.csv", row.names = FALSE)

#### changing format of df to be used with facet_wrap ####

df <- read_csv("changed_rate_reg_test_preg_women_only.csv")

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

df_long_facet %>%
  filter(
    indicator == output_names[3],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[3]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[4],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[4]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[5],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[5]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[6],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[6]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == "pct_vert_trans",
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle("% vertical transmission from known positive pregnant women of total hiv+ births") +
  facet_wrap(~intervention_year)


#### Output with changed testing rates ####
## loop for each 5-year interval between 2025 and 2065
## make new output for each year intervention implemented
for (i in 1:9){
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  formatted_data <- format_data(data, dictionary)
  formatted_data <- edit_formatted_data("rate_first_test_neg_fem_under_25", 
                                        new_values = 0.0, starting_year = (2020+(i*5)))
  formatted_data <- edit_formatted_data("rate_reg_test_preg_women", 
                                        new_values = 0.2877, starting_year = (2020+(i*5)))
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  run_thembisa()
  outputs_df[2+i] <- read_output("TotalHIVtests")
  outputs_df[12+i] <- read_output("BirthsDiagHIV")
  outputs_df[22+i] <- read_output("TotBirthsHIV")
  outputs_df[32+i] <- read_output("TotBirthDiagnosed")
  outputs_df[42+i] <- read_output("VertTransmKnownPos")
  outputs_df[52+i] <- read_output("PrevPreg15to49")
}

## calculate percentage of vertical transmission from known pos of total HIV births

outputs_df <- outputs_df %>%
  mutate(pct_vert_trans_baseline = 
           VertTransmKnownPos_baseline/TotBirthsHIV_baseline * 100) %>% 
  mutate(pct_vert_trans_2025 = 
           VertTransmKnownPos_2025/TotBirthsHIV_2025 * 100) %>%
  mutate(pct_vert_trans_2030 = 
           VertTransmKnownPos_2030/TotBirthsHIV_2030 * 100) %>%
  mutate(pct_vert_trans_2035 = 
           VertTransmKnownPos_2035/TotBirthsHIV_2035 * 100) %>%
  mutate(pct_vert_trans_2040 = 
           VertTransmKnownPos_2040/TotBirthsHIV_2040 * 100) %>%
  mutate(pct_vert_trans_2045 = 
           VertTransmKnownPos_2045/TotBirthsHIV_2045 * 100) %>%
  mutate(pct_vert_trans_2050 = 
           VertTransmKnownPos_2050/TotBirthsHIV_2050 * 100) %>%
  mutate(pct_vert_trans_2055 = 
           VertTransmKnownPos_2055/TotBirthsHIV_2055 * 100) %>%
  mutate(pct_vert_trans_2060 = 
           VertTransmKnownPos_2060/TotBirthsHIV_2060 * 100) %>%
  mutate(pct_vert_trans_2065 = 
           VertTransmKnownPos_2065/TotBirthsHIV_2065 * 100)

outputs_df <- outputs_df %>%
  mutate(pct_hiv_births_diag_baseline = 
           TotBirthDiagnosed_baseline/TotBirthsHIV_baseline) %>% 
  mutate(pct_hiv_births_diag_2025 = 
           TotBirthDiagnosed_2025/TotBirthsHIV_2025) %>%
  mutate(pct_hiv_births_diag_2030 = 
           TotBirthDiagnosed_2030/TotBirthsHIV_2030) %>%
  mutate(pct_hiv_births_diag_2035 = 
           TotBirthDiagnosed_2035/TotBirthsHIV_2035) %>%
  mutate(pct_hiv_births_diag_2040 = 
           TotBirthDiagnosed_2040/TotBirthsHIV_2040) %>%
  mutate(pct_hiv_births_diag_2045 = 
           TotBirthDiagnosed_2045/TotBirthsHIV_2045) %>%
  mutate(pct_hiv_births_diag_2050 = 
           TotBirthDiagnosed_2050/TotBirthsHIV_2050) %>%
  mutate(pct_hiv_births_diag_2055 = 
           TotBirthDiagnosed_2055/TotBirthsHIV_2055) %>%
  mutate(pct_hiv_births_diag_2060 = 
           TotBirthDiagnosed_2060/TotBirthsHIV_2060) %>%
  mutate(pct_hiv_births_diag_2065 = 
           TotBirthDiagnosed_2065/TotBirthsHIV_2065)


### write csv of outputs_df ####

write.csv(outputs_df, "changed_rate_reg_test_preg_women_and_general.csv", row.names = FALSE)

#### changing format of df to be used with facet_wrap ####

df <- read_csv("changed_rate_reg_test_preg_women_and_general.csv")

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

df_long_facet %>%
  filter(
    indicator == output_names[3],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[3]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[4],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[4]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[5],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[5]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == output_names[6],
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle(output_names[6]) +
  facet_wrap(~intervention_year)

df_long_facet %>%
  filter(
    indicator == "pct_vert_trans",
    years >= 2020
  ) %>%
  ggplot(aes(years, value, color = scenario)) +
  geom_line() + ggtitle("% vertical transmission from known positive pregnant women of total hiv+ births") +
  facet_wrap(~intervention_year)
