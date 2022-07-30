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

# names of all the outputs of interest

output_names <- c("TotalHIVtests", "TotalNewHIV", "LYlostAIDS", 
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", 
                  "DiagnosedHIV_M", "DiagnosedHIV_F", 
                  "Number1stHIVtestsPos", "StartingARTtot", "Prop1stHIVtestsPos", 
                  "NewDiagnosesPregnancy")

# create empty dataframe

outputs_df <- create_outputs_df(output_names)

# run and save baseline outputs

outputs_df <- save_baseline_outputs()

# change testing rates and save them 

outputs_df <- change_testing_rate()

## Calculate total AIDS related mortality in Adults 

outputs_df$TotalAIDSdeathsAdult_baseline <- outputs_df$AIDSdeathsAdultF_baseline + 
  outputs_df$AIDSdeathsAdultM_baseline
outputs_df$TotalAIDSdeathsAdult_2025 <- outputs_df$AIDSdeathsAdultF_2025 + 
  outputs_df$AIDSdeathsAdultM_2025
outputs_df$TotalAIDSdeathsAdult_2030 <- outputs_df$AIDSdeathsAdultF_2030 + 
  outputs_df$AIDSdeathsAdultM_2030
outputs_df$TotalAIDSdeathsAdult_2035 <- outputs_df$AIDSdeathsAdultF_2035 + 
  outputs_df$AIDSdeathsAdultM_2035
outputs_df$TotalAIDSdeathsAdult_2040 <- outputs_df$AIDSdeathsAdultF_2040 + 
  outputs_df$AIDSdeathsAdultM_2040
outputs_df$TotalAIDSdeathsAdult_2045 <- outputs_df$AIDSdeathsAdultF_2045 + 
  outputs_df$AIDSdeathsAdultM_2045
outputs_df$TotalAIDSdeathsAdult_2050 <- outputs_df$AIDSdeathsAdultF_2050 + 
  outputs_df$AIDSdeathsAdultM_2050
outputs_df$TotalAIDSdeathsAdult_2055 <- outputs_df$AIDSdeathsAdultF_2055 + 
  outputs_df$AIDSdeathsAdultM_2055
outputs_df$TotalAIDSdeathsAdult_2060 <- outputs_df$AIDSdeathsAdultF_2060 + 
  outputs_df$AIDSdeathsAdultM_2060
outputs_df$TotalAIDSdeathsAdult_2065 <- outputs_df$AIDSdeathsAdultF_2065 + 
  outputs_df$AIDSdeathsAdultM_2065


### write csv of outputs_df ####

write.csv(outputs_df, "outputs_df.csv", row.names = FALSE)

# make long format pivoted df

df <- make_long_df()

# plot outputs 
# for some reason I can't loop over these

plot_outputs(df, output_names[1])
plot_outputs(df, output_names[2])
plot_outputs(df, output_names[3])
plot_outputs(df, output_names[4])
plot_outputs(df, output_names[5])
plot_outputs(df, output_names[6])
plot_outputs(df, output_names[7])
plot_outputs(df, output_names[8])
plot_outputs(df, output_names[9])
plot_outputs(df, output_names[10])
plot_outputs(df, output_names[11])
plot_outputs(df, output_name = "TotalAIDSdeathsAdult")

## cumulative outputs

# make a dataframe of the cumulative results 
# start with the baseline results from 2025 to 2045 

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2025,
    years <= 2025+20, 
    scenario == "baseline",
    intervention_year == 2025
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs

# then baseline results from 2030 to 2050 

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2030,
    years <= 2030+20, 
    scenario == "baseline",
    intervention_year == 2030
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2030

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2030))

# baseline results from 2035 to 2055

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2035,
    years <= 2035+20, 
    scenario == "baseline",
    intervention_year == 2035
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2035

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2035))

# baseline results from 2040 to 2060

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2040,
    years <= 2040+20, 
    scenario == "baseline",
    intervention_year == 2040
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2040

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2040))


# baseline results from 2045 to 2065

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2045,
    years <= 2045+20, 
    scenario == "baseline",
    intervention_year == 2045
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2045

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2045))


# baseline results from 2050 to 2070

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2050,
    years <= 2050+20, 
    scenario == "baseline",
    intervention_year == 2050
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2050

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2050))

# make a new a new dataframe of the cumulative results of the 2025 intervention

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2025,
    years <= 2025+20, 
    scenario == "intervention",
    intervention_year == 2025
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2025
# add that to the bottom of the baseline 
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_2025))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2030,
    years <= 2030+20, 
    scenario == "intervention",
    intervention_year == 2030
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2030

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2030))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2035,
    years <= 2035+20, 
    scenario == "intervention",
    intervention_year == 2035
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2035

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2035))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2040,
    years <= 2040+20, 
    scenario == "intervention",
    intervention_year == 2040,
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2040

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2040))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2045,
    years <= 2045+20, 
    scenario == "intervention",
    intervention_year == 2045
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2045

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2045))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    years >= 2050,
    years <= 2050+20, 
    scenario == "intervention",
    intervention_year == 2050
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2050

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2050))

# plot all in one grid

cumulative_outputs %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM") %>% 
  ggplot(aes(intervention_year, cumulative, color = scenario)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Cumulative Value")

# plot each individually

# cumulative_outputs %>% 
#   filter(
#     indicator == "TotalNewHIV",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("TotalNewHIV") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "TotalHIV",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("TotalHIV") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "TotalHIVtests",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("TotalHIVtests")
# 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "LYlostAIDS",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("LYlostAIDS") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "AIDSdeathsAdultF",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("AIDSdeathsAdultF") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "AIDSdeathsAdultM",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("AIDSdeathsAdultM") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "DiagnosedHIV_F",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("DiagnosedHIV_F")
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "DiagnosedHIV_M",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("DiagnosedHIV_M")
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "UndiagnosedHIV_F",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("UndiagnosedHIV_F")
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "UndiagnosedHIV_M",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("UndiagnosedHIV_M")



