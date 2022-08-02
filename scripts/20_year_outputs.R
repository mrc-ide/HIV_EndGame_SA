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
                  "NewDiagnosesPregnancy", "TotalART15F", "TotalART15M", 
                  "RediagnosesPregnancy", "TotANCtests")

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

## Calculate total ART in Adults 

outputs_df$TotalARTAdult_baseline <- outputs_df$TotalART15F_baseline + 
  outputs_df$TotalART15M_baseline
outputs_df$TotalARTAdult_2025 <- outputs_df$TotalART15F_2025 + 
  outputs_df$TotalART15M_2025
outputs_df$TotalARTAdult_2030 <- outputs_df$TotalART15F_2030 + 
  outputs_df$TotalART15M_2030
outputs_df$TotalARTAdult_2035 <- outputs_df$TotalART15F_2035 + 
  outputs_df$TotalART15M_2035
outputs_df$TotalARTAdult_2040 <- outputs_df$TotalART15F_2040 + 
  outputs_df$TotalART15M_2040
outputs_df$TotalARTAdult_2045 <- outputs_df$TotalART15F_2045 + 
  outputs_df$TotalART15M_2045
outputs_df$TotalARTAdult_2050 <- outputs_df$TotalART15F_2050 + 
  outputs_df$TotalART15M_2050
outputs_df$TotalARTAdult_2055 <- outputs_df$TotalART15F_2055 + 
  outputs_df$TotalART15M_2055
outputs_df$TotalARTAdult_2060 <- outputs_df$TotalART15F_2060 + 
  outputs_df$TotalART15M_2060
outputs_df$TotalARTAdult_2065 <- outputs_df$TotalART15F_2065 + 
  outputs_df$TotalART15M_2065

## Calculate total number of diagnosed adults

outputs_df$TotalDiagnosedHIV_baseline <- outputs_df$DiagnosedHIV_F_baseline + 
  outputs_df$DiagnosedHIV_M_baseline
outputs_df$TotalDiagnosedHIV_2025 <- outputs_df$DiagnosedHIV_F_2025 + 
  outputs_df$DiagnosedHIV_M_2025
outputs_df$TotalDiagnosedHIV_2030 <- outputs_df$DiagnosedHIV_F_2030 + 
  outputs_df$DiagnosedHIV_M_2030
outputs_df$TotalDiagnosedHIV_2035 <- outputs_df$DiagnosedHIV_F_2035 + 
  outputs_df$DiagnosedHIV_M_2035
outputs_df$TotalDiagnosedHIV_2040 <- outputs_df$DiagnosedHIV_F_2040 + 
  outputs_df$DiagnosedHIV_M_2040
outputs_df$TotalDiagnosedHIV_2045 <- outputs_df$DiagnosedHIV_F_2045 + 
  outputs_df$DiagnosedHIV_M_2045
outputs_df$TotalDiagnosedHIV_2050 <- outputs_df$DiagnosedHIV_F_2050 + 
  outputs_df$DiagnosedHIV_M_2050
outputs_df$TotalDiagnosedHIV_2055 <- outputs_df$DiagnosedHIV_F_2055 + 
  outputs_df$DiagnosedHIV_M_2055
outputs_df$TotalDiagnosedHIV_2060 <- outputs_df$DiagnosedHIV_F_2060 + 
  outputs_df$DiagnosedHIV_M_2060
outputs_df$TotalDiagnosedHIV_2065 <- outputs_df$DiagnosedHIV_F_2065 + 
  outputs_df$DiagnosedHIV_M_2065

# test positivity in the total population
# number of diagnoses / number of tests

outputs_df$TotalTestPositivity_baseline <- outputs_df$Number1stHIVtestsPos_baseline / 
  outputs_df$TotalHIVtests_baseline * 100
outputs_df$TotalTestPositivity_2025 <- outputs_df$Number1stHIVtestsPos_2025 / 
  outputs_df$TotalHIVtests_2025 * 100
outputs_df$TotalTestPositivity_2030 <- outputs_df$Number1stHIVtestsPos_2030 / 
  outputs_df$TotalHIVtests_2030 * 100
outputs_df$TotalTestPositivity_2035 <- outputs_df$Number1stHIVtestsPos_2035 / 
  outputs_df$TotalHIVtests_2035 * 100
outputs_df$TotalTestPositivity_2040 <- outputs_df$Number1stHIVtestsPos_2040 / 
  outputs_df$TotalHIVtests_2040 * 100
outputs_df$TotalTestPositivity_2045 <- outputs_df$Number1stHIVtestsPos_2045 / 
  outputs_df$TotalHIVtests_2045 * 100
outputs_df$TotalTestPositivity_2050 <- outputs_df$Number1stHIVtestsPos_2050 / 
  outputs_df$TotalHIVtests_2050 * 100
outputs_df$TotalTestPositivity_2055 <- outputs_df$Number1stHIVtestsPos_2055 / 
  outputs_df$TotalHIVtests_2055 * 100
outputs_df$TotalTestPositivity_2060 <- outputs_df$Number1stHIVtestsPos_2060 / 
  outputs_df$TotalHIVtests_2060 * 100
outputs_df$TotalTestPositivity_2065 <- outputs_df$Number1stHIVtestsPos_2065 / 
  outputs_df$TotalHIVtests_2065 * 100

# number of ANC diagnoses / number of ANC tests

outputs_df$ANCTestPositivity_baseline <- outputs_df$NewDiagnosesPregnancy_baseline / 
  outputs_df$TotANCtests_baseline * 100
outputs_df$ANCTestPositivity_2025 <- outputs_df$NewDiagnosesPregnancy_2025 / 
  outputs_df$TotANCtests_2025 * 100
outputs_df$ANCTestPositivity_2030 <- outputs_df$NewDiagnosesPregnancy_2030 / 
  outputs_df$TotANCtests_2030 * 100
outputs_df$ANCTestPositivity_2035 <- outputs_df$NewDiagnosesPregnancy_2035 / 
  outputs_df$TotANCtests_2035 * 100
outputs_df$ANCTestPositivity_2040 <- outputs_df$NewDiagnosesPregnancy_2040 / 
  outputs_df$TotANCtests_2040 * 100
outputs_df$ANCTestPositivity_2045 <- outputs_df$NewDiagnosesPregnancy_2045 / 
  outputs_df$TotANCtests_2045 * 100
outputs_df$ANCTestPositivity_2050 <- outputs_df$NewDiagnosesPregnancy_2050 / 
  outputs_df$TotANCtests_2050 * 100
outputs_df$ANCTestPositivity_2055 <- outputs_df$NewDiagnosesPregnancy_2055 / 
  outputs_df$TotANCtests_2055 * 100
outputs_df$ANCTestPositivity_2060 <- outputs_df$NewDiagnosesPregnancy_2060 / 
  outputs_df$TotANCtests_2060 * 100
outputs_df$ANCTestPositivity_2065 <- outputs_df$NewDiagnosesPregnancy_2065 / 
  outputs_df$TotANCtests_2065 * 100

# ratio of number of people who start ART to number of people diagnosed 

outputs_df$ARTinitiationRatio_baseline <- outputs_df$StartingARTtot_baseline / 
  outputs_df$Number1stHIVtestsPos_baseline 
outputs_df$ARTinitiationRatio_2025 <- outputs_df$StartingARTtot_2025 / 
  outputs_df$Number1stHIVtestsPos_2025 
outputs_df$ARTinitiationRatio_2030 <- outputs_df$StartingARTtot_2030 / 
  outputs_df$Number1stHIVtestsPos_2030 
outputs_df$ARTinitiationRatio_2035 <- outputs_df$StartingARTtot_2035 / 
  outputs_df$Number1stHIVtestsPos_2035
outputs_df$ARTinitiationRatio_2040 <- outputs_df$StartingARTtot_2040 / 
  outputs_df$Number1stHIVtestsPos_2040 
outputs_df$ARTinitiationRatio_2045 <- outputs_df$StartingARTtot_2045 / 
  outputs_df$Number1stHIVtestsPos_2045
outputs_df$ARTinitiationRatio_2050 <- outputs_df$StartingARTtot_2050 / 
  outputs_df$Number1stHIVtestsPos_2050
outputs_df$ARTinitiationRatio_2055 <- outputs_df$StartingARTtot_2055 / 
  outputs_df$Number1stHIVtestsPos_2055
outputs_df$ARTinitiationRatio_2060 <- outputs_df$StartingARTtot_2060 / 
  outputs_df$Number1stHIVtestsPos_2060
outputs_df$ARTinitiationRatio_2065 <- outputs_df$StartingARTtot_2065 / 
  outputs_df$Number1stHIVtestsPos_2065

# ratio of number of people who total number on ART to total people diagnosed

outputs_df$TotalARTRatio_baseline <- outputs_df$TotalARTAdult_baseline / 
  outputs_df$TotalDiagnosedHIV_baseline 
outputs_df$TotalARTRatio_2025 <- outputs_df$TotalARTAdult_2025 / 
  outputs_df$TotalDiagnosedHIV_2025 
outputs_df$TotalARTRatio_2030 <- outputs_df$TotalARTAdult_2030 / 
  outputs_df$TotalDiagnosedHIV_2030 
outputs_df$TotalARTRatio_2035 <- outputs_df$TotalARTAdult_2035 / 
  outputs_df$TotalDiagnosedHIV_2035
outputs_df$TotalARTRatio_2040 <- outputs_df$TotalARTAdult_2040 / 
  outputs_df$TotalDiagnosedHIV_2040 
outputs_df$TotalARTRatio_2045 <- outputs_df$TotalARTAdult_2045 / 
  outputs_df$TotalDiagnosedHIV_2045
outputs_df$TotalARTRatio_2050 <- outputs_df$TotalARTAdult_2050 / 
  outputs_df$TotalDiagnosedHIV_2050
outputs_df$TotalARTRatio_2055 <- outputs_df$TotalARTAdult_2055 / 
  outputs_df$TotalDiagnosedHIV_2055
outputs_df$TotalARTRatio_2060 <- outputs_df$TotalARTAdult_2060 / 
  outputs_df$TotalDiagnosedHIV_2060
outputs_df$TotalARTRatio_2065 <- outputs_df$TotalARTAdult_2065 / 
  outputs_df$TotalDiagnosedHIV_2065

### write csv of outputs_df ####

write.csv(outputs_df, "outputs_df.csv", row.names = FALSE)
outputs_df <- read.csv("outputs_df.csv")
# make long format pivoted df

df <- make_long_df()

# plot outputs 
# for some reason I can't loop over these

plot_outputs(output_names[1], title_of_plot = "Annual number of HIV tests performed", ylab = "HIV tests")
plot_outputs(output_names[2], title_of_plot = "Annual number of new HIV infections", ylab = "New HIV infections") 
plot_outputs(output_names[3], title_of_plot = "Annual life-years lost to AIDS", ylab = "Life-years lost")
plot_outputs(output_names[4], title_of_plot = "Annual adult male AIDS-related mortality", ylab = "AIDS-related deaths")
plot_outputs(output_names[5], title_of_plot = "Annual adult female AIDS-related mortality", ylab = "AIDS-related deaths")
plot_outputs(output_names[6], title_of_plot = "Total number of adult males with diagnosed HIV", ylab = "HIV diagnosed adults")
plot_outputs(output_names[7], title_of_plot = "Total number of adult females with diagnosed HIV", ylab = "HIV diagnosed adults")
plot_outputs(output_names[8], title_of_plot = "Annual number of adults with newly diagnosed HIV", ylab = "New HIV diagnoses")
plot_outputs(output_names[9], title_of_plot = "Annual number of adults initiating ART", ylab = "Adults initating ART")
plot_outputs(output_names[10], title_of_plot = "Proportion of HIV tests that are first-time positive", ylab = "Proportion")
plot_outputs(output_names[11], title_of_plot = "Annual number of first-time diagnoses at ANC", ylab = "New HIV diagnoses")
plot_outputs(output_names[12], title_of_plot = "Total number of female adults on ART", ylab = "Adults on ART")
plot_outputs(output_names[13], title_of_plot = "Total number of male adults on ART", ylab = "Adults on ART")
plot_outputs(output_names[14], title_of_plot = "Annual number of rediagnoses at ANC", ylab = "HIV re-diagnoses")
plot_outputs(output_names[15], title_of_plot = "Annual number of HIV tests performed at ANC", ylab = "HIV tests")
plot_outputs(output_name = "TotalAIDSdeathsAdult", title_of_plot = "Annual adult AIDS-related mortality", ylab = "AIDS-related deaths")
plot_outputs(output_name = "TotalARTAdult", title_of_plot = "Total number of adults on ART", ylab = "Adults on ART")
plot_outputs(output_name = "TotalDiagnosedHIV", title_of_plot = "Total number of adults with diagnosed HIV", ylab = "HIV diagnosed adults")
plot_outputs("TotalTestPositivity", title_of_plot = "Percentage of HIV tests that are first-time positive", ylab = "Percentage (%)")
plot_outputs("ANCTestPositivity", title_of_plot = "Percentage of HIV tests at ANC that are first-time positive", ylab = "Percentage (%)")
plot_outputs("ARTinitiationRatio", title_of_plot = "Ratio of ART initiation to new HIV diagnoses", ylab = "Ratio")
plot_outputs("TotalARTRatio", title_of_plot = "Ratio of total number on ART to total number with diagnosed HIV", ylab = "Ratio")

# calculate percentage from baseline for trends

baseline_only <- df %>% 
  filter(scenario == "baseline")
intervention_only <- df %>%
  filter(scenario == "intervention")
intervention_only$percent_change <- 
  ((intervention_only$value - baseline_only$value) / baseline_only$value) * 100

# plot percentage change from baseline 

plot_pct_trend(output_name = output_names[1])
plot_pct_trend(output_name = output_names[2])
plot_pct_trend(output_name = output_names[3])
plot_pct_trend(output_name = output_names[4])
plot_pct_trend(output_name = output_names[5])
plot_pct_trend(output_name = output_names[6])
plot_pct_trend(output_name = output_names[7])
plot_pct_trend(output_name = output_names[8])
plot_pct_trend(output_name = output_names[9])
plot_pct_trend(output_name = output_names[10])
plot_pct_trend(output_name = output_names[11])
plot_pct_trend(output_name = output_names[12])
plot_pct_trend(output_name = output_names[13])
plot_pct_trend(output_name = output_names[14])
plot_pct_trend(output_name = output_names[15])
plot_pct_trend(output_name = "TotalAIDSdeathsAdult")
plot_pct_trend(output_name = "TotalARTAdult")
plot_pct_trend(output_name = "TotalDiagnosedHIV")

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
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult") %>% 
  ggplot(aes(intervention_year, cumulative, color = scenario)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Cumulative Value")

# calculate cumulative percent change from baseline 

cumulative_baseline_only <- cumulative_outputs %>% 
  filter(scenario == "baseline")
cumulative_intervention_only <- cumulative_outputs %>%
  filter(scenario == "intervention")
cumulative_intervention_only$percent_change <- 
  ((cumulative_intervention_only$cumulative - cumulative_baseline_only$cumulative)
   / cumulative_baseline_only$cumulative) * 100

# plot percentage change  in one grid

cumulative_intervention_only %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult") %>% 
  ggplot(aes(intervention_year, percent_change)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Percentage of baseline (%)")

# epidemiologic outcomes only 

cumulative_intervention_only %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult", indicator != "NewDiagnosesPregnancy", 
         indicator != "Number1stHIVtestsPos", indicator != "RediagnosesPregnancy",
         indicator!= "StartingARTtot", indicator != "TotalHIVtests", 
         indicator != "TotANCtests") %>% 
  ggplot(aes(intervention_year, percent_change)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Change from baseline (%)")


# epidemiologic outcomes only 

cumulative_intervention_only %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult", indicator != "TotalNewHIV", 
         indicator != "LYlostAIDS", indicator != "TotalHIVtests", 
         indicator != "TotalAIDSdeathsAdult") %>% 
  ggplot(aes(intervention_year, percent_change)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Change from baseline (%)")



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



