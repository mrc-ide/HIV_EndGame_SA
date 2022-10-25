#### Load packages/functions ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("R/support_modify_inputs.R"))
source(here("scripts/modify_rollout.R"))
source(here("R/read_and_run.R"))

#### Make empty dataframe for outputs ####

# names of all the outputs of interest

output_names <- c("TotalHIVtests", "NewAdultHIV",
                  "LYlostAIDS", 
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", 
                  "DiagnosedHIV_M", "DiagnosedHIV_F", 
                  "Number1stHIVtestsPos", "StartingARTtot", "Prop1stHIVtestsPos", 
                  "NewDiagnosesPregnancy", "TotalART15F", "TotalART15M", 
                  "RediagnosesPregnancy", "TotANCtests", "VLsuppressed", "TotHIV15")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# establish intervention years
intervention_years <- seq(2025, 2050, 5)

#### 100% reduction ####
# proportion of base rate by which to reduce
base_rate_reduction = 0.00

# run baseline model
baseline <- run_thembisa_scenario(NA, output_names, base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names, base_rate_reduction = base_rate_reduction)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

# make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)

# calculate total aids-related deaths 

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

# calculate percentage from baseline for trends

df <- df %>%
  pivot_wider(names_from = scenario) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(intervention_year:indicator), names_to = "scenario")


# save results 
write.csv(df, "results/100_pct_red_df.csv", row.names = FALSE)
df <- read.csv("results/100_pct_red_df.csv")

# Cumulative values over 20 years #

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario")

cumulative_values_100_pct_red <- cumulative_values
write_csv(cumulative_values_100_pct_red, "results/cumulative_values_100_pct_red.csv")

# testing
plot_outputs_with_uncertainty(output_names[1]) + ggtitle ("100% testing reduction") + scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
plot_pct_chg_uncertainty(output_names[1]) + ggtitle("Total HIV tests in adults")
# new infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("100% testing reduction") + ylab ("New HIV infections")
plot_pct_chg_uncertainty(output_names[2]) + ggtitle ("New HIV infections")
# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("100% testing reduction") + ylab ("AIDS-related deaths")
plot_pct_chg_uncertainty("TotalAIDSdeathsadult") + ggtitle ("Adult AIDS-related deaths")

cumulative_values_100_pct_red %>% filter(
  scenario != "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = scenario)) +
  geom_point(aes(color = scenario), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
  xlab("Years") + ylab("Cumulative value") +
  facet_wrap(~indicator, scales = "free_y", 
             labeller = labeller(indicator = c(
               "LYlostAIDS" = "Life-years lost to AIDS", 
               "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
               "Number1stHIVtestsPos" = "First-time diagnoses",
               "RediagnosesPregnancy" = "Re-diagnoses at ANC",
               "StartingARTtot" = "ART inititation",
               "TotalAIDSdeathsadult" = "AIDS-related deaths in adults",
               "TotalHIVtests" = "Total HIV tests",
               "TotalNewHIV" = "New HIV infections",
               "TotANCtests" = "HIV tests at ANC",
               "ARTinititationRatio" = "ART initation : New diagnoses",
               "ARTInitPerNewInfection" = "ART initation : New infections",
               "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
               "NewDiagPerInfection" = "New diagnosis : New infections",
               "Pct1stHIVTestPos" = "Percentage first test positive",
               "PctANCTestPos" = "Percentage first test positive at ANC",
               "TotalARTratio" = "Total ART : Total HIV diagnoses",
               "NewAdultHIV" = "New HIV infections in adults")),nrow = 3) + 
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12))+ 
  ggtitle("100% testing reduction") + scale_y_continuous("Cumulative values (millions)", labels = (function(l) {round(l/1e6,1)})) 



#### 75% reduction ####
# proportion of base rate by which to reduce
base_rate_reduction = 0.25

# run baseline model
baseline <- run_thembisa_scenario(NA, output_names, base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names, base_rate_reduction = base_rate_reduction)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

# make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)

# calculate total aids-related deaths 

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

# calculate percentage from baseline for trends

df <- df %>%
  pivot_wider(names_from = scenario) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(intervention_year:indicator), names_to = "scenario")


# save results 
write.csv(df, "results/75_pct_red_df.csv", row.names = FALSE)
df <- read.csv("results/75_pct_red_df.csv")

# Cumulative values over 20 years #

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario")

cumulative_values_75_pct_red <- cumulative_values
write_csv(cumulative_values_75_pct_red, "results/cumulative_values_75_pct_red.csv")

# testing
plot_outputs_with_uncertainty(output_names[1]) + ggtitle ("75% testing reduction") + scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
plot_pct_chg_uncertainty(output_names[1]) + ggtitle("Total HIV tests in adults")
# new infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("75% testing reduction") + ylab ("New HIV infections")
plot_pct_chg_uncertainty(output_names[2]) + ggtitle ("New HIV infections")
# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("75% testing reduction") + ylab ("AIDS-related deaths")
plot_pct_chg_uncertainty("TotalAIDSdeathsadult") + ggtitle ("Adult AIDS-related deaths")

cumulative_values_75_pct_red %>% filter(
  scenario != "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = scenario)) +
  geom_point(aes(color = scenario), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
  xlab("Years") + ylab("Cumulative value") +
  facet_wrap(~indicator, scales = "free_y", 
             labeller = labeller(indicator = c(
               "LYlostAIDS" = "Life-years lost to AIDS", 
               "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
               "Number1stHIVtestsPos" = "First-time diagnoses",
               "RediagnosesPregnancy" = "Re-diagnoses at ANC",
               "StartingARTtot" = "ART inititation",
               "TotalAIDSdeathsadult" = "AIDS-related deaths in adults",
               "TotalHIVtests" = "Total HIV tests",
               "TotalNewHIV" = "New HIV infections",
               "TotANCtests" = "HIV tests at ANC",
               "ARTinititationRatio" = "ART initation : New diagnoses",
               "ARTInitPerNewInfection" = "ART initation : New infections",
               "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
               "NewDiagPerInfection" = "New diagnosis : New infections",
               "Pct1stHIVTestPos" = "Percentage first test positive",
               "PctANCTestPos" = "Percentage first test positive at ANC",
               "TotalARTratio" = "Total ART : Total HIV diagnoses",
               "NewAdultHIV" = "New HIV infections in adults")),nrow = 3) + 
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) + 
  ggtitle("75% testing reduction") + scale_y_continuous("Cumulative values (millions)", labels = (function(l) {round(l/1e6,1)})) 


#### 50% reduction ####
# proportion of base rate by which to reduce
base_rate_reduction = 0.50

# run baseline model
baseline <- run_thembisa_scenario(NA, output_names, base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names, base_rate_reduction = base_rate_reduction)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

# make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)

# calculate total aids-related deaths 

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

# calculate percentage from baseline for trends

df <- df %>%
  pivot_wider(names_from = scenario) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(intervention_year:indicator), names_to = "scenario")


# save results 
write.csv(df, "results/50_pct_red_df.csv", row.names = FALSE)
df <- read.csv("results/50_pct_red_df.csv")

# Cumulative values over 20 years #

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario")

cumulative_values_50_pct_red <- cumulative_values
write_csv(cumulative_values_50_pct_red, "results/cumulative_values_50_pct_red.csv")

# testing
plot_outputs_with_uncertainty(output_names[1]) + ggtitle ("50% testing reduction") + scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
plot_pct_chg_uncertainty(output_names[1]) + ggtitle("Total HIV tests in adults")
# new infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("50% testing reduction") + ylab ("New HIV infections")
plot_pct_chg_uncertainty(output_names[2]) + ggtitle ("New HIV infections")
# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("50% testing reduction") + ylab ("AIDS-related deaths")
plot_pct_chg_uncertainty("TotalAIDSdeathsadult") + ggtitle ("Adult AIDS-related deaths")

cumulative_values_50_pct_red %>% filter(
  scenario != "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = scenario)) +
  geom_point(aes(color = scenario), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
  xlab("Years") + ylab("Cumulative value") +
  facet_wrap(~indicator, scales = "free_y", 
             labeller = labeller(indicator = c(
               "LYlostAIDS" = "Life-years lost to AIDS", 
               "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
               "Number1stHIVtestsPos" = "First-time diagnoses",
               "RediagnosesPregnancy" = "Re-diagnoses at ANC",
               "StartingARTtot" = "ART inititation",
               "TotalAIDSdeathsadult" = "AIDS-related deaths in adults",
               "TotalHIVtests" = "Total HIV tests",
               "TotalNewHIV" = "New HIV infections",
               "TotANCtests" = "HIV tests at ANC",
               "ARTinititationRatio" = "ART initation : New diagnoses",
               "ARTInitPerNewInfection" = "ART initation : New infections",
               "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
               "NewDiagPerInfection" = "New diagnosis : New infections",
               "Pct1stHIVTestPos" = "Percentage first test positive",
               "PctANCTestPos" = "Percentage first test positive at ANC",
               "TotalARTratio" = "Total ART : Total HIV diagnoses",
               "NewAdultHIV" = "New HIV infections in adults")),nrow = 3) + 
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12))+ 
  ggtitle("50% testing reduction") + scale_y_continuous("Cumulative values (millions)", labels = (function(l) {round(l/1e6,1)})) 


#### 25% reduction ####
# proportion of base rate by which to reduce
base_rate_reduction = 0.75

# run baseline model
baseline <- run_thembisa_scenario(NA, output_names, base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names, base_rate_reduction = base_rate_reduction)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

# make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)

# calculate total aids-related deaths 

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

# calculate percentage from baseline for trends

df <- df %>%
  pivot_wider(names_from = scenario) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(intervention_year:indicator), names_to = "scenario")


# save results 
write.csv(df, "results/25_pct_red_df.csv", row.names = FALSE)
df <- read.csv("results/25_pct_red_df.csv")

# Cumulative values over 20 years #

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario")

cumulative_values_25_pct_red <- cumulative_values
write_csv(cumulative_values_25_pct_red, "results/cumulative_values_25_pct_red.csv")

# testing
plot_outputs_with_uncertainty(output_names[1]) + ggtitle ("25% testing reduction") + scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
plot_pct_chg_uncertainty(output_names[1]) + ggtitle("Total HIV tests in adults")
# new infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("25% testing reduction") + ylab ("New HIV infections")
plot_pct_chg_uncertainty(output_names[2]) + ggtitle ("New HIV infections")
# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("25% testing reduction") + ylab ("AIDS-related deaths")
plot_pct_chg_uncertainty("TotalAIDSdeathsadult") + ggtitle ("Adult AIDS-related deaths")

cumulative_values_25_pct_red %>% filter(
  scenario != "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = scenario)) +
  geom_point(aes(color = scenario), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
  xlab("Years") + ylab("Cumulative value") +
  facet_wrap(~indicator, scales = "free_y", 
             labeller = labeller(indicator = c(
               "LYlostAIDS" = "Life-years lost to AIDS", 
               "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
               "Number1stHIVtestsPos" = "First-time diagnoses",
               "RediagnosesPregnancy" = "Re-diagnoses at ANC",
               "StartingARTtot" = "ART inititation",
               "TotalAIDSdeathsadult" = "AIDS-related deaths in adults",
               "TotalHIVtests" = "Total HIV tests",
               "TotalNewHIV" = "New HIV infections",
               "TotANCtests" = "HIV tests at ANC",
               "ARTinititationRatio" = "ART initation : New diagnoses",
               "ARTInitPerNewInfection" = "ART initation : New infections",
               "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
               "NewDiagPerInfection" = "New diagnosis : New infections",
               "Pct1stHIVTestPos" = "Percentage first test positive",
               "PctANCTestPos" = "Percentage first test positive at ANC",
               "TotalARTratio" = "Total ART : Total HIV diagnoses",
               "NewAdultHIV" = "New HIV infections in adults")),nrow = 3) + 
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) + 
  ggtitle("25% testing reduction") + scale_y_continuous("Cumulative values (millions)", labels = (function(l) {round(l/1e6,1)})) 
