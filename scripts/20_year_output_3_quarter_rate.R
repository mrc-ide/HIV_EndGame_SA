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

run_thembisa_scenario <- function(intervention_year, output_names){
  ## read in input parameter file
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  if (!is.na(intervention_year)){
    formatted_data <- edit_formatted_data("rate_first_test_neg_fem_under_25", 
                                          new_values = 0.14385, 
                                          starting_year = intervention_year)
  }
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  ## compile and model
  run_thembisa()
  read_thembisa_scenario(output_names)
}


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

dir.create("results_three_quarter_rate", FALSE, TRUE)

# establish intervention years
intervention_years <- seq(2025, 2050, 5)

# run baseline model
t1 <- Sys.time()
baseline <- run_thembisa_scenario(NA, output_names)

# save baseline outputs
write.csv(baseline, "results_three_quarter_rate/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names)
  write.csv(one_scenario, paste0("results_three_quarter_rate/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

# make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)
write_csv(df, "results_three_quarter_rate/all_scenarios.csv")
df <- read_csv("results_three_quarter_rate/all_scenarios.csv")

t2 <- Sys.time()
time_elapsed <- t2 - t1

## Calculate total AIDS related mortality in Adults 
## Calculate total ART in Adults 
## Calculate total number of diagnosed adults

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  mutate(TotalARTAdult = TotalART15F + TotalART15M) %>% 
  mutate(TotalDiagnosedHIV = DiagnosedHIV_F + DiagnosedHIV_M) %>%
  mutate(NewDiagPerInfection = Number1stHIVtestsPos/ NewAdultHIV) %>% 
  mutate(NewANCDiagPerInfection = NewDiagnosesPregnancy/NewAdultHIV) %>% 
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

# calculate test positivity for all diagnoses
# calculate test positivity for ANC diagnoses 
# calculate ratio of ART initiation to first time diagnoses
# calculate ratio of total numbers on ART to number of people diagnosed with HIV

df <- df %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(Pct1stHIVTestPos = (Number1stHIVtestsPos / TotalHIVtests) * 100) %>% 
  mutate(PctANCTestPos = (NewDiagnosesPregnancy /TotANCtests)*100 ) %>% 
  mutate(PropAdultsDiag = TotalDiagnosedHIV / TotHIV15) %>% 
  mutate(TotalARTratio = TotalARTAdult / TotalDiagnosedHIV) %>% 
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

df <- df %>% pivot_wider(names_from = c(indicator, scenario)) %>% 
  mutate(LifeYrsSaved_intervention = LYlostAIDS_baseline - LYlostAIDS_intervention) %>%
  mutate(TestEfficiencyLYS_intervention = (LifeYrsSaved_intervention / TotalHIVtests_intervention) *1000) %>% 
  mutate(InfectionsAverted_intervention = NewAdultHIV_baseline - NewAdultHIV_intervention) %>%
  mutate(TestEfficiencyIA_intervention = (InfectionsAverted_intervention / TotalHIVtests_intervention) *1000) %>% 
  pivot_longer(-(intervention_year:parameter_set), 
               names_to = c("indicator", "scenario"), 
               names_pattern = "(.+)_(baseline|intervention|percent_change)")

# calculate percentage from baseline for trends

df <- df %>%
  pivot_wider(names_from = scenario) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(intervention_year:indicator), names_to = "scenario") 

### write csv of df ####

write.csv(df, "results_three_quarter_rate/df.csv", row.names = FALSE)
df <- read.csv("results_three_quarter_rate/df.csv")

### plot yearly outputs #### 
# for some reason I can't loop over these

# testing
plot_outputs_with_uncertainty(output_names[1]) + ggtitle ("") + ylab("Number of HIV tests")

# epidemiologic outcomes
plot_outputs_with_uncertainty(output_names[2]) + ggtitle ("") + ylab ("New HIV infections") 
plot_outputs_with_uncertainty(output_names[3]) + ggtitle ("") + ylab ("Life-years lost")
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("") + ylab ("AIDS-related deaths")

# diagnoses 
plot_outputs_with_uncertainty(output_names[8]) + ggtitle ("") + ylab ("New HIV diagnoses")
plot_outputs_with_uncertainty(output_names[11]) + ggtitle ("") + ylab ("New HIV diagnoses")
plot_outputs_with_uncertainty("TotalDiagnosedHIV") + ggtitle ("") + ylab ("HIV diagnosed adults")

# test positivity

plot_outputs_with_uncertainty("Pct1stHIVTestPos") + ggtitle("") + ylab("Positive tests (%)")
plot_outputs_with_uncertainty("PctANCTestPos") + ggtitle("") + ylab("Positive tests (%)")

#diagnoses to infections 

plot_outputs_with_uncertainty("NewDiagPerInfection") + ggtitle("") + ylab("New HIV diagnoses : New HIV Infections")
plot_outputs_with_uncertainty("NewANCDiagPerInfection")+ ggtitle("") + ylab("New HIV diagnoses at ANC : New HIV Infections")

# 90-90-90

plot_outputs_with_uncertainty("PropAdultsDiag") + ggtitle("") + ylab("Proportion of adults with HIV")
plot_outputs_with_uncertainty("TotalARTratio") + ggtitle("") + ylab("Proportion of HIV diagnosed adults")
plot_outputs_with_uncertainty("VLsuppressed") + ggtitle("") + ylab("Proportion of adults on ART")

# test efficiency
df %>% filter(
  scenario != "percent_change",
  indicator == "LifeYrsSaved",
  scenario == "intervention",
  year >= 2020) %>% 
  group_by(year, scenario, intervention_year) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, median, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, group = scenario), alpha = 0.25) +
  geom_line(aes(color = scenario)) + 
  facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()

df %>% filter(
  scenario != "percent_change",
  indicator == "InfectionsAverted",
  scenario == "intervention",
  year >= 2020) %>% 
  group_by(year, scenario, intervention_year) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, median, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, group = scenario), alpha = 0.25) +
  geom_line(aes(color = scenario)) + 
  facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()

df %>% filter(
  scenario != "percent_change",
  indicator == "TestEfficiencyLYS",
  scenario == "intervention",
  year >= 2020) %>% 
  group_by(year, scenario, intervention_year) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, median, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, group = scenario), alpha = 0.25) +
  geom_line(aes(color = scenario)) + 
  facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()

df %>% filter(
  scenario != "percent_change",
  indicator == "TestEfficiencyIA",
  scenario == "intervention",
  year >= 2020) %>% 
  group_by(year, scenario, intervention_year) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, median, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, group = scenario), alpha = 0.25) +
  geom_line(aes(color = scenario)) + 
  facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()

# plot percentage change from baseline 

plot_pct_chg_uncertainty(output_names[1]) + ggtitle("Total HIV tests in adults")
plot_pct_chg_uncertainty(output_names[2]) + ggtitle ("New HIV infections")
plot_pct_chg_uncertainty(output_names[3]) + ggtitle ("Life-years lost to AIDS")
plot_pct_chg_uncertainty(output_names[4]) + ggtitle ("Adult male AIDS-related mortality")
plot_pct_chg_uncertainty(output_names[5]) + ggtitle ("Adult female AIDS-related mortality")
plot_pct_chg_uncertainty(output_names[6]) + ggtitle ("Total number of adult males with diagnosed HIV")
plot_pct_chg_uncertainty(output_names[7]) + ggtitle ("Total number of adult females with diagnosed HIV")
plot_pct_chg_uncertainty(output_names[8]) + ggtitle ("First-time HIV diagnoses")
plot_pct_chg_uncertainty(output_names[9]) + ggtitle ("Adults initiating ART")
plot_pct_chg_uncertainty(output_names[11]) + ggtitle ("First-time diagnoses at ANC")
plot_pct_chg_uncertainty(output_names[12]) + ggtitle ("Female adults on ART") 
plot_pct_chg_uncertainty(output_names[13]) + ggtitle ("Male adults on ART") 
plot_pct_chg_uncertainty(output_names[14]) + ggtitle ("Rediagnoses at ANC") 
plot_pct_chg_uncertainty(output_names[15]) + ggtitle ("HIV tests at ANC")
plot_pct_chg_uncertainty("TotalAIDSdeathsadult") + ggtitle ("Adult AIDS-related mortality")
plot_pct_chg_uncertainty("TotalARTAdult") + ggtitle ("Adults on ART")
plot_pct_chg_uncertainty("TotalDiagnosedHIV") + ggtitle ("Adults with diagnosed HIV")
plot_pct_chg_uncertainty("NewDiagPerInfection") + ggtitle("New HIV diagnoses : new infections")
plot_pct_chg_uncertainty("NewANCDiagPerInfection") + ggtitle("New HIV diagnoses at ANC : new infections")
plot_pct_chg_uncertainty("TotalARTratio")

# Cumulative values over 20 years ####

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario") 

 # test efficiency

cumulative_values %>% filter(
  scenario != "percent_change", 
  indicator == "TestEfficiencyLYS" |
    indicator == "TestEfficiencyIA",
  scenario == "intervention") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median)) +
  geom_point(shape = 15, color = "#00BFC4") +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), color = "#00BFC4", width = 0.1) +
  xlab("Years") + ylab("Test efficiency") +
  facet_wrap(~indicator, scales = "free_y", 
             labeller = labeller(indicator = c(
               "TestEfficiencyLYS" = "Life years saved per 1000 tests",
               "TestEfficiencyIA" = "Infections averted per 1000 tests",
               "LifeYrsSaved" = "Life years saved",
               "InfectionsAverted" = "Infections averted",
               "TotalHIVtests" = "HIV tests"))) + 
  expand_limits(y=0) + theme_bw()

cumulative_values %>% filter(
  scenario != "percent_change", 
  indicator == "TestEfficiencyLYS" |
    indicator == "TotalHIVtests",
  scenario == "intervention") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))

# plot all baseline and intervention for all outcomes in one plot

cumulative_values$indicator <- 
  factor(cumulative_values$indicator, 
         levels = c("TotalHIVtests", "NewAdultHIV",
                    "TotalAIDSdeathsadult", "LYlostAIDS",
                    "Number1stHIVtestsPos", "NewDiagnosesPregnancy",
                    "StartingARTtot", "TotANCtests", "TotalARTratio",
                    "Pct1stHIVTestPos", "PctANCTestPos","NewDiagPerInfection",
                    "NewANCDiagPerInfection",
                    "ARTinititationRatio", "ARTInitPerNewInfection"))

plot_cumulative_uncertainty()

# plot percentage change  in one grid

plot_cumulative_pct_chg()

# epidemiologic outcomes only 

plot_cumulative_epi_uncertainty(cumulative_values) + ggtitle("")

plot_cumulative_epi_pct_chg(cumulative_values) + ggtitle("")

epi_cumulative_value <- cumulative_values %>% filter(
  scenario != "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), 
            lower_CI = quantile(value, probs = 0.025),  
            upper_CI = quantile(value, probs = 0.975))
write_csv(epi_cumulative_value, "epi_cumulative_value.csv")

epi_pct_chg <- cumulative_values %>% filter(
  scenario == "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), 
            lower_CI = quantile(value, probs = 0.025),  
            upper_CI = quantile(value, probs = 0.975))
write_csv(epi_pct_chg, "epi_pct_chg.csv")

# cumulative new diagnoses

plot_cumulative_diag_uncertainty(cumulative_values)

plot_cumulative_diag_pct_chg(cumulative_values)

diag_cumulative <- cumulative_values %>% filter(
  scenario != "percent_change", 
  indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value),lower_CI = quantile(value, probs = 0.025), 
            upper_CI = quantile(value, probs = 0.975), 
  )

diag_cum_pct_chg <- cumulative_values %>% filter(
  scenario == "percent_change", 
  indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value),lower_CI = quantile(value, probs = 0.025), 
            upper_CI = quantile(value, probs = 0.975), 
  )

temp <- cumulative_values %>% filter(
  scenario != "percent_change", 
  indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value)/100000, lower_CI = quantile(value, probs = 0.025)/100000, 
            upper_CI = quantile(value, probs = 0.975)/100000, 
            lower_CI = quantile(value, probs = 0.025)/100000)


# relationship between HIV infections and  new diagnoses

cumulative_values %>% filter(
  scenario != "percent_change", 
  indicator == "NewDiagPerInfection") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = scenario)) +
  geom_point(aes(color = scenario), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
  xlab("Years")

cumulative_values %>% filter(
  scenario == "percent_change", 
  indicator == "NewDiagPerInfection") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = scenario)) +
  geom_point(aes(color = scenario), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
  xlab("Years")

# Cumulative values over 25 years ####

cumulative_values_25 <- calc_all_cumulatives(intervention_years, 25)

# calculate cumulative percent change from baseline 

cumulative_values_25 <- cumulative_values_25 %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario") 

# epidemiologic outcomes only 

plot_cumulative_epi_uncertainty(cumulative_values_25) + ggtitle("")

plot_cumulative_epi_pct_chg(cumulative_values_25) + ggtitle("")

epi_cumulative_value_25 <- cumulative_values_25 %>% filter(
  scenario != "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult", 
  intervention_year != 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), 
            lower_CI = quantile(value, probs = 0.025),  
            upper_CI = quantile(value, probs = 0.975))
write_csv(epi_cumulative_value_25, "epi_cumulative_value_25.csv")

epi_pct_chg_25 <- cumulative_values_25 %>% filter(
  scenario == "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult", 
  intervention_year != 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), 
            lower_CI = quantile(value, probs = 0.025),  
            upper_CI = quantile(value, probs = 0.975))
write_csv(epi_pct_chg, "epi_pct_chg.csv")

# cumulative new diagnoses

plot_cumulative_diag_uncertainty(cumulative_values_25)

plot_cumulative_diag_pct_chg(cumulative_values_25)

diag_cumulative_25 <- cumulative_values_25 %>% filter(
  scenario != "percent_change", 
  indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))

diag_cum_pct_chg_25 <- cumulative_values_25 %>% filter(
  scenario == "percent_change", 
  indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))


# Cumulative values over 30 years ####

cumulative_values_30 <- calc_all_cumulatives(intervention_years, 30)

# calculate cumulative percent change from baseline 

cumulative_values_30 <- cumulative_values_30 %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario") 

# epidemiologic outcomes only 

plot_cumulative_epi_uncertainty(cumulative_values_30) + ggtitle("")

plot_cumulative_epi_pct_chg(cumulative_values_30) + ggtitle("")

epi_cumulative_value_30 <- cumulative_values_30 %>% filter(
  scenario != "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), 
            lower_CI = quantile(value, probs = 0.025),  
            upper_CI = quantile(value, probs = 0.975))
write_csv(epi_cumulative_value, "epi_cumulative_value.csv")

epi_pct_chg_30 <- cumulative_values_30 %>% filter(
  scenario == "percent_change", 
  indicator == "NewAdultHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), 
            lower_CI = quantile(value, probs = 0.025),  
            upper_CI = quantile(value, probs = 0.975))
write_csv(epi_pct_chg, "epi_pct_chg.csv")

# cumulative new diagnoses

plot_cumulative_diag_uncertainty(cumulative_values_30)

plot_cumulative_diag_pct_chg(cumulative_values_30)

diag_cumulative_30 <- cumulative_values_30 %>% filter(
  scenario != "percent_change", 
  indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))

diag_cum_pct_chg_30 <- cumulative_values_30 %>% filter(
  scenario == "percent_change", 
  indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))
