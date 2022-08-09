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

# create empty folder for results

dir.create("results", FALSE, TRUE)

# establish intervention years
intervention_years <- seq(2025, 2050, 5)

# run baseline model
t1 <- Sys.time()
baseline <- run_thembisa_scenario(NA, output_names)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

# make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)
write_csv(df, "results/all_scenarios.csv")
df <- read_csv("results/all_scenarios.csv")

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
  mutate(NewDiagPerInfection = Number1stHIVtestsPos/ TotalNewHIV) %>% 
  mutate(NewANCDiagPerInfection = NewDiagnosesPregnancy/TotalNewHIV) %>% 
  mutate(ARTInitPerNewInfection = StartingARTtot / TotalNewHIV) %>% 
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

# calculate test positivity for all diagnoses
# calculate test positivity for ANC diagnoses 
# calculate ratio of ART initiation to first time diagnoses
# calculate ratio of total numbers on ART to number of people diagnosed with HIV

df <- df %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(Pct1stHIVTestPos = (Number1stHIVtestsPos / TotalHIVtests) * 100) %>% 
  mutate(PctANCTestPos = (NewDiagnosesPregnancy /TotANCtests)*100 ) %>% 
  mutate(ARTinititationRatio = StartingARTtot / Number1stHIVtestsPos) %>% 
  mutate(TotalARTratio = TotalARTAdult / TotalDiagnosedHIV) %>% 
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

df <- df %>% pivot_wider(names_from = c(indicator, scenario)) %>% 
  mutate(LifeYrsSaved_intervention = 
    LYlostAIDS_baseline - LYlostAIDS_intervention) %>%
  mutate(TestEfficiency_intervention = LifeYrsSaved_intervention / (TotalHIVtests_intervention *1000)) %>% 
  pivot_longer(-(intervention_year:parameter_set), 
               names_to = c("indicator", "scenario"), 
               names_pattern = "(.+)_(baseline|intervention|percent_change)")

# calculate percentage from baseline for trends

df <- df %>%
  pivot_wider(names_from = scenario) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(intervention_year:indicator), names_to = "scenario") 

### write csv of df ####

write.csv(df, "results/df.csv", row.names = FALSE)
df <- read.csv("results/df.csv")

### plot yearly outputs #### 
# for some reason I can't loop over these

plot_outputs_with_uncertainty(output_names[1]) + ggtitle("Total HIV tests") + ylab("Number of HIV tests")
plot_outputs_with_uncertainty(output_names[2]) + ggtitle ("New HIV infections") + ylab ("New HIV infections") 
plot_outputs_with_uncertainty(output_names[3]) + ggtitle ("Life-years lost to AIDS") + ylab ("Life-years lost")
plot_outputs_with_uncertainty(output_names[4]) + ggtitle ("Adult male AIDS-related mortality") + ylab ("AIDS-related deaths")
plot_outputs_with_uncertainty(output_names[5]) + ggtitle ("Adult female AIDS-related mortality") + ylab ("AIDS-related deaths")
plot_outputs_with_uncertainty(output_names[6]) + ggtitle ("Total number of adult males with diagnosed HIV") + ylab ("HIV diagnosed adults")
plot_outputs_with_uncertainty(output_names[7]) + ggtitle ("Total number of adult females with diagnosed HIV") + ylab ("HIV diagnosed adults")
plot_outputs_with_uncertainty(output_names[8]) + ggtitle ("First-time HIV diagnoses") + ylab ("New HIV diagnoses")
plot_outputs_with_uncertainty(output_names[9]) + ggtitle ("Adults initiating ART") + ylab ("Adults initating ART")
plot_outputs_with_uncertainty(output_names[10]) + ggtitle ("Proportion of HIV tests that are first-time positive") + ylab ("Proportion")
plot_outputs_with_uncertainty(output_names[11]) + ggtitle ("First-time diagnoses at ANC") + ylab ("New HIV diagnoses")
plot_outputs_with_uncertainty(output_names[12]) + ggtitle ("Female adults on ART") + ylab ("Adults on ART")
plot_outputs_with_uncertainty(output_names[13]) + ggtitle ("Male adults on ART") + ylab ("Adults on ART")
plot_outputs_with_uncertainty(output_names[14]) + ggtitle ("Rediagnoses at ANC") + ylab ("HIV re-diagnoses")
plot_outputs_with_uncertainty(output_names[15]) + ggtitle ("HIV tests at ANC") + ylab ("HIV tests")
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("Adult AIDS-related mortality") + ylab ("AIDS-related deaths")
plot_outputs_with_uncertainty("TotalARTAdult") + ggtitle ("Adults on ART") + ylab ("Adults on ART")
plot_outputs_with_uncertainty("TotalDiagnosedHIV") + ggtitle ("Adults with diagnosed HIV") + ylab ("HIV diagnosed adults")
plot_outputs_with_uncertainty("Pct1stHIVTestPos") + ggtitle("Test positivity of all HIV tests") + ylab("Positive tests (%)")
plot_outputs_with_uncertainty("PctANCTestPos") + ggtitle("Test positivity of ANC HIV tests") + ylab("Positive tests (%)")
plot_outputs_with_uncertainty("ARTinititationRatio") + ggtitle("Ratio of new diagnoses initiating ART") + ylab("Ratio (ART initiation : new diganoses)")
plot_outputs_with_uncertainty("TotalARTratio") + ggtitle("Proportion of diagnosed adults on ART") + ylab("Proportion of diagnosed adults on ART")
plot_outputs_with_uncertainty("NewDiagPerInfection")
plot_outputs_with_uncertainty("NewANCDiagPerInfection")
plot_outputs_with_uncertainty("ARTInitPerNewInfection")


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
  indicator == "TestEfficiency",
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

plot_pct_chg_uncertainty(output_names[1]) + ggtitle("Total HIV tests")
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
plot_pct_chg_uncertainty("ARTInitPerNewInfection") + ggtitle("ART initation : new infections")


df %>% filter(
  scenario == "baseline",
  indicator == "TotalDiagnosedHIV",
  year >= 2030, year <= 2040) %>% 
  group_by(year, scenario, intervention_year) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  summarise(max(median))
  
# Cumulative values ####

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario") 

cumulative_values %>% filter(
  scenario != "percent_change", 
  indicator == "TestEfficiency",
  scenario == "intervention") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = scenario)) +
  geom_point(aes(color = scenario), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
  xlab("Years") + ylab("Life-years saved \n per 1000 HIV tests") +
  facet_wrap(~indicator, scales = "free_y", 
             labeller = labeller(indicator = c(
               "TestEfficiency" = "Testing efficiency"))) + 
  expand_limits(y=0) + theme_bw()

# plot all baseline and intervention for all outcomes in one plot

cumulative_values$indicator <- 
  factor(cumulative_values$indicator, 
         levels = c("TotalHIVtests","TotalNewHIV",
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

plot_cumulative_epi_uncertainty()

plot_cumulative_epi_pct_chg()

# surveillance outcomes only 

plot_cumulative_surv_uncertainty()

plot_cumulative_surv_pct_chg()

# relationship between HIV infections and  new diagnoses

cumulative_values %>% filter(
  scenario != "percent_change", 
  scenario == "intervention", 
  indicator == "Number1stHIVtestsPos" | indicator == "TotalNewHIV") %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(intervention_year, median, fill = indicator)) +
  geom_point(aes(color = indicator), shape = 15) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = indicator), width = 0.1) +
  xlab("Years")

