#### Set working directory ####

setwd("/Users/stefan/Documents/HIV_EndGame_SA")

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
  mutate(TotalAdultsOnART = TotalARTAdult / TotHIV15) %>% 
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

df <- df %>% pivot_wider(names_from = c(indicator, scenario)) %>% 
  mutate(LifeYrsSaved_intervention = LYlostAIDS_baseline - LYlostAIDS_intervention) %>%
  mutate(TestEfficiencyLYS_intervention = ((LYlostAIDS_baseline - LYlostAIDS_intervention) / (TotalHIVtests_intervention - TotalHIVtests_baseline) ) *1000) %>% 
  mutate(InfectionsAverted_intervention = NewAdultHIV_baseline - NewAdultHIV_intervention) %>%
  mutate(TestEfficiencyIA_intervention = ((NewAdultHIV_baseline - NewAdultHIV_intervention) / (TotalHIVtests_intervention - TotalHIVtests_baseline)) *1000) %>% 
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
