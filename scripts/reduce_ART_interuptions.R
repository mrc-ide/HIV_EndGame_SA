#### Load packages/functions ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)


source(here("scripts/modify_rollout.R"))
source(here("R/read_and_run.R"))


#### Make empty dataframe for outputs ####

### names of all the outputs of interest

output_names <- c("TotalHIVtests", "NewAdultHIV",
                  "LYlostAIDS", 
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", 
                  "DiagnosedHIV_M", "DiagnosedHIV_F", 
                  "Number1stHIVtestsPos", "StartingARTtot", "Prop1stHIVtestsPos", 
                  "NewDiagnosesPregnancy", "TotalART15F", "TotalART15M", 
                  "RediagnosesPregnancy", "TotANCtests", "VLsuppressed", "TotHIV15",
                  "ARTcoverageAdult")

#### create empty folder for results - only if there isn't one already

dir.create("results", FALSE, TRUE) 

### establish intervention years
intervention_years <- c(2025, 2030)

### proportion of base rate by which to reduce
base_rate_reduction = 0.25

### reduce art interuption rate to increase ART coverage
art_interuption_rate = 0.4
art_increase_year = 2050

### run baseline model
baseline <- run_thembisa_scenario_future_variable(NA, output_names, base_rate_reduction)

### save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

### for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario_future_variable(intervention_year, output_names, base_rate_reduction)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

### make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)
write.csv(df, 'results/df.csv', row.names = FALSE)
df <- read_csv('results/df.csv')
### testing
plot_outputs_with_uncertainty("ARTcoverageAdult") + ggtitle ("") + scale_y_continuous("Proportion of adults with HIV on ART")
plot_outputs_with_uncertainty("TotalHIVtests") + ggtitle ("") + scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))

