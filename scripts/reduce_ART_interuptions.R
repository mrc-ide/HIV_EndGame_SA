#### Set working directory ####

setwd("/Users/stefan/Documents/HIV_EndGame_SA")

#### Load packages/functions ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

source("scripts/modify_rollout.R")
source("R/read_and_run.R")
source("R/art_interuption_fxns.R")

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

#dir.create("results", FALSE, TRUE) 

### establish intervention years
intervention_years <- seq(2025, 2050, 5)

### proportion of base rate by which to reduce
base_rate_reduction = 0.25

### reduce art interuption rate to increase ART coverage
art_interuption_rate = 0.4
art_increase_year = 2050
reduce_art_interuption(0.40, 2050)

### run baseline model
baseline <- run_thembisa_scenario(NA, output_names, base_rate_reduction = base_rate_reduction)

### save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

### for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names, base_rate_reduction = base_rate_reduction)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

### make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)

### testing
plot_outputs_with_uncertainty("ARTcoverageAdult") + ggtitle ("") + scale_y_continuous("Proportion of adults with HIV on ART")
plot_outputs_with_uncertainty(output_names[1]) + ggtitle ("") + scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))

