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
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF","TotSexActs",
                  "SWsexActs", "TotProtSexActs", "SWsexActsProt")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# establish intervention years
intervention_years <- seq(2025, 2050, 5)

#### 100% reduction ####
# proportion of base rate by which to reduce
base_rate_reduction = 0.5 # I want to test this with no changes to the testing rate first

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

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(CondomUsage = ((TotProtSexActs/TotSexActs)*100)) %>%
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")


# save results 
write.csv(df, "results/100_pct_red_df.csv", row.names = FALSE)
df <- read.csv("results/100_pct_red_df.csv")

# testing
plot_outputs_with_uncertainty("TotalHIVtests") + ggtitle ("0% testing reduction, 50% ST condom reduction") + scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
# condom usage
plot_outputs_with_uncertainty("CondomUsage")
# new hiv infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("0% testing reduction, 50% ST condom reduction") + ylab ("New HIV infections")
# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("0% testing reduction, 50% ST condom reduction") + ylab ("AIDS-related deaths")



