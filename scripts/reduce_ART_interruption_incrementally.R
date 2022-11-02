#### Load packages/functions ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("scripts/modify_rollout.R"))
source(here("R/read_and_run.R"))

#### Make empty dataframe for outputs ####

# names of all the outputs of interest

output_names <- c("TotalHIVtests", "NewAdultHIV",
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", "ARTcoverageAdult")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- seq(2025, 2030, 5) # establish years PITC changes occur
base_rate_reduction = 0.5 # proportion of PITC base rate
condom_usage_decrease = NA # this is amount that the condom usage decreases by each year
condom_usage_init = 0.0025 # this is the value of the first year's decrease
condom_incr_years = seq(2025, 2070, 1) # these are the year for which condom usage decreases
art_interrupt_incr = 0.01 # this is amount that the art interruption rate decreases by each year
art_interrupt_init = 0.01 # this is the value of the first year's art interruption rate decrease
art_incr_years = seq(2025, 2070, 1) # these are the year for which art interruption rate decreases

# run baseline model
baseline <- run_thembisa_scenario_future_variables(intervention_year = NA,
                                                  condom_usage_init = condom_usage_init,
                                                  condom_usage_decrease = NA,
                                                  condom_incr_years = condom_incr_years,
                                                  art_interrupt_init = art_interrupt_init,
                                                  art_interrupt_incr = NA,
                                                  art_incr_years = art_incr_years,
                                                  output_names = output_names, 
                                                  base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario_future_variables(intervention_year,
                                                        condom_usage_init,
                                                        condom_usage_decrease,
                                                        condom_incr_years,
                                                        art_interrupt_init = art_interrupt_init,
                                                        art_interrupt_incr = art_interrupt_incr,
                                                        art_incr_years = art_incr_years,
                                                        output_names,
                                                        base_rate_reduction)
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

# save results 
write.csv(df, "results/df.csv", row.names = FALSE)
df <- read.csv("results/df.csv")

# testing
plot_outputs_with_uncertainty("TotalHIVtests") + ggtitle ("50% testing reduction in 2025 or 2035 \n1% ART interruption rate reduction per year from 2025") + 
  scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
# ART coverage
plot_outputs_with_uncertainty("ARTcoverageAdult") + ggtitle ("50% testing reduction in 2025 or 2035 \n1% ART interruption rate reduction per year from 2025") + 
  ylab("Popportion of adults with HIV on ART")


# new hiv infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("50% testing reduction in 2025 or 2035 \n1% ART interruption rate reduction per year from 2025") + 
  ylab ("New HIV infections")

# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("0% testing reduction, 100% condom reduction") + 
  ylab ("AIDS-related deaths")



