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
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF","TotSexActs",
                  "SWsexActs", "TotProtSexActs", "SWsexActsProt")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- seq(2025, 2030, 5) # establish years PITC changes occur
base_rate_reduction = 0.5 # proportion of PITC base rate
condom_usage_decrease = 0.0025 # this is amount that the condom usage decreases by each year
condom_usage_init = 0.0025 # this is the value of the first year's decrease
condom_incr_years = seq(2025, 2070, 1) # these are the year for which condom usage decreases
# run baseline model
baseline <- run_thembisa_scenario_future_variables(intervention_year = NA,
                                                  condom_usage_init = condom_usage_init,
                                                  condom_usage_decrease = NA,
                                                  output_names = output_names, 
                                                  base_rate_reduction = base_rate_reduction,
                                                  condom_incr_years =condom_incr_years)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario_future_variables(intervention_year,
                                                        condom_usage_init,
                                                        condom_usage_decrease,
                                                        output_names, 
                                                        base_rate_reduction,
                                                        condom_incr_years)
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
  mutate(FSWCondomUsage = ((SWsexActsProt/SWsexActs)*100)) %>%
  mutate(NonFSWCondomUsage = (((TotProtSexActs-SWsexActsProt)/(TotSexActs-SWsexActs))*100)) %>% 
  mutate(PctSexActsSW = ((SWsexActs/TotSexActs)*100)) %>%  # percentage of sex acts which are sex work
  mutate(PctProtSexActsSW = ((SWsexActsProt/TotProtSexActs)*100)) %>%  # percentage of protected sex acts which are sex work
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")


# save results 
write.csv(df, "results/df.csv", row.names = FALSE)
df <- read.csv("results/df.csv")

# testing
plot_outputs_with_uncertainty("TotalHIVtests") + ggtitle ("50% testing reduction in 2025 or 2035 \n0.25% condom reduction per year from 2025") + 
  scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
# condom usage
plot_outputs_with_uncertainty("CondomUsage") + ggtitle ("50% testing reduction in 2025 or 2035 \n0.25% condom reduction per year from 2025") + 
  ylab("Percentage of total sex acts that used condoms (%)")


# new hiv infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("50% testing reduction in 2025 or 2035 \n0.25% condom reduction per year from 2025") + 
  ylab ("New HIV infections")

# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("0% testing reduction, 100% condom reduction") + 
  ylab ("AIDS-related deaths")


# extra condom usage checks

plot_outputs_with_uncertainty("FSWCondomUsage") + ggtitle ("0% testing reduction, 100% condom reduction") + 
  ylab("Percentage of FSW sex acts that used condoms (%)")

plot_outputs_with_uncertainty("NonFSWCondomUsage") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  ylab("Percentage of non-FSW sex acts that used condoms (%)")

plot_outputs_with_uncertainty("TotSexActs") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  scale_y_continuous("Number of total sex acts (millions)", labels = (function(l) {round(l/1e6,1)})) 

plot_outputs_with_uncertainty("TotProtSexActs") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  scale_y_continuous("Number of total sex acts that used condoms (millions)", labels = (function(l) {round(l/1e6,1)}))

plot_outputs_with_uncertainty("SWsexActs") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  scale_y_continuous("Number of FSW sex acts (millions)", labels = (function(l) {round(l/1e6,1)}))

plot_outputs_with_uncertainty("SWsexActsProt") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  scale_y_continuous("Number of FSW sex acts that used condoms (millions)", labels = (function(l) {round(l/1e6,1)}))

plot_outputs_with_uncertainty("PctSexActsSW") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  ylab("Percentage of total sex acts that FSW-client (%)")

plot_outputs_with_uncertainty("PctProtSexActsSW") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  ylab("Percentage of protected sex acts that FSW-client (%)")


