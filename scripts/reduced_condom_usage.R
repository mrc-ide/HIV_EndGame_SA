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

# establish intervention years
intervention_years <- c(2040)

#### 100% reduction ####
# proportion of base rate by which to reduce
base_rate_reduction = 1 # I want to test this with no changes to the testing rate first

# run baseline model
baseline <- run_thembisa_scenario_future_variable(NA, output_names, 
                                                  base_rate_reduction,
                                                  future_var_parameter = "reduction_condom_fsw", 
                                                  future_var_value = 1, 
                                                  future_var_year = 2040)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario_future_variable(intervention_year, output_names, 
                                                        base_rate_reduction,
                                                        future_var_parameter = "reduction_condom_fsw", 
                                                        future_var_value = 0, 
                                                        future_var_year = 2040)
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
write.csv(df, "results/100_pct_red_df.csv", row.names = FALSE)
df <- read.csv("results/100_pct_red_df.csv")

# testing
plot_outputs_with_uncertainty("TotalHIVtests") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  scale_y_continuous("Number of HIV tests (millions)", breaks = c(0, 10e6, 20e6, 30e6), labels = c(0, 10, 20, 30))
# condom usage
plot_outputs_with_uncertainty("CondomUsage") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
  ylab("Percentage of total sex acts that used condoms (%)")

plot_outputs_with_uncertainty("FSWCondomUsage") + ggtitle ("0% testing reduction, 100% FSW condom reduction") + 
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


# new hiv infections
plot_outputs_with_uncertainty("NewAdultHIV") + ggtitle ("50% testing reduction, 50% FSW condom reduction") + 
  ylab ("New HIV infections")
# aids-related deaths
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("0% testing reduction, 50% ST condom reduction") + 
  ylab ("AIDS-related deaths")



