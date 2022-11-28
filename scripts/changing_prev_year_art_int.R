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
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", "TotSexActs",
                  "SWsexActs", "TotProtSexActs", "SWsexActsProt", "ARTcoverageAdult")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- 2025 # establish years PITC changes occur
base_rate_reduction = 1 # proportion of PITC base rate
sliding_scale_reduction = seq(0,100,20) # 10x percentage of sliding scale reduction
fsw_condom_usage_decrease = NA # this is amount that fsw condom usage decreases by each year
st_condom_usage_decrease = NA # this is amount that st condom usage decreases by each year
lt_condom_usage_decrease = NA # this is amount that the condom usage decreases by each year
condom_incr_years = NA # these are the year for which condom usage decreases
condom_maintenance_years = NA # maintains condom usage at these values after reduction
art_interrupt_incr = NA # this is amount that the art interruption rate decreases by each year
art_incr_years = seq(2021, 2070, 1) # these are the year for which art interruption rate decreases

# run baseline model
baseline <- run_thembisa_scenario_future_variables(intervention_year = NA,
                                                   fsw_condom_usage_init = fsw_condom_usage_init,
                                                   st_condom_usage_init = st_condom_usage_init,
                                                   lt_condom_usage_init = lt_condom_usage_init,
                                                  fsw_condom_usage_decrease = 0,
                                                  st_condom_usage_decrease = 0,
                                                  lt_condom_usage_decrease = 0,
                                                  condom_incr_years = NA,
                                                  art_interrupt_init = 0,
                                                  art_interrupt_incr = NA,
                                                  art_incr_years = art_incr_years,
                                                  output_names = output_names, 
                                                  base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (percentage_value in sliding_scale_reduction){
  art_interrupt_incr <- percentage_value/1000
  for (intervention_year in intervention_years){
    one_scenario <- run_thembisa_scenario_prev_year(intervention_year = intervention_year,
                                                    fsw_condom_usage_decrease = fsw_condom_usage_decrease,
                                                    st_condom_usage_decrease = st_condom_usage_decrease,
                                                    lt_condom_usage_decrease = lt_condom_usage_decrease,
                                                    condom_incr_years = condom_incr_years,
                                                    condom_maintenance_years = condom_maintenance_years,
                                                    art_interrupt_incr = art_interrupt_incr,
                                                    art_incr_years = art_incr_years,
                                                    output_names = output_names,
                                                    base_rate_reduction = base_rate_reduction)
    write.csv(one_scenario, paste0("results/scenario_", intervention_year, "_", percentage_value,".csv"),
              row.names = FALSE)
  }
}


# make a new data frame joining all results 
df <- read_thembisa_results_sliding_scale(intervention_years, sliding_scale_reduction)

# calculate total aids-related deaths 

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")

# calculate condom usage for total adults and fsw-client only
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

df$test_reduction <- as.integer(df$test_reduction)/10
df$test_reduction <- as.factor(df$test_reduction)

# testing
df %>% filter(
  scenario == "intervention",
  indicator == "TotalHIVtests",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(vars(intervention_year)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Reduction of \nprevious year's \nART interruption rate (%)") +
  scale_y_continuous("Number of HIV tests (millions)", labels = (function(l) {round(l/1e6,1)})) 


# ART coverage
df %>% filter(
  scenario == "intervention",
  indicator == "ARTcoverageAdult",
  year >= 2020) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction)) +
  geom_hline(yintercept = 0.95, lty = 3) +
  geom_hline(yintercept = 0.78, lty = 3) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(vars(intervention_year)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Reduction of \nprevious year's \nART interruption \nrate (%)") +
  scale_y_continuous(name = "Proportion of Adult ART coverage", breaks = c(seq(0, 1, 0.25), 0.78, 0.95)) + 
  scale_x_continuous("Years", breaks = seq(2020, 2100, 5))

# new hiv infections
df %>% filter(
  scenario == "intervention",
  indicator == "NewAdultHIV",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(ncol = 2, vars(intervention_year)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Reduction of \nprevious year's \nART interruption \nrate (%)") +
  scale_y_continuous("Number of new HIV infections") 

# AIDS related deaths
df %>% filter(
  scenario == "intervention",
  indicator == "TotalAIDSdeathsadult",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(ncol = 2, vars(intervention_year)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Reduction of \nprevious year's \nART interruption \nrate (%)") +
  scale_y_continuous("Number of AIDS-related deaths") 

df <- read.csv("results/df.csv") # this is read again so that test_reduction can be used as an integer
df$test_reduction <- as.integer(df$test_reduction)/10

# calculating cumulative values for different time scales
cumulative_values <- calc_all_cumulatives(intervention_years, 40)
write.csv(cumulative_values, "results/cumulative_values.csv",row.names = FALSE)
cumulative_values <- read_csv("results/cumulative_values.csv")

# relative and absolute differences 
cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(indicator:test_reduction), names_to = "scenario") 

# Cumulative New HIV infections - wrapped by scenario
cumulative_values %>% filter(
  scenario == "intervention" | scenario == "percent_change" | scenario == "absolute_dif", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  mutate(scenario = factor(scenario, levels = c("intervention", "absolute_dif", "percent_change", "baseline"))) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's ART rate reduction (%)") + scale_y_continuous("HIV infections")  +
  facet_wrap(vars(scenario), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ), scenario = c("baseline" = "Baseline", 
                             "intervention" = "Cumulative values",
                             "percent_change" = "Relative difference (%)",
                             "absolute_dif" = "Absolute difference"))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Cumulative HIV infections over 40 years") + 
  scale_fill_discrete("ART \ninterruption \nrate \nreduction \nstart year") + scale_color_discrete("ART \ninterruption \nrate \nreduction \nstart year")

#Cumulative AIDS-related deaths wrapped by scenario
cumulative_values %>% filter(
  scenario != "baseline", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  mutate(scenario = factor(scenario, levels = c("intervention", "absolute_dif", "percent_change", "baseline"))) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's ART rate reduction (%)") + scale_y_continuous("AIDS-related deaths")  +
  facet_wrap(vars(scenario), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ), scenario = c("baseline" = "Baseline", 
                             "intervention" = "Cumulative values",
                             "percent_change" = "Relative difference (%)",
                             "absolute_dif" = "Absolute difference"))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Cumulative AIDS-related deaths over 40 years") + 
  scale_fill_discrete("ART \ninterruption \nrate \nreduction \nstart year") + scale_color_discrete("ART \ninterruption \nrate \nreduction \nstart year")


# Relative change HIV infections - wrapped by ART interruption rate reduction start year
cumulative_values %>% filter(
  scenario == "percent_change", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean)) +
  geom_line(aes(), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI,), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's ART rate reduction (%)") + scale_y_continuous("Realtive change in HIV infections (%)")  +
  facet_wrap(vars(intervention_year), 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ), scenario = c("baseline" = "Baseline", 
                             "intervention" = "Cumulative values",
                             "percent_change" = "Relative difference (%)",
                             "absolute_dif" = "Absolute difference"))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Relative change in HIV infections over 40 years \nStarting ART interrtuption rate at different years") + 
  scale_fill_discrete("ART \ninterruption \nrate \nreduction \nstart year") + scale_color_discrete("ART \ninterruption \nrate \nreduction \nstart year")


# Relative change AIDS related deaths - wrapped by ART interruption rate reduction start year
cumulative_values %>% filter(
  scenario == "percent_change", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean)) +
  geom_line(aes(), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI,), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's ART rate reduction (%)") + scale_y_continuous("Relative change in AIDS related deaths (%)")  +
  facet_wrap(vars(intervention_year), 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ), scenario = c("baseline" = "Baseline", 
                             "intervention" = "Cumulative values",
                             "percent_change" = "Relative difference (%)",
                             "absolute_dif" = "Absolute difference"))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Relative change in AIDS related deaths over 40 years \nStarting ART interrtuption rate at different years") + 
  scale_fill_discrete("ART \ninterruption \nrate \nreduction \nstart year") + scale_color_discrete("ART \ninterruption \nrate \nreduction \nstart year")


# Relative change HIV infections and AIDS related deaths - wrapped by ART interruption rate reduction start year
cumulative_values %>% filter(
  scenario == "percent_change", indicator == "TotalAIDSdeathsadult" | indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean, fill = indicator)) +
  geom_line(aes(colour = indicator), show.legend = T ) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's ART rate reduction (%)") + scale_y_continuous("Relative change in AIDS related deaths (%)")  +
  facet_wrap(vars(intervention_year), 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ), scenario = c("baseline" = "Baseline", 
                             "intervention" = "Cumulative values",
                             "percent_change" = "Relative difference (%)",
                             "absolute_dif" = "Absolute difference"))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Relative change in AIDS related deaths over 40 years \nStarting ART interrtuption rate at different years") + 
  scale_fill_discrete("ART \ninterruption \nrate \nreduction \nstart year") + scale_color_discrete("ART \ninterruption \nrate \nreduction \nstart year")
