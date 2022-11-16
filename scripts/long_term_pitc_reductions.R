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
                  "SWsexActs", "TotProtSexActs", "SWsexActsProt", 
                  "ARTcoverageAdult")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- seq(2025, 2050, 5) # establish years PITC changes occur
base_rate_reduction = 1 # proportion of PITC base rate
sliding_scale_reduction = seq(0, 100, 10) # 10x percentage of sliding scale reduction
fsw_condom_usage_decrease = 0 # this is amount that fsw condom usage decreases by each year
st_condom_usage_decrease = 0 # this is amount that st condom usage decreases by each year
lt_condom_usage_decrease = 0 # this is amount that the condom usage decreases by each year
fsw_condom_usage_init = 0 # this is the value of the first year's fsw decrease
st_condom_usage_init = 0 # this is the value of the first year's st decrease
lt_condom_usage_init = 0 # this is the value of the first year's lt decrease
condom_incr_years = NA # these are the year for which condom usage decreases
art_interrupt_incr = 0 # this is amount that the art interruption rate decreases by each year
art_interrupt_init = 0 # this is the value of the first year's art interruption rate decrease
art_incr_years = seq(intervention_year, 2070, 1) # these are the year for which art interruption rate decreases

# run baseline model
baseline <- run_thembisa_scenario_future_variables(intervention_year = NA,
                                                   fsw_condom_usage_init = fsw_condom_usage_init,
                                                   st_condom_usage_init = st_condom_usage_init,
                                                   lt_condom_usage_init = lt_condom_usage_init,
                                                  fsw_condom_usage_decrease = 0,
                                                  st_condom_usage_decrease = 0,
                                                  lt_condom_usage_decrease = 0,
                                                  condom_incr_years = NA,
                                                  art_interrupt_init = art_interrupt_init,
                                                  art_interrupt_incr = NA,
                                                  art_incr_years = art_incr_years,
                                                  output_names = output_names, 
                                                  base_rate_reduction = base_rate_reduction)


# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (percentage_value in sliding_scale_reduction){
  base_rate_reduction <- percentage_value/100
  for (intervention_year in intervention_years){
    one_scenario <- run_thembisa_scenario_future_variables(intervention_year,
                                                         fsw_condom_usage_init = fsw_condom_usage_init,
                                                         st_condom_usage_init = st_condom_usage_init,
                                                         lt_condom_usage_init = lt_condom_usage_init,
                                                        fsw_condom_usage_decrease = fsw_condom_usage_decrease,
                                                        st_condom_usage_decrease = st_condom_usage_decrease,
                                                        lt_condom_usage_decrease = lt_condom_usage_decrease,
                                                        condom_incr_years = condom_incr_years,
                                                        art_interrupt_init = art_interrupt_init,
                                                        art_interrupt_incr = art_interrupt_incr,
                                                        art_incr_years = art_incr_years,
                                                        output_names,
                                                        base_rate_reduction)
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

df$test_reduction <- 100 -df$test_reduction
df$test_reduction <- as.factor(df$test_reduction)

# testing
df %>% filter(
  scenario == "intervention",
  indicator == "TotalHIVtests",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("Number of HIV tests (millions)", labels = (function(l) {round(l/1e6,1)})) + ggtitle("PITC testing reduction (%)")


# New HIV infections
df %>% filter(
  scenario == "intervention",
  indicator == "NewAdultHIV",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("Number of new HIV infections") + ggtitle("PITC testing reduction (%)")


# AIDS-related deaths
df %>% filter(
  scenario == "intervention",
  indicator == "TotalAIDSdeathsadult",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("Number of AIDS-related deaths")  + ggtitle("PITC testing reduction (%)")

# ART coverage
df %>% filter(
  scenario == "intervention",
  indicator == "ARTcoverageAdult",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("Proportion of Adult ART coverage")  + ggtitle("PITC testing reduction (%)")

df <- read.csv("results/df.csv") # this is read again so that test_reduction can be used as an integer
df$test_reduction <- 100 -df$test_reduction
# cumulative values

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(indicator:test_reduction), names_to = "scenario") 

# Plotting cumulative HIV infections over 20 year 
cumulative_values %>% filter(
  scenario == "intervention", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% annual ART interruption rate reduction") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(nrow = 1, vars(indicator), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Cumulative values over 20 years") + 
  scale_fill_discrete("ART \ninterruption rate \nstart year") + scale_color_discrete("ART \ninterruption rate \nstart year")

# Plotting cumulative HIV infections over 30 year 
cumulative_values_30 <- calc_all_cumulatives(intervention_years, 30)

# calculate cumulative percent change from baseline 

cumulative_values_30 <- cumulative_values_30 %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(indicator:test_reduction), names_to = "scenario") 

# Plotting cumulative HIV infections over 30 year 
cumulative_values_30 %>% filter(
  scenario == "intervention", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% annual ART interruption rate reduction") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(nrow = 1, vars(indicator), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Cumulative values over 30 years") + 
  scale_fill_discrete("ART \ninterruption rate \nstart year") + scale_color_discrete("ART \ninterruption rate \nstart year")

# Plotting cumulative HIV infections over 40 year 
cumulative_values_40 <- calc_all_cumulatives(intervention_years, 40)

# calculate cumulative percent change from baseline 

cumulative_values_40 <- cumulative_values_40 %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(indicator:test_reduction), names_to = "scenario") 

# Plotting cumulative HIV infections over 40 year 
cumulative_values_40 %>% filter(
  scenario == "intervention", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% annual ART interruption rate reduction") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(nrow = 1, vars(indicator), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Cumulative values over 40 years") + 
  scale_fill_discrete("ART \ninterruption rate \nstart year") + scale_color_discrete("ART \ninterruption rate \nstart year")

cumulative_years <- seq(20,50, 10)
for (cumulative_year in cumulative_years){
  cumulative_values <- calc_all_cumulatives(intervention_years, cumulative_year)
  write.csv(cumulative_values, paste0("results/cumulative_values_", cumulative_year,".csv"),
            row.names = FALSE)
}

# calculating cumulative values for different time scales

filepaths <- paste0("results/cumulative_values_", cumulative_years, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- cumulative_years
cumulative_values_all_years <- bind_rows(temp, .id = "cumulative_year")

# relative and absolute differences 
cumulative_values_all_years <- cumulative_values_all_years %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(cumulative_year:test_reduction), names_to = "scenario") 

# Cumulative New HIV infections - wrapped by cumulative years
cumulative_values_all_years %>% filter(
  scenario == "intervention", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(cumulative_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Time scale for cumulative values") + 
  scale_fill_discrete("Test \nreduction \nyear") + scale_color_discrete("Test \nreduction \nyear")

# Cumulative AIDS related deaths- wrapped by cumulative years
cumulative_values_all_years %>% filter(
  scenario == "intervention", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Cumulative AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(cumulative_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Time scale for cumulative values") + 
  scale_fill_discrete("Test \nreduction \nyear") + scale_color_discrete("Test \nreduction \nyear")

# Cumulative HIV infections - wrapped by test reduction year
cumulative_values_all_years %>% filter(
  scenario == "intervention", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = cumulative_year)) +
  geom_line(aes(color = cumulative_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = cumulative_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(intervention_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Test reduction year") + 
  scale_fill_discrete("Cumulative \nyears \nafter \nreduction") + scale_color_discrete("Cumulative \nyears \nafter \nreduction")

# Cumulative AIDS related deaths- wrapped by test reduction year
cumulative_values_all_years %>% filter(
  scenario == "intervention", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = cumulative_year)) +
  geom_line(aes(color = cumulative_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = cumulative_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Cumulative AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(intervention_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Test reduction year") + 
  scale_fill_discrete("Cumulative \nyears \nafter \nreduction") + scale_color_discrete("Cumulative \nyears \nafter \nreduction")

# Relative difference New HIV infections - wrapped by cumulative years
cumulative_values_all_years %>% filter(
  scenario == "percent_change", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("HIV infections \nRelative change from baseline (%)")  +
  facet_wrap(vars(cumulative_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Time scale for cumulative values") + 
  scale_fill_discrete("Test \nreduction \nyear") + scale_color_discrete("Test \nreduction \nyear")

# Relative difference AIDS related deaths- wrapped by cumulative years
cumulative_values_all_years %>% filter(
  scenario == "percent_change", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("AIDS-relatd deaths \nRelative change from baseline (%)")  +
  facet_wrap(vars(cumulative_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Time scale for cumulative values") + 
  scale_fill_discrete("Test \nreduction \nyear") + scale_color_discrete("Test \nreduction \nyear")

# Relative difference New HIV infections - wrapped by test reduction year
cumulative_values_all_years %>% filter(
  scenario == "percent_change", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = cumulative_year)) +
  geom_line(aes(color = cumulative_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = cumulative_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("HIV infections \nRelative change from baseline (%)")  +
  facet_wrap(vars(intervention_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Test reduction year") + 
  scale_fill_discrete("Cumulative \nyears \nafter \nreduction") + scale_color_discrete("Cumulative \nyears \nafter \nreduction")

# Relative difference AIDS related deaths- wrapped y test reduction year
cumulative_values_all_years %>% filter(
  scenario == "percent_change", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = cumulative_year)) +
  geom_line(aes(color = cumulative_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = cumulative_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("AIDS-relatd deaths \nRelative change from baseline (%)")  +
  facet_wrap(vars(intervention_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Test reduction year") + 
  scale_fill_discrete("Cumulative \nyears \nafter \nreduction") + scale_color_discrete("Cumulative \nyears \nafter \nreduction")


# Absolute difference New HIV infections - wrapped by cumulative years
cumulative_values_all_years %>% filter(
  scenario == "absolute_dif", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Absolute difference HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(cumulative_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Time scale for cumulative values") + 
  scale_fill_discrete("Test \nreduction \nyear") + scale_color_discrete("Test \nreduction \nyear")

# Absolute difference AIDS related deaths- wrapped by cumulative years
cumulative_values_all_years %>% filter(
  scenario == "absolute_dif", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Absolute difference AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(cumulative_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Time scale for cumulative values") + 
  scale_fill_discrete("Test \nreduction \nyear") + scale_color_discrete("Test \nreduction \nyear")

# Absolute difference HIV infections - wrapped by test reduction year
cumulative_values_all_years %>% filter(
  scenario == "absolute_dif", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = cumulative_year)) +
  geom_line(aes(color = cumulative_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = cumulative_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Absolute difference HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(intervention_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Test reduction year") + 
  scale_fill_discrete("Cumulative \nyears \nafter \nreduction") + scale_color_discrete("Cumulative \nyears \nafter \nreduction")

# Absolute difference AIDS related deaths- wrapped by test reduction year
cumulative_values_all_years %>% filter(
  scenario == "absolute_dif", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario, cumulative_year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(test_reduction, mean, fill = cumulative_year)) +
  geom_line(aes(color = cumulative_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = cumulative_year), alpha = 0.10, show.legend = T) +
  xlab("PITC testing reduction (%)") + scale_y_continuous("Absolute difference AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(vars(intervention_year), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related mortality",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Test reduction year") + 
  scale_fill_discrete("Cumulative \nyears \nafter \nreduction") + scale_color_discrete("Cumulative \nyears \nafter \nreduction")
