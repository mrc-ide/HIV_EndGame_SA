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
                  "SWsexActs", "TotProtSexActs", "SWsexActsProt", "HIVinc15to49")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- c(2025, 2050, 2099) # establish years PITC changes occur
base_rate_reduction = 1 # proportion of PITC base rate
sliding_scale_reduction = seq(0,100,20) # 1000x percentage of sliding scale reduction
fsw_condom_usage_decrease = 0 # this is amount that fsw condom usage decreases by each year
st_condom_usage_decrease = 0 # this is amount that st condom usage decreases by each year
lt_condom_usage_decrease = 0 # this is amount that the condom usage decreases by each year
fsw_condom_usage_init = 0 # this is the value of the first year's fsw decrease
st_condom_usage_init = 0 # this is the value of the first year's st decrease
lt_condom_usage_init = 0 # this is the value of the first year's lt decrease
condom_incr_years = NA # these are the year for which condom usage decreases
condom_maintenance_years = NA # maintains condom usage at these values after reduction
art_interrupt_incr = NA # this is amount that the art interruption rate decreases by each year
art_interrupt_init = NA # this is the value of the first year's art interruption rate decrease
art_incr_years = NA # these are the year for which art interruption rate decreases

# run baseline model
baseline <- run_thembisa_scenario_prev_year(intervention_year = NA,
                                                   fsw_condom_usage_decrease <- 0.0475,
                                                   st_condom_usage_decrease <- 0.14,
                                                   lt_condom_usage_decrease <- 0.14,
                                                   condom_incr_years <- seq(2035, 2035+10, 1),
                                                   condom_maintenance_years <- seq(2035+11, 2100, 1),
                                                  art_interrupt_incr = NA,
                                                  art_incr_years = art_incr_years,
                                                  output_names = output_names, 
                                                  base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (percentage_value in sliding_scale_reduction){
  fsw_condom_usage_decrease <- 0.0475
  st_condom_usage_decrease <- 0.14
  lt_condom_usage_decrease <- 0.14
  base_rate_reduction <- percentage_value/100
  intervention_year <-2025
  condom_incr_years <- seq(2035, 2035+10, 1)
  condom_maintenance_years <- seq(2035+11, 2100, 1)
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
  intervention_year <-2050
  condom_incr_years <- seq(2035, 2035+10, 1)
  condom_maintenance_years <- seq(2035+11, 2100, 1)
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
  intervention_year <-2099
  condom_incr_years <- 2100
  condom_maintenance_years <- 2100
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
write.csv(df, "results/condom_reduction_2030.csv", row.names = FALSE)
df <- read.csv("results/condom_reduction_2030.csv")


df$test_reduction <- 100 - as.integer(df$test_reduction)
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
  scale_color_discrete("Annual \ncondom usage \nprobability \nreduction (%)") +
  scale_y_continuous("Number of HIV tests (millions)", labels = (function(l) {round(l/1e6,1)})) 

# condom usage
condom_usage_summary <- df %>% filter(
  scenario == "intervention",
  indicator == "CondomUsage",
  year >= 1990) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))

condom_usage_summary %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  filter(test_reduction == 0, intervention_year != 2050) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("") +
  ylab("Number of HIV tests") +
  expand_limits(y=0) + theme_classic() + theme(axis.text = element_text(size = 14), axis.title.y = element_text(size = 14), axis.title.x = element_text(size = 14)) +
  scale_color_manual(" ", values = c("red", "blue")) +
  scale_fill_manual(" ", values = c("red", "blue")) +
  scale_y_continuous("Sex acts that used condom (%)") 

# FSW condom usage
fsw_condom_usage_summary <- df %>% filter(
  scenario == "intervention",
  indicator == "FSWCondomUsage",
  year >= 2020) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))

fsw_condom_usage_summary %>% 
  filter(test_reduction == 0 | test_reduction == 19) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(vars(intervention_year)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Reduction of \nprevious year's \ncondom usage \nprobability (%)") +
  scale_y_continuous("Percentage of FSW sex acts that used condoms (%)") 

# Non-FSW condom usage
df %>% filter(
  scenario == "intervention",
  indicator == "NonFSWCondomUsage",
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
  scale_color_discrete("Annual \ncondom usage \nprobability \nreduction (%)") +
  scale_y_continuous("Percentage of non-FSW sex acts that used condoms (%)") 

# HIV infections
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
  scale_color_discrete("Reduction of \nprevious year's \ncondom usage \nprobability (%)") +
  scale_y_continuous("Number of new HIV infections") 

# AIDS-related deaths
df %>% filter(
  scenario == "intervention",
  indicator == "TotalAIDSdeathsadult",
  year >= 2025, test_reduction == 0 | test_reduction == 10) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction)) +
  xlab("Years") +
  ylab("Number of HIV tests") +
  facet_wrap(ncol = 2, vars(intervention_year)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Reduction of \nprevious year's \ncondom usage \nprobability (%)") +
  scale_y_continuous("AIDS-related deaths") 

df <- read.csv("results/condom_reduction_2030.csv") # this is read again so that test_reduction can be used as an integer
df$test_reduction <- 100 - as.integer(df$test_reduction)

# cumulative values

cumulative_values <- calc_all_cumulatives(intervention_years, 50)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(indicator:test_reduction), names_to = "scenario")

# Plotting cumulative HIV infections over 40 year 
cumulative_values %>% filter(
  scenario == "absolute_dif", indicator == "NewAdultHIV", intervention_year == 2050) %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean)) +
  geom_line(aes(), show.legend = F) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.10, show.legend = F) +
  xlab("PITC reduction (%)") + scale_y_continuous("Additional HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  # facet_wrap(nrow = 1, vars(indicator), scales = "free_y", 
  #            labeller = labeller(indicator = c(
  #              "TotalAIDSdeathsadult" = "AIDS-related mortality",
  #              "TotalHIVtests" = "Total HIV tests",
  #              "NewAdultHIV" = "New HIV infections",
  #              "SWsexActs" = "FSW sex acts",
  #              "SWsexActsProt" = "Protected FSW sex acts",
  #              "TotProtSexActs" = "Total protected sex acts",
  #              "TotSexActs" = "Total sex acts"
  #            ))) +  
  expand_limits(y=0) + theme_classic() + ggtitle(" ") + theme(axis.text = element_text(size = 14), axis.title.y = element_text(size = 14), axis.title.x = element_text(size = 14)) +
  scale_fill_discrete("Condom usage \nreduction \nstart year") + scale_color_discrete("Condom usage \nreduction \nstart year")

# Plotting cumulative AIDS-related deaths over 40 year 
cumulative_values %>% filter(
  scenario == "absolute_dif", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's condom usage probability (%)") + scale_y_continuous("Cumulative AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)}))  +
  facet_wrap(nrow = 1, vars(indicator), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related deaths",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Cumulative values over 40 years") + 
  scale_fill_discrete("Condom usage \nreduction \nstart year") + scale_color_discrete("Condom usage \nreduction \nstart year")

# Plotting relative HIV infections over 40 year 
cumulative_values %>% filter(
  scenario == "percent_change", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's condom usage probability (%)") + scale_y_continuous("Realtive change in HIV infections (%)")  +
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
  scale_fill_discrete("Condom usage \nreduction \nstart year") + scale_color_discrete("Condom usage \nreduction \nstart year")

# Plotting relative AIDS-related deaths over 40 year 
cumulative_values %>% filter(
  scenario == "percent_change", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's condom usage probability (%)") + scale_y_continuous("Relative change in AIDS-related deaths (%)")  +
  facet_wrap(nrow = 1, vars(indicator), scales = "free_y", 
             labeller = labeller(indicator = c(
               "TotalAIDSdeathsadult" = "AIDS-related deaths",
               "TotalHIVtests" = "Total HIV tests",
               "NewAdultHIV" = "New HIV infections",
               "SWsexActs" = "FSW sex acts",
               "SWsexActsProt" = "Protected FSW sex acts",
               "TotProtSexActs" = "Total protected sex acts",
               "TotSexActs" = "Total sex acts"
             ))) +  
  expand_limits(y=0) + theme_bw() + ggtitle("Cumulative values over 40 years") + 
  scale_fill_discrete("Condom usage \nreduction \nstart year") + scale_color_discrete("Condom usage \nreduction \nstart year")

# Plotting absolute HIV infections over 40 year 
cumulative_values %>% filter(
  scenario == "absolute_dif", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's condom usage probability (%)") + scale_y_continuous("Absoltue difference in HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
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
  scale_fill_discrete("Condom usage \nreduction \nstart year") + scale_color_discrete("Condom usage \nreduction \nstart year")

# Plotting absolute AIDS-related deaths over 40 year 
cumulative_values %>% filter(
  scenario == "absolute_dif", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("Reduction of previous year's condom usage probability (%)") + scale_y_continuous("Absolute difference in AIDS-related deaths" , labels = (function(l) {round(l/1e6,1)}))  +
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
  scale_fill_discrete("Condom usage \nreduction \nstart year") + scale_color_discrete("Condom usage \nreduction \nstart year")

