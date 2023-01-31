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

output_names <- c("AdultARTinterrupters", "AdultInterruptPropn", "TotalART15F", "TotalART15M")

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
baseline <- run_thembisa_scenario_prev_year(pitc_reduction_year = NA,
                                            condom_usage_reduction <- FALSE,
                                            fsw_condom_usage_decrease <- 0,
                                            st_condom_usage_decrease <- 0,
                                            lt_condom_usage_decrease <- 0,
                                            condom_incr_years <- NA,
                                            condom_maintenance_years <- condom_maintenance_years,
                                            art_coverage_increase = FALSE,
                                            art_interrupt_rate_decrease = NA,
                                            art_incr_years = art_incr_years,
                                            art_coverage_decrease = FALSE,
                                            art_interrupt_rate_increase = NA, 
                                            art_decr_years = NA,
                                            output_names = output_names, 
                                            base_rate_reduction = base_rate_reduction)


baseline <- baseline %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(TotalART15 = TotalART15F + TotalART15M) %>% 
  mutate(art_interrupt_rate = AdultARTinterrupters / TotalART15) %>% 
  pivot_longer(-(year:parameter_set), names_to = "indicator")
  
baseline %>% group_by(year, indicator) %>% 
  filter(indicator == "art_interrupt_rate", year > 2020) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
            lower_CI = quantile(value, probs = 0.025)) %>%
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.9, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  theme_classic() +
  scale_y_continuous("Proportion of PLWH on ART who are ART interrupters")

baseline_summary_projection <- baseline %>% group_by(year, indicator) %>% 
  filter(indicator == "art_interrupt_rate", year > 2020) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
            lower_CI = quantile(value, probs = 0.025))

baseline_summary_projection %>% 
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.9, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  theme_classic() +
  expand_limits(y = 0) +
  scale_y_continuous("Proportion of PLWH on ART who are ART interrupters", breaks = seq(0, 0.25, 0.05))
             
             
baseline %>% group_by(year, indicator) %>%
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
            lower_CI = quantile(value, probs = 0.025)) %>% 
  filter(indicator == "TotalART15", year < 2021) %>% 
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.9, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  theme_classic() +
  scale_y_continuous("Number of adults on ART (millions)", labels = (function(l) {round(l/1e6,1)})) 


baseline %>% group_by(year, indicator) %>%
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
                   lower_CI = quantile(value, probs = 0.025)) %>% 
  filter(indicator == "AdultARTinterrupters", year < 2021) %>% 
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.9, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  theme_classic() +
  scale_y_continuous("Number of ART interrupters (millions)", labels = (function(l) {round(l/1e6,1)})) 
  
baseline %>% group_by(year, indicator) %>%
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
            lower_CI = quantile(value, probs = 0.025)) %>% 
  filter(indicator == "AdultInterruptPropn", year < 2021) %>% 
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.9, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  theme_classic() +
  scale_y_continuous("Proportion of PLWH who are ART interrupters")
