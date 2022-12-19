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

output_names <- c("MarriedM17to49", "MarriedF17to49")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- c(2099) # establish years PITC changes occur
base_rate_reduction = 1 # proportion of PITC base rate
sliding_scale_reduction = 100 # 1000x percentage of sliding scale reduction
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
                                                   fsw_condom_usage_decrease <- NA,
                                                   st_condom_usage_decrease <- NA,
                                                   lt_condom_usage_decrease <- NA,
                                                   condom_incr_years <- NA,
                                                   condom_maintenance_years <- NA,
                                                  art_interrupt_incr = NA,
                                                  art_incr_years = art_incr_years,
                                                  output_names = output_names, 
                                                  base_rate_reduction = base_rate_reduction)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (percentage_value in sliding_scale_reduction){
  percentage_value <- 100
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

baseline %>% filter(
  indicator == "MarriedM17to49",
  year >= 1985, 
  year <= 2100) %>% 
  group_by(year) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.10, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  ylab("Number of married men (millions)") + theme_classic() + scale_y_continuous(labels = (function(l) {round(l/1e6,2)}))
  theme_bw() + theme(text = element_text(size = 12))
  
baseline %>% filter(
    indicator == "MarriedF17to49",
    year >= 1985, 
    year <= 2100) %>% 
    group_by(year) %>% 
    summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(year, mean)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.10, show.legend = F) +
    geom_line(aes()) +
    xlab("Years") +
    ylab("Number of married wommen (millions)") + theme_classic() + scale_y_continuous(labels = (function(l) {round(l/1e6,2)}))
  theme_bw() + theme(text = element_text(size = 12))