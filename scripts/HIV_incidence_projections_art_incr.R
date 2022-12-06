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
                  "ARTcoverageAdult", "TotHIV15", "NewHIVinFSW", "NewHIVclients", 
                  "HIVinc0to14", "HIVinc15to49", "HIVinc15to49M", "HIVinc15to49F", 
                  "HIVinc15to24M", "HIVinc15to24F",
                  "HIVinc25to49M","HIVinc25to49F", "HIVinc50M", "HIVinc50F", "HIVinc15plus")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- c(2025, 2099) # establish years PITC changes occur
base_rate_reduction = 1 # proportion of PITC base rate
sliding_scale_reduction = seq(0, 100, 20) # 10x percentage of sliding scale reduction
fsw_condom_usage_decrease = 0.01 # this is amount that fsw condom usage decreases by each year
st_condom_usage_decrease = 0.10 # this is amount that st condom usage decreases by each year
lt_condom_usage_decrease = 0.10 # this is amount that the condom usage decreases by each year
fsw_condom_usage_init = NA # this is the value of the first year's fsw decrease
st_condom_usage_init = NA # this is the value of the first year's st decrease
lt_condom_usage_init = NA # this is the value of the first year's lt decrease
condom_incr_years = 2025 # these are the year for which condom usage decreases
art_interrupt_incr = NA # this is amount that the art interruption rate decreases by each year
art_incr_years = NA # these are the year for which art interruption rate decreases

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
  print(base_rate_reduction)
  for (intervention_year in intervention_years){
    print(intervention_year)
    one_scenario <- run_thembisa_scenario_prev_year(intervention_year,
                                                        fsw_condom_usage_decrease = fsw_condom_usage_decrease,
                                                        st_condom_usage_decrease = st_condom_usage_decrease,
                                                        lt_condom_usage_decrease = lt_condom_usage_decrease,
                                                        condom_incr_years = condom_incr_years,
                                                        condom_maintenance_years = condom_maintenance_years,
                                                        art_interrupt_incr = art_interrupt_incr,
                                                        art_incr_years = art_incr_years,
                                                        output_names,
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
write.csv(df, "results/df_2025_red_condom.csv", row.names = FALSE)
df <- read.csv("results/df_2025_red_condom.csv")


 # HIV incidence by population ####

# HIV incidence of 15-49 year olds
  pitc_reduction <- 80 # percentage pitc reduction to show in graph
  reduction_year <- 2030 # year in which reduction starts
  df_2030_2050 %>% filter(
    scenario == "intervention",
    indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
    year >= 1990,                # start year of graph
    year < 2100) %>%             # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
    group_by(year, intervention_year, test_reduction) %>% 
    summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
              lower_CI = quantile(value, probs = 0.025)) %>% 
    mutate(intervention_year = as.factor(intervention_year)) %>% 
    filter(intervention_year == reduction_year | intervention_year == "2099", test_reduction == pitc_reduction) %>% # the 2099 reduction is used as a baseline / no reduction
    ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.25, show.legend = F) +
    geom_line(aes(colour = intervention_year)) +
    geom_hline(aes(yintercept = 0.001, colour = "Elimination threshold"), lty = 2, colour = 1) +
    xlab("Years") +
    ylab("HIV incidence - new infections per population (%)") +
    expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
    scale_colour_manual(" ", values = c("red", "blue"), labels = c(paste0(pitc_reduction, "% reduction"), "No reduction")) +
    scale_y_continuous("New HIV infections per negative population (%)", 
                       labels = (function(l) {round(l*1e2,2)}),breaks = c(0, 0.0010, 0.0050, 0.010, 0.015, 0.020)) + 
    ggtitle(paste0("HIV incidence for all adults aged 15-49", "\n",pitc_reduction,"% PITC reduction in ", reduction_year))
 
### calculation year elimination is reached

  df_2030_2050 %>% filter(
    scenario == "intervention",
    indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
    year >= 1990,                # start year of graph
    year < 2100) %>%             # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
    group_by(year, intervention_year, test_reduction) %>% 
    summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
              lower_CI = quantile(value, probs = 0.025)) %>% 
    filter(mean<= 0.001 & mean >= 0.0009) %>% filter(intervention_year == 2050) %>% 
    group_by(year, intervention_year, test_reduction) %>%
    summarise(max(mean))
  
  
#### calculating cumulative values ####

df <- read.csv("results/df_2030_2050.csv") # this is read again so that test_reduction can be used as an integer
df$test_reduction <- 100 -df$test_reduction
# cumulative values additional HIV infections between 2025 and 2075

cumulative_2030 <- df_2030_2050 %>% filter(indicator == "NewAdultHIV",
              year >= 2025,
              year <= 2075,
              intervention_year == 2030)  %>% 
  group_by(indicator, intervention_year, scenario, parameter_set, test_reduction) %>% 
  summarise(value = sum(value))

cumulative_2050 <- df_2030_2050 %>% filter(indicator == "NewAdultHIV",
                                           year >= 2025,
                                           year <= 2075,
                                           intervention_year == 2050)  %>% 
  group_by(indicator, intervention_year, scenario, parameter_set, test_reduction) %>% 
  summarise(value = sum(value))
cumulative_values <-list(cumulative_2030, cumulative_2050)
names(cumulative_values) <- c(2030, 2050)
cumulative_2030_2050 <- bind_rows(cumulative_values, .id = "intervention_year")

# calculate cumulative percent change from baseline 

cumulative_2030_2050 <- cumulative_2030_2050 %>%
  pivot_wider(names_from = scenario, values_from = value) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(indicator:test_reduction), names_to = "scenario")

additional_infections_2025_2075 <- cumulative_2030_2050 %>% filter(
  scenario == "absolute_dif", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))
write_csv(additional_infections_2025_2075, "additional_infections_2025_2075.csv")

# Plotting cumulative HIV infections over 40 year 
cumulative_2030_2050 %>% filter(
  scenario == "absolute_dif", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = as.factor(intervention_year)) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% PITC reduction") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
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
  scale_fill_discrete("PITC \nreduction \nstart year") + scale_color_discrete("PITC \nreduction \nstart year")

# Plotting cumulative HIV infections over 40 year 
cumulative_values %>% filter(
  scenario == "intervention", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% PITC reduction") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)}))  +
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
  scale_fill_discrete("PITC \nreduction \nstart year") + scale_color_discrete("PITC \nreduction \nstart year")

# Plotting relative HIV infections over 40 year 
cumulative_values %>% filter(
  scenario == "percent_change", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% PITC reduction") + scale_y_continuous("Relative change in HIV infections (%)")  +
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
  scale_fill_discrete("PITC \nreduction \nstart year") + scale_color_discrete("PITC \nreduction \nstart year")

# Plotting relative AIDS-related deaths over 40 year 
cumulative_values %>% filter(
  scenario == "percent_change", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% PITC reduction") + scale_y_continuous("Relative change in AIDS-related deaths (%)")  +
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
  scale_fill_discrete("PITC \nreduction \nstart year") + scale_color_discrete("PITC \nreduction \nstart year")

# Plotting relative HIV infections over 40 year 
cumulative_values %>% filter(
  scenario == "absolute_dif", indicator == "NewAdultHIV") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% PITC reduction") + scale_y_continuous("Absoltue difference in HIV infections")  +
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
  scale_fill_discrete("PITC \nreduction \nstart year") + scale_color_discrete("PITC \nreduction \nstart year")

# Plotting absolute AIDS-related deaths over 40 year 
cumulative_values %>% filter(
  scenario == "absolute_dif", indicator == "TotalAIDSdeathsadult") %>% 
  group_by(indicator, intervention_year, test_reduction, scenario) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>%
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>%
  ggplot(aes(test_reduction, mean, fill = intervention_year)) +
  geom_line(aes(color = intervention_year), show.legend = T) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = T) +
  xlab("% PITC reduction") + scale_y_continuous("Absolute difference in AIDS-related deaths")  +
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
  scale_fill_discrete("PITC \nreduction \nstart year") + scale_color_discrete("PITC \nreduction \nstart year")

