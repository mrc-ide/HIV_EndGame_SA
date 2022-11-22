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
                  "HIVinc25to49M","HIVinc25to49F", "HIVinc50M", "HIVinc50F")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- c(2025, 2050, 2099) # establish years PITC changes occur
base_rate_reduction = 1 # proportion of PITC base rate
sliding_scale_reduction = seq(0, 100, 20) # 10x percentage of sliding scale reduction
fsw_condom_usage_decrease = 0 # this is amount that fsw condom usage decreases by each year
st_condom_usage_decrease = 0 # this is amount that st condom usage decreases by each year
lt_condom_usage_decrease = 0 # this is amount that the condom usage decreases by each year
fsw_condom_usage_init = NA # this is the value of the first year's fsw decrease
st_condom_usage_init = NA # this is the value of the first year's st decrease
lt_condom_usage_init = NA # this is the value of the first year's lt decrease
condom_incr_years = NA # these are the year for which condom usage decreases
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
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), labels = c("2025","2050","Baseline"))) %>% 
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
  scale_y_continuous("Number of AIDS-related deaths")  + 
  ggtitle("PITC testing reduction (%)")

# HIV incidence by population ####
# HIV incidence by population 0 to 14
df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc0to14",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("HIV incidence - new infections per population (%)") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("New infections per population (%)", 
                     labels = (function(l) {round(l*1e2,2)})) + 
  ggtitle("HIV incidence in ages 0 to 14")

# HIV incidence by population 15 to 24 males

df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to24M",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("HIV incidence - new infections per population (%)") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("New infections per population (%)", labels = (function(l) {round(l*1e2,2)})) + 
  ggtitle("HIV incidence in males aged 15 to 24")


# HIV incidence by population 15 to 24 females

df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to24F",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("HIV incidence - new infections per population (%)") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("New infections per population (%)", labels = (function(l) {round(l*1e2,2)})) + 
  ggtitle("HIV incidence in females aged 15 to 24")

# HIV incidence by population 24 to 49 males

df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc25to49M",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("HIV incidence - new infections per population (%)") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("New infections per population (%)", labels = (function(l) {round(l*1e2,2)})) + 
  ggtitle("HIV incidence in males aged 25 to 49")

# HIV incidence by population 24 to 49 females

df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc25to49F",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("HIV incidence - new infections per population (%)") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("New infections per population (%)", labels = (function(l) {round(l*1e2,2)})) + 
  ggtitle("HIV incidence in females aged 25 to 49")

# HIV incidence by population over 50 males

df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc50M",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("HIV incidence - new infections per population (%)") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("New infections per population (%)", labels = (function(l) {round(l*1e2,2)})) + 
  ggtitle("HIV incidence in males aged over 50")


# HIV incidence by population over 50 females

df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc50F",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, 
                                    levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), 
                                    labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("HIV incidence - new infections per population (%)") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("New infections per population (%)", labels = (function(l) {round(l*1e2,2)})) + 
  ggtitle("HIV incidence in females aged over 50")

# total adults with HIV

df %>% filter(
  scenario == "intervention",
  indicator == "TotHIV15",
  year >= 2025,
  year < 2100) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = factor(intervention_year, levels = c("2025"="2025","2050" = "2050", "Baseline" = "2099"), labels = c("2025","2050","Baseline"))) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year)) +
  xlab("Years") +
  ylab("Number of adults with HIV") +
  facet_wrap(vars(test_reduction)) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_color_discrete("Test \nreduction \nyear") +
  scale_y_continuous("Adults living with HIV (million)", labels = (function(l) {round(l/1e6,1)})) + ggtitle("PITC testing reduction (%)")


#### calculating cumulative values ####

df <- read.csv("results/df.csv") # this is read again so that test_reduction can be used as an integer
df$test_reduction <- 100 -df$test_reduction
# cumulative values

cumulative_values <- calc_all_cumulatives(intervention_years, 40)

# calculate cumulative percent change from baseline 

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  mutate(absolute_dif = intervention - baseline) %>% 
  pivot_longer(-(indicator:test_reduction), names_to = "scenario")

cumulative_values <- cumulative_values %>% 
  pivot_wider(names_from = intervention_year, values_from = value) %>% 
  select(-`2099`) %>% 
  pivot_longer(-(indicator:scenario), names_to = "intervention_year")

# Plotting cumulative HIV infections over 40 year 
cumulative_values %>% filter(
  scenario == "intervention", indicator == "NewAdultHIV") %>% 
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

