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
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", "TotalHIV",
                  "TotHIV15", "TotPop", "TotSexWorkers", "TotBirths", "MalesOver15" ,
                  "FemalesOver15", "FSWcondomUse", "SWsexActs", "MarriedM17to49")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# model input parameters 
intervention_years <- 2025 # establish years PITC changes occur
base_rate_reduction = 1 # proportion of PITC base rate
sliding_scale_reduction = 0 # 10x percentage of sliding scale reduction
fsw_condom_usage_decrease = 0 # this is amount that fsw condom usage decreases by each year
st_condom_usage_decrease = 0 # this is amount that st condom usage decreases by each year
lt_condom_usage_decrease = 0 # this is amount that the condom usage decreases by each year
fsw_condom_usage_init = 0 # this is the value of the first year's fsw decrease
st_condom_usage_init = 0 # this is the value of the first year's st decrease
lt_condom_usage_init = 0 # this is the value of the first year's lt decrease
condom_incr_years = NA # these are the year for which condom usage decreases
art_interrupt_incr = 0 # this is amount that the art interruption rate decreases by each year
art_interrupt_init = 0 # this is the value of the first year's art interruption rate decrease
art_incr_years = 2025 # these are the year for which art interruption rate decreases

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
  art_interrupt_incr <- percentage_value/1000
  art_interrupt_init <- percentage_value/1000
  for (intervention_year in intervention_years){
    art_incr_years <- seq(intervention_year, 2070, 1)
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
  mutate(Pct_FSW_adult_females = ((TotSexWorkers / FemalesOver15)*100)) %>% 
  mutate(Sex_acts_per_FSW = SWsexActs/TotSexWorkers) %>%
  mutate(Pct_adult_females = ((FemalesOver15/TotPop)*100)) %>% 
  mutate(Pct_adult_males = ((MalesOver15/TotPop)*100)) %>% 
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")


# save results 
write.csv(df, "results/df.csv", row.names = FALSE)
df <- read.csv("results/df.csv")

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
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  ylab("Number of HIV tests") + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Number of HIV tests (millions)", labels = (function(l) {round(l/1e6,1)}))


# number of adult females
df %>% filter(
  scenario == "intervention",
  indicator == "FemalesOver15",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Number of females aged > 15 (millions)", labels = (function(l) {round(l/1e6,1)})) +
  ggtitle("Adult females")

# number of adult males
df %>% filter(
  scenario == "intervention",
  indicator == "MalesOver15",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Number of males aged > 15 (millions)", labels = (function(l) {round(l/1e6,1)})) +
  ggtitle("Adult males")

# number of FSW
df %>% filter(
  scenario == "intervention",
  indicator == "TotSexWorkers",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Number of FSW (millions)", labels = (function(l) {round(l/1e6,1)})) +
  ggtitle("Total female sex workers")

# sex acts per fsw

df %>% filter(
  scenario == "intervention",
  indicator == "Sex_acts_per_FSW",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Number of sex acts per FSW") +
  ggtitle("Number of sex acts per FSW")


# number of FSW
df %>% filter(
  scenario == "intervention",
  indicator == "TotSexWorkers",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Number of FSW (millions)", labels = (function(l) {round(l/1e6,1)})) +
  ggtitle("Total female sex workers")

# percentage of adult females that are FSW

df %>% filter(
  scenario == "intervention",
  indicator == "Pct_FSW_adult_females",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Adult females who are FSW (%)") +
  ggtitle("Percentage of adult females who are FSW")

# Percentage of population who are adult females

df %>% filter(
  scenario == "intervention",
  indicator == "Pct_adult_females",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Adult females (%)") +
  ggtitle("Percentage of population who are female adults")

# Percentage of population who are adult males

df %>% filter(
  scenario == "intervention",
  indicator == "Pct_adult_males",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Adult males (%)") +
  ggtitle("Percentage of population who are male adults")


# married men aged 17-49
df %>% filter(
  scenario == "intervention",
  indicator == "MarriedM17to49",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Married males aged 17-49 (millions)", labels = (function(l) {round(l/1e6,1)})) +
  ggtitle("Married males aged 17-49")


# total population
df %>% filter(
  scenario == "intervention",
  indicator == "TotPop",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Total population (millions)", labels = (function(l) {round(l/1e6,1)})) +
  ggtitle("Total population")

# FSW condom use
df %>% filter(
  scenario == "intervention",
  indicator == "FSWcondomUse",
  year >= 2025) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  ggplot(aes(year, mean, group = intervention_year, fill = intervention_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = intervention_year), show.legend = F) +
  xlab("Years") +
  expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12)) +
  scale_y_continuous("Proportion of FSW who use condom") +
  ggtitle("Proportion of FSW who use condom")


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
  scale_color_discrete("Intervention year") +
  scale_y_continuous("Proportion of Adult ART coverage") 


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
  scale_color_discrete("Annual ART \ninterruption rate \nreduction (%)") +
  scale_y_continuous("Number of new HIV infections") 

df <- read.csv("results/df.csv") # this is read again so that test_reduction can be used as an integer

# cumualtive values

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
  geom_point(aes(color = intervention_year), show.legend = T) +
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


