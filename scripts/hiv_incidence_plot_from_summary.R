#### Load packages ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

### calculate summary 

df$test_reduction <- 100 - as.numeric(as.character(df$test_reduction))
df$test_reduction <- as.factor(df$test_reduction)

df$intervention_year <- as.integer(df$intervention_year)
means_95_CI_2025 <- df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
  year >= 1990,                # start year of graph
  year < 2100,
  intervention_year == 2025) %>%
  mutate(intervention_year == as.integer(intervention_year)) %>% # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
            lower_CI = quantile(value, probs = 0.025))

means_95_CI_2099 <- df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
  year >= 1990,                # start year of graph
  year < 2100,
  intervention_year == (2099)) %>%
  mutate(intervention_year == as.integer(intervention_year)) %>% # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
            lower_CI = quantile(value, probs = 0.025))

means_95_CI_2030 <- df_2030_2050 %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
  year >= 1990,                # start year of graph
  year < 2100,
  intervention_year == 2030) %>%             # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
            lower_CI = quantile(value, probs = 0.025))

means_95_CI_2040 <- df_2040_2045 %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
  year >= 1990,                # start year of graph
  year < 2100,
  intervention_year == 2040) %>%             # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
            lower_CI = quantile(value, probs = 0.025))

means_95_CI_2045 <- df_2040_2045 %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
  year >= 1990,                # start year of graph
  year < 2100,
  intervention_year == 2045) %>%             # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
            lower_CI = quantile(value, probs = 0.025))

means_95_CI_2050 <- df_2030_2050 %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to49", # can be changed to HIVinc15plus for all adults 15+
  year >= 1990,                # start year of graph
  year < 2100,
  intervention_year == 2050) %>%             # final year is 2099 as the pitc reduction in 2099 is used as baseline or no reduction
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), # all lines are means and shading is 95% CI
            lower_CI = quantile(value, probs = 0.025))

HIV_incidence_means_95_CI <- list(means_95_CI_2025, means_95_CI_2099) 
names(HIV_incidence_means_95_CI) <- c(2025, 2099)
red_condom_HIV_incidence_summary <- bind_rows(HIV_incidence_means_95_CI, .id = "intervention_year")

### read in df

hiv_inc <- read_csv("results/HIV_incidence_summary.csv")

### define function
plot_hiv_inc_from_summary <- function(df, reduction_year, pitc_reduction){
  
  df %>% 
    mutate(intervention_year = as.factor(intervention_year)) %>% 
    filter(intervention_year == reduction_year | 
             intervention_year == "2099", #2099 reduction is used as a baseline / no reduction
           test_reduction == pitc_reduction) %>% #pitc_reduction is used to show the percentage reduced - 100% = complete cessation
    ggplot(aes(year, mean, group = intervention_year, fill = intervention_year), show.legend = F) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = intervention_year), alpha = 0.25, show.legend = F) +
    geom_line(aes(colour = intervention_year), show.legend = F) +
    geom_hline(aes(yintercept = 0.001), lty = 2, colour = 1) +
    xlab(" ") +
    expand_limits(y=0) + theme_classic() + theme(axis.text = element_text(size = 12), axis.title.y = element_text(size = 14)) +
    scale_colour_manual(" ", values = c("red", "blue")) +
    scale_y_continuous("HIV incidence rate per 1000 (15-49y)", 
                       labels = (function(l) {round(l*1e3,2)}),breaks = c(0, 0.0010, 0.0050, 0.010, 0.015, 0.020))
}

# example plot
plot_hiv_inc_from_summary(red_condom_HIV_incidence_summary, 2025, 0)

# baseline plot

incr_art_HIV_inc %>% 
  mutate(intervention_year = as.factor(intervention_year)) %>% 
  filter(intervention_year == "2099", #2099 reduction is used as a baseline / no reduction
         test_reduction == 0) %>% #pitc_reduction is used to show the percentage reduced - 100% = complete cessation
  ggplot(aes(year, mean, fill = "blue"), show.legend = F) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = "blue"), fill = "blue", alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = "blue"), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = 2, lwd = 0.75, colour = 1) +
  expand_limits(y=0)  + theme_classic() + theme(axis.text = element_text(size = 12), axis.title.y = element_text(size = 14))  +
  scale_colour_manual(" ", values = c("blue"), labels = "No reduction") +
  xlab("") +
  scale_y_continuous("HIV incidence rate per 1000 (15-49y)", 
                     labels = (function(l) {round(l*1e3,2)}),breaks = c(0, 0.0010, 0.0050, 0.010, 0.015, 0.020))
  




elimination_years <- hiv_inc %>% 
  filter(mean <= 0.001, mean >=0.00095)
