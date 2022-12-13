#### Load packages ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

condom_usage_inc <- df %>% filter(
  scenario == "intervention",
  indicator == "HIVinc15to49",
  year >= 1990) %>% 
  group_by(year, intervention_year, test_reduction) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025))


plot_hiv_inc_from_summary <- function(df, reduction_year, pitc_reduction){
  
  df %>% 
    mutate(intervention_year = as.factor(intervention_year)) %>% 
    filter(intervention_year == reduction_year, #2099 reduction is used as a baseline / no reduction
            test_reduction == pitc_reduction | test_reduction == 0) %>% #pitc_reduction is used to show the percentage reduced - 100% = complete cessation
    mutate(test_reduction = factor(test_reduction, levels = c(pitc_reduction, 0))) %>% 
    ggplot(aes(year, mean, group = test_reduction)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
    geom_line(aes(colour = test_reduction), show.legend = F) +
    geom_hline(aes(yintercept = 0.001), lty = 2, colour = 1) +
    xlab(" ") +
    expand_limits(y=0) + theme_classic() + theme(axis.text = element_text(size = 14), axis.title.y = element_text(size = 14), axis.title.x = element_text(size = 14)) +
    scale_colour_manual(" ", values = c("red", "blue")) +
    scale_fill_manual(" ", values = c("red", "blue")) +
    scale_y_continuous("HIV incidence rate per 1000 (15-49y)", 
                       labels = (function(l) {round(l*1e3,2)}),breaks = c(0, 0.0010, 0.0050, 0.010, 0.015, 0.020)) + 
    ggtitle("")
}

# example plot
plot_hiv_inc_from_summary(condom_usage_inc, 2050, 100)

condom_usage_inc %>% 
  mutate(intervention_year = as.factor(2025)) %>% 
  filter(intervention_year == 2025 , #2099 reduction is used as a baseline / no reduction
         test_reduction == 20 | test_reduction == 0) %>% #pitc_reduction is used to show the percentage reduced - 100% = complete cessation
  ggplot(aes(year, mean, group = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = 2, colour = 1) +
  xlab(" ") +
  expand_limits(y=0) + theme_classic() + theme(axis.text = element_text(size = 14), axis.title.y = element_text(size = 14), axis.title.x = element_text(size = 14)) +
  scale_colour_manual(" ", values = c("blue", "red")) +
  scale_y_continuous("HIV incidence rate per 1000 (15-49y)", 
                     labels = (function(l) {round(l*1e3,2)}),breaks = c(0, 0.0010, 0.0050, 0.010, 0.015, 0.020)) + 
  ggtitle("")
