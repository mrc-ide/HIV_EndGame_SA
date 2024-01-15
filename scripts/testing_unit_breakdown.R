library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(metR)
library(ggpubr)
library(ggfx)
library(RColorBrewer)

# Under status quo, HIV testing was #
cumulative_costs %>% filter(indicator == "percent_total_cost_testing",
                            scenario == "baseline",
                            cumulative_years == 9,
                            discount %in% c("3%","6%"), 
                            test_reduction == 0) %>% 
  mutate(mean = paste0(round(mean,1),"%"),
         lower_CI = paste0(round(lower_CI,1),"%"),
         upper_CI = paste0(round(upper_CI,1),"%")) %>% 
  select(indicator, scenario, discount, mean, lower_CI, upper_CI)

# of the total #
cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "baseline",
                            cumulative_years == 9,
                            discount %in% c("3%","6%"), 
                            test_reduction == 0) %>% 
  mutate(mean = paste(round(mean / 10**9,1), "billion"), 
         lower_CI = paste(round(lower_CI/10**9,1), "billion"),
         upper_CI = paste(round(upper_CI/10**9,1), "billion")) %>% 
  select(indicator, scenario, discount, mean, lower_CI, upper_CI)

# Reducing general HTS by 25% to 100% reduced total costs over 10 years by XX % (cumulative_cost_dif)
# percentage change
cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "percent_change",
                            cumulative_years == 9,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(25, 100)) %>% 
  mutate(mean = paste(round(mean / 10**0,1), "%"), 
         lower_CI = paste(round(lower_CI/10**0,1), "%"),
         upper_CI = paste(round(upper_CI/10**0,1), "%")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, lower_CI, upper_CI)

# absolute difference
cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "absolute_dif",
                            cumulative_years == 9,
                            discount %in% c("3%","6%"), 
                            test_reduction%in% c(25, 100)) %>% 
  mutate(mean = paste(round(mean / 10**6,0), "million"), 
         lower_CI = paste(round(lower_CI/10**6,0), "million"),
         upper_CI = paste(round(upper_CI/10**6,0), "million")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, lower_CI, upper_CI)
  
# Most of this reduction was from lower testing costs #

cumulative_costs %>% filter(indicator == "cost_total_testing",
                            scenario == "absolute_dif",
                            cumulative_years == 9,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(25, 100)) %>% 
  mutate(mean = paste(round(mean / 10**6,0), "million"), 
         lower_CI = paste(round(lower_CI/10**6,0), "million"),
         upper_CI = paste(round(upper_CI/10**6,0), "million")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, lower_CI, upper_CI)

# while some reduction was from lower ART costs 

cumulative_costs %>% filter(indicator == "cost_total_treatment",
                            scenario == "absolute_dif",
                            cumulative_years == 9,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(25, 100)) %>% 
  mutate(mean = paste(round(mean / 10**6,0), "million"), 
         lower_CI = paste(round(lower_CI/10**6,0), "million"),
         upper_CI = paste(round(upper_CI/10**6,0), "million")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, lower_CI, upper_CI)

# Our central estimate suggested that reducing general HTS by 50% would save

cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "absolute_dif",
                            cumulative_years == 49,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(50)) %>% 
  mutate(mean = paste(round(mean / 10**6,0), "million"), 
         lower_CI = paste(round(lower_CI/10**6,0), "million"),
         upper_CI = paste(round(upper_CI/10**6,0), "million")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, upper_CI, lower_CI)

cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "percent_change",
                            cumulative_years == 49,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(50)) %>% 
  mutate(mean = paste(round(mean / 10**0,1), "%"), 
         lower_CI = paste(round(lower_CI/10**0,1), "%"),
         upper_CI = paste(round(upper_CI/10**0,1), "%")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, lower_CI, upper_CI)

# Reducing general HTS by 75% modestly increased the undiscounted cumulative costs by 

cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "absolute_dif",
                            cumulative_years == 49,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(75)) %>% 
  mutate(mean = paste(round(mean / 10**6,0), "million"), 
         lower_CI = paste(round(lower_CI/10**6,0), "million"),
         upper_CI = paste(round(upper_CI/10**6,0), "million")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, upper_CI, lower_CI)

cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "percent_change",
                            cumulative_years == 49,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(75)) %>% 
  mutate(mean = paste(round(mean / 10**0,1), "%"), 
         lower_CI = paste(round(lower_CI/10**0,1), "%"),
         upper_CI = paste(round(upper_CI/10**0,1), "%")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, lower_CI, upper_CI)

# Ceasing general HTS in 2025 increased 50-year cumulative costs by

cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "absolute_dif",
                            cumulative_years == 49,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(100)) %>% 
  mutate(mean = paste(round(mean / 10**6,0), "million"), 
         lower_CI = paste(round(lower_CI/10**6,0), "million"),
         upper_CI = paste(round(upper_CI/10**6,0), "million")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, upper_CI, lower_CI)

cumulative_costs %>% filter(indicator == "cost_total",
                            scenario == "percent_change",
                            cumulative_years == 49,
                            discount %in% c("undiscounted", "3%","6%"), 
                            test_reduction%in% c(100)) %>% 
  mutate(mean = paste(round(mean / 10**0,1), "%"), 
         lower_CI = paste(round(lower_CI/10**0,1), "%"),
         upper_CI = paste(round(upper_CI/10**0,1), "%")) %>% 
  select(indicator, test_reduction, scenario, discount, mean, lower_CI, upper_CI)

# cost by testing line item #

#### cumulative_cost_from_summary ####

cost_cum_base_line <- cost_summary %>% 
  filter(indicator %in% c("cost_general_hts_neg",
                          "cost_general_hts_pos",
                          "cost_anc_test_neg",
                          "cost_anc_test_pos"),
         scenario %in% c("baseline"),
         test_reduction %in% c(0, 25, 50, 75, 100),
         year >= 2025) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value)) %>% 
  group_by(scenario, indicator, pitc_reduction_year, test_reduction, discount) %>% 
  summarise(cumulative_mean = cumsum(mean))
cost_cum_base_line$year <- c(rep(seq(2025,2100,1), 60))
cost_cum_base_line <- cost_cum_base_line %>% 
  filter(year %in% c(2029, 2034, 2049, 2074)) %>% 
  mutate(cumulative_years = year - 2024) %>% 
  relocate(cumulative_years, .before = scenario) 

# intervention testing cost

cost_cum_testing_line <- cost_summary %>% 
  filter(indicator %in% c("cost_general_hts_neg",
                          "cost_general_hts_pos",
                          "cost_anc_test_neg",
                          "cost_anc_test_pos"),
         scenario %in% c("annual_absolute_dif"),
         test_reduction %in% c(0, 25, 50, 75, 100),
         year >= 2025) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value)) %>% 
  group_by(scenario, indicator, pitc_reduction_year, test_reduction, discount) %>% 
  summarise(cumulative_mean = cumsum(mean))
cost_cum_testing_line$year <- c(rep(seq(2025,2100,1), 60))
cost_cum_testing_line <- cost_cum_testing_line %>% 
  filter(year %in% c(2029, 2034, 2049, 2074)) %>% 
  mutate(cumulative_years = year - 2024) %>% 
  relocate(cumulative_years, .before = scenario) 


cost_cum_testing_line$percentage_change_baseline <- (cost_cum_intervention$cumulative_mean  / cost_cum_base_line$cumulative_mean) * 100


cost_summary %>% 
  filter(indicator %in% c("cost_general_hts_neg",
                          "cost_general_hts_pos",
                          "cost_anc_test_neg",
                          "cost_anc_test_pos"),
         scenario %in% c("intervention"),
         test_reduction %in% c(0, 25, 50, 75, 100),
         year >= 2020, discount == "undiscounted") %>% 
  select(-c(future_variability, future_value)) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(year, mean, group = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous("",expand = c(0, 0)) + 
  theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Cost (US$ millions)", labels =(function(l) {round(l/1e6,1)}), expand = c(0,0)) +
  scale_fill_brewer("General HTS reduction (%)", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Costs included in testing component") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_general_hts_neg" = "General adult negative",
                                                                    "cost_general_hts_pos" = "General adult positive",
                                                                    "cost_anc_test_neg" = "ANC negative",
                                                                    "cost_anc_test_pos" = "ANC positive")))

cost_summary %>% 
  filter(indicator %in% c("cost_general_hts_neg",
                          "cost_general_hts_pos",
                          "cost_anc_test_neg",
                          "cost_anc_test_pos"),
         scenario %in% c("annual_percent_change"),
         test_reduction %in% c(0, 25, 50, 75, 100),
         year >= 2020, discount == "undiscounted") %>% 
  select(-c(future_variability, future_value)) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(year, mean, group = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous("",expand = c(0, 0)) + 
  theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Change from baseline cost (%)", labels =(function(l) {round(l,1)}), expand = c(0,0)) +
  scale_fill_brewer("General HTS reduction (%)", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Costs included in testing component") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_general_hts_neg" = "General adult negative",
                                                                     "cost_general_hts_pos" = "General adult positive",
                                                                     "cost_anc_test_neg" = "ANC negative",
                                                                     "cost_anc_test_pos" = "ANC positive")))


test_costs_only <- cost_summary %>% 
  filter(indicator %in% c("cost_general_hts_neg",
                          "cost_general_hts_pos",
                          "cost_anc_test_neg",
                          "cost_anc_test_pos",
                          "cost_total_testing"),
         scenario %in% c("intervention"),
         test_reduction %in% c(0, 25, 50, 75, 100),
         year >= 2020) %>% 
  select(-c(scenario, future_variability, future_value))
write_csv(test_costs_only, "test_costs_only.csv")


test_costs_only_discount <- test_costs_only %>% 
  filter(discount == "undiscounted")

### calculate number of tests (unit) using cost divided by cost per unit

test_costs_only_undiscount_tests <- test_costs_only_discount %>% 
  filter(indicator %in% c("cost_general_hts_neg",
                "cost_general_hts_pos",
                "cost_anc_test_neg",
                "cost_anc_test_pos")) %>% 
  mutate(mean_test = case_when(
    indicator == "cost_general_hts_neg" ~ (mean / 3.62338764573665),
    indicator == "cost_general_hts_pos" ~ (mean / 5.19545019977912),
    indicator == "cost_anc_test_neg" ~ (mean / 3.16302881313226),
    indicator == "cost_anc_test_pos" ~ (mean / 4.71561496557645)),
    lower_CI_test = case_when(
      indicator == "cost_general_hts_neg" ~ (lower_CI / 3.62338764573665),
      indicator == "cost_general_hts_pos" ~ (lower_CI / 5.19545019977912),
      indicator == "cost_anc_test_neg" ~ (lower_CI / 3.16302881313226),
      indicator == "cost_anc_test_pos" ~ (lower_CI / 4.71561496557645)),
    upper_CI_test = case_when(
      indicator == "cost_general_hts_neg" ~ (upper_CI / 3.62338764573665),
      indicator == "cost_general_hts_pos" ~ (upper_CI / 5.19545019977912),
      indicator == "cost_anc_test_neg" ~ (upper_CI / 3.16302881313226),
      indicator == "cost_anc_test_pos" ~ (upper_CI / 4.71561496557645))
    )
    
test_costs_only_undiscount_tests %>% 
  filter(indicator %in% c("cost_general_hts_neg",
                          "cost_general_hts_pos",
                          "cost_anc_test_neg",
                          "cost_anc_test_pos"),
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(year, mean_test, group = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI_test, ymax = upper_CI_test, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous("",expand = c(0, 0)) + 
  theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Number of tests (millions)", labels =(function(l) {round(l/10^6,2)}), expand = c(0,0)) +
  scale_fill_brewer("General\nHTS\nreduction", labels = c("None\n(Status quo)", "25%", "50%", "75%", "100%"), 
                    aesthetics = c("colour", "fill"), palette = "Set1") +
  ggtitle("Annual number of positive and negative tests") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_general_hts_neg" = "Non-ANC negative",
                                                                     "cost_general_hts_pos" = "Non-ANC positive",
                                                                     "cost_anc_test_neg" = "ANC negative",
                                                                     "cost_anc_test_pos" = "ANC positive")))    

#### calculate total tests and positivity ####

# test_diag_pos <- test_costs_only_undiscount_tests %>% 
#   select(-c(mean, lower_CI, upper_CI)) %>% 
#   pivot_wider(names_from = indicator, values_from = c(mean_test, lower_CI_test, upper_CI_test)) %>%
#   mutate(mean_test_gen_total = (mean_test_cost_general_hts_neg + mean_test_cost_general_hts_pos)) %>%
#   mutate(mean_test_anc_total = (mean_test_cost_anc_test_neg + mean_test_cost_anc_test_pos)) %>% 
#   mutate(mean_test_gen_positivity =(mean_test_cost_general_hts_pos / mean_test_gen_total),
#          mean_test_anc_positivity = (mean_test_cost_anc_test_pos/ mean_test_anc_total)) %>% 
#   mutate(lower_CI_test_gen_total = (lower_CI_test_cost_general_hts_neg + lower_CI_test_cost_general_hts_pos)) %>%
#   mutate(lower_CI_test_anc_total = (lower_CI_test_cost_anc_test_neg + lower_CI_test_cost_anc_test_pos)) %>% 
#   mutate(lower_CI_test_gen_positivity =(lower_CI_test_cost_general_hts_pos / lower_CI_test_gen_total),
#          lower_CI_test_anc_positivity = (lower_CI_test_cost_anc_test_pos/ lower_CI_test_anc_total)) %>% 
#   mutate(upper_CI_test_gen_total = (upper_CI_test_cost_general_hts_neg + upper_CI_test_cost_general_hts_pos)) %>%
#   mutate(upper_CI_test_anc_total = (upper_CI_test_cost_anc_test_neg + upper_CI_test_cost_anc_test_pos)) %>% 
#   mutate(upper_CI_test_gen_positivity =(upper_CI_test_cost_general_hts_pos / upper_CI_test_gen_total),
#          upper_CI_test_anc_positivity = (upper_CI_test_cost_anc_test_pos/ upper_CI_test_anc_total)) %>% 
#   pivot_longer(-(year:upper_CI_test), names_to = "indicator",values_to = "mean_test") 
#    
test_diag_pos <- test_costs_only_undiscount_tests %>% 
  select(-c(mean, lower_CI, upper_CI)) %>% 
  pivot_longer(cols = c(mean_test, upper_CI_test, lower_CI_test)) %>% 
  pivot_wider(names_from = indicator) %>% 
  mutate(gen_total = (cost_general_hts_neg + cost_general_hts_pos)) %>%
  mutate(anc_total = (cost_anc_test_neg + cost_anc_test_pos)) %>% 
  mutate(gen_positivity =(cost_general_hts_pos / gen_total),
         anc_positivity = (cost_anc_test_pos/ anc_total)) %>% 
  pivot_longer(-(year:name), names_to = "indicator") %>% 
  pivot_wider()

total_diagnoses <- test_diag_pos %>% 
  filter(indicator %in% c("cost_anc_test_pos",
                          "cost_general_hts_pos"),
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(year, mean_test, group = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI_test, ymax = upper_CI_test, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous(" ", breaks = seq(2025, 2100, 25),expand = c(0, 0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.text = element_text(size = 11),
        plot.margin = unit(c(0,0.5,0,0), "cm"), 
        strip.background = element_blank()) +
  scale_y_continuous("Number of tests (thousands)", labels =(function(l) {round(l/10**3,2)}), expand = c(0,0), limits = c(0,NA)) +
  scale_fill_brewer("General\nHTS\nreduction", labels = c("None\n(Status quo)", "25%", "50%", "75%", "100%"), 
                    aesthetics = c("colour", "fill"), palette = "Set1") +
  ggtitle("") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_general_hts_pos" = "Positive tests excluding ANC",
                                                                     "cost_anc_test_pos" = "Positive tests at ANC",
                                                                     "gen_positivity" = "General positivity",
                                                                     "anc_positivity" = "ANC positivity")))
total_tests <- test_diag_pos %>% 
  filter(indicator %in% c("gen_total",
                          "anc_total"),
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(year, mean_test, group = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI_test, ymax = upper_CI_test, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous(" ", breaks = seq(2025, 2100, 25), expand = c(0, 0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.text = element_text(size = 11),
        plot.margin = unit(c(0,0.5,0,0), "cm"), 
        strip.background = element_blank()) +
  scale_y_continuous("Number of tests (millions)", labels =(function(l) {round(l/10**6,2)}), expand = c(0,0), limits = c(0,NA))+
  scale_fill_brewer("General\nHTS\nreduction", labels = c("None\n(Status quo)", "25%", "50%", "75%", "100%"), 
                    aesthetics = c("colour", "fill"), palette = "Set1") +
  ggtitle("") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("gen_total" = "Total tests excluding ANC",
                                                                     "anc_total" = "Total tests at ANC",
                                                                     "gen_positivity" = "General positivity",
                                                                     "anc_positivity" = "ANC positivity")))

test_positivity <- test_diag_pos %>% 
  filter(indicator %in% c("gen_positivity",
                          "anc_positivity"),
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(year, mean_test, group = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI_test, ymax = upper_CI_test, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous(" ", breaks = seq(2025, 2100, 25), expand = c(0, 0)) +
  theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.text = element_text(size = 11),
        plot.margin = unit(c(0,0.5,0,0), "cm"),
        strip.background = element_blank()) +
  scale_y_continuous("Test positivity (%)", labels =(function(l) {paste0(round(l*10**2,2),"%")}), expand = c(0,0), limits = c(0,NA)) +
  scale_fill_brewer("General\nHTS\nreduction", labels = c("None\n(Status quo)", "25%", "50%", "75%", "100%"), 
                    aesthetics = c("colour", "fill"), palette = "Set1") +
  ggtitle("") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("gen_total" = "Positivity excluding ANC",
                                                                     "anc_total" = "Positivity at ANC",
                                                                     "gen_positivity" = "General positivity",
                                                                     "anc_positivity" = "ANC positivity")))

tests_pos_diag_fig <- ggarrange(total_tests, total_diagnoses, test_positivity, ncol = 1, 
          common.legend = TRUE, legend = "right", labels = "AUTO", font.label = list(size = 14))

ggsave(filename = "figures/tests_pos_diag_fig.pdf", plot = tests_pos_diag_fig, device = "pdf", width = 25, height = 20, units = "cm")
ggsave(filename = "figures/tests_pos_diag_fig.png", plot = tests_pos_diag_fig, device = "png", width = 25, height = 20, units = "cm")
    