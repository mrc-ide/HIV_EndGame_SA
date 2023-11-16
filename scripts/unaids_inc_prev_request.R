#### Unaids request for Jeff ####

#### changing prep and mmc ####
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(metR)
library(ggpubr)
library(ggfx)
library(RColorBrewer)
library(cowplot)
library(ggh4x)

#### baseline ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = FALSE,
               condom_usage_decrease = 0,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 0,
               art_incr_start = 2025,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = FALSE,
               mmc_rel_rate = 0,
               mmc_change_start = 2025,
               change_prep = FALSE,
               prep_rel_rate = 0, 
               prep_change_start = 2025, 
               summary_name = "baseline" 
)

baseline_summary <- read_csv("results/baseline.csv")

#### sustained prep AND MMC + art retention improved to 95-95-95 ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = FALSE,
               condom_usage_decrease = 0,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = TRUE,
               art_interrupt_rate_decrease = 18.8,
               art_incr_start = 2023,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = FALSE,
               mmc_rel_rate = 0,
               mmc_change_start = 2025,
               change_prep = FALSE,
               prep_rel_rate = 0, 
               prep_change_start = 2025, 
               summary_name = "art95" 
)

art95_summary <- read_csv("results/art95.csv")

#### NB CHANGE "DiscontinuedPrEP" from 0 to 1 in Thembisa.h ####

#### changed prep ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = FALSE,
               condom_usage_decrease = 0,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 0,
               art_incr_start = 2025,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = FALSE,
               mmc_rel_rate = 0,
               mmc_change_start = 2025,
               change_prep = TRUE,
               prep_rel_rate = 1, 
               prep_change_start = 2025, 
               summary_name = "prep" 
)

prep_summary <- read_csv("results/prep.csv")

#### changed prep AND MMC ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = FALSE,
               condom_usage_decrease = 0,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 0,
               art_incr_start = 2025,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = TRUE,
               mmc_rel_rate = 1,
               mmc_change_start = 2025,
               change_prep = TRUE,
               prep_rel_rate = 1, 
               prep_change_start = 2025, 
               summary_name = "prep_mmc" 
)

prep_mmc_summary <- read_csv("results/prep_mmc.csv")

#### changed prep AND MMC and condom usage reduced to 23.3% ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = TRUE,
               condom_usage_decrease = 7,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 0,
               art_incr_start = 2025,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = TRUE,
               mmc_rel_rate = 1,
               mmc_change_start = 2025,
               change_prep = TRUE,
               prep_rel_rate = 1, 
               prep_change_start = 2025, 
               summary_name = "prep_mmc_condom23" 
)

prep_mmc_condom23_summary <- read_csv("results/prep_mmc_condom23.csv")

#### changed prep AND MMC and condom usage reduced to 28% ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = TRUE,
               condom_usage_decrease = 3,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 0,
               art_incr_start = 2025,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = TRUE,
               mmc_rel_rate = 1,
               mmc_change_start = 2025,
               change_prep = TRUE,
               prep_rel_rate = 1, 
               prep_change_start = 2025, 
               summary_name = "prep_mmc_condom28" 
)

prep_mmc_condom28_summary <- read_csv("results/prep_mmc_condom28.csv")

#### changed prep AND MMC + art retention improved to 95-95-95 ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = FALSE,
               condom_usage_decrease = 0,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = TRUE,
               art_interrupt_rate_decrease = 18.8,
               art_incr_start = 2023,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = TRUE,
               mmc_rel_rate = 1,
               mmc_change_start = 2025,
               change_prep = TRUE,
               prep_rel_rate = 1, 
               prep_change_start = 2025, 
               summary_name = "prep_mmc_art95" 
)

prep_mmc_art95_summary <- read_csv("results/prep_mmc_art95.csv")

#### prepare data sets for combining ####

# Status quo
baseline_summary <- baseline_summary %>% 
  filter(scenario == "baseline") %>% 
  mutate(modeled_scenario = "Status quo")

# No PrEP

prep_summary <- prep_summary %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "No PrEP")

# No PrEP + No VMMC 

prep_mmc_summary <- prep_mmc_summary %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "No PrEP + No VMMC")

# No PrEP + No VMMC + Condom usage reduced to 28%

prep_mmc_condom28_summary <- prep_mmc_condom28_summary %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "No PrEP + No VMMC + Condom usage 28%")

# No PrEP + No VMMC + Condom usage reduced to 23%

prep_mmc_condom23_summary <- prep_mmc_condom23_summary %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "No PrEP + No VMMC + Condom usage 23%")

# 95-95-95 but other interventions sustained

art95_summary <- art95_summary %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "95-95-95")

# 95-95-95 + No PrEP + No VMMC
prep_mmc_art95_summary <- prep_mmc_art95_summary %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "95-95-95 + No PrEP + No VMMC")

#### combine data sets ####

all_scenarios <- bind_rows(baseline_summary, prep_summary, prep_mmc_summary, 
                           prep_mmc_condom28_summary, prep_mmc_condom23_summary, 
                           art95_summary, prep_mmc_art95_summary) 

all_scenarios <- all_scenarios %>% 
  mutate(modeled_scenario = factor(modeled_scenario, 
                                   levels = c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                              "No PrEP + No VMMC + Condom usage 28%",
                                              "No PrEP + No VMMC + Condom usage 23%",
                                              "95-95-95", "95-95-95 + No PrEP + No VMMC")))

write_csv(all_scenarios, "results/prep_vmmc_scenarios.csv")
all_scenarios <- read_csv("results/prep_vmmc_scenarios.csv")
all_scenarios <- all_scenarios %>% 
  mutate(modeled_scenario = factor(modeled_scenario, 
                                   levels = c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                              "No PrEP + No VMMC + Condom usage 28%",
                                              "No PrEP + No VMMC + Condom usage 23%",
                                              "95-95-95", "95-95-95 + No PrEP + No VMMC")))

#### select outputs, types and group to include in df ####

names <- c("ARTcoverage15to49", "ARTcoverageAdult", "HIVinc15to49",
"HIVinc15plus", "Prev15to49", "Prev15plus", "IncPrevRatio15to49", "IncPrevRatio15plus", 
"NewAdultHIV", "NewHIV15to49", "TotHIV15to49", "TotHIV15", "Total15to49", "Total15plus")

ylabs <- c("ART coverage", "HIV incidence", "HIV prevalence",
"Incidence:Prevalence ratio", "New HIV infections", "Total living with HIV", "Total population")

groups <- rep(c("15-49 years", "15+ years"), length(names) / 2)
names(groups) <- names
types <- rep(ylabs, each = 2)
names(types) <- names

df <- all_scenarios %>% 
  dplyr::filter(indicator %in% names) %>% 
  tidyr::pivot_longer(cols = c(mean, upper_CI, lower_CI)) %>% 
  tidyr::pivot_wider(names_from = indicator) %>%
  tidyr::pivot_longer(cols = ARTcoverage15to49:Total15to49, names_to = "indicator") %>% 
  tidyr::pivot_wider() %>% 
  dplyr::mutate(type = types[indicator],
                group = groups[indicator])

tidy_df <- df %>% 
  select(-c(scenario, pitc_reduction_year, test_reduction, discount, future_variability, future_value))

write_csv(tidy_df, "./results/inc_prev_adults_thembisa4_5.csv")

#### transforming outputs based on types and groups to for easier plotting ####
names <- c("ARTcoverage15to49", "ARTcoverageAdult", "HIVinc15to49",
           "HIVinc15plus", "Prev15to49", "Prev15plus", "IncPrevRatio15to49", "IncPrevRatio15plus", 
           "NewAdultHIV", "NewHIV15to49", "TotHIV15to49", "TotHIV15", "Total15to49", "Total15plus")

ylabs <- c("ART coverage\n(%)", "HIV incidence\nper 1000", "HIV prevalence\n(%)",
           "Inc:Prev\nratio", "New HIV infections\n(thousands)", "Total PLWH\n(millions)", "Total population\n(millions)")
groups <- rep(c("15-49 years", "15+ years"), length(names) / 2)
names(groups) <- names
types <- rep(ylabs, each = 2)
names(types) <- names

df2 <- all_scenarios %>% 
  dplyr::filter(indicator %in% names) %>% 
  tidyr::pivot_longer(cols = c(mean, upper_CI, lower_CI)) %>% 
  tidyr::pivot_wider(names_from = indicator) %>% 
  dplyr::mutate(across(contains("HIVinc15"), ~ . * 1e3),
                across(contains("Prev15"), ~ . * 100),
                across(contains("coverage"), ~ . * 100),
                across(contains("TotHIV"), ~ . /10^6),
                across(contains("Total15"), ~ . /10^6),
                across(contains("New"), ~ . /1000)
  ) %>% 
  tidyr::pivot_longer(cols = ARTcoverage15to49:Total15to49, names_to = "indicator") %>% 
  tidyr::pivot_wider() %>% 
  dplyr::mutate(type = types[indicator],
                group = groups[indicator])

df2 %>% filter(year >= 2020, 
               !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>%
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", independent = "x") + theme_bw() +
  scale_x_continuous("", breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "top",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold"),
        strip.text.x = element_text(size = 13, face = "bold", angle = 0),
        # strip.background = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), palette = "Set1", 
                    labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"))

ggsave(filename = "unaids_figures/adult_comparison1.png", device = "png", units = "cm", height = 38, width = 24)

df2 %>% filter(year >= 2020, 
               modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>%
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", independent = "x") + theme_bw() +
  scale_x_continuous("", breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "top",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold"),
        strip.text.x = element_text(size = 13, face = "bold", angle = 0),
        # strip.background = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), palette = "Set1")

ggsave(filename = "unaids_figures/adult_comparison2.png", device = "png", units = "cm", height = 38, width = 24)

