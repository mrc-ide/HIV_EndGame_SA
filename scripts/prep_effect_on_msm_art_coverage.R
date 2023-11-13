#### investigate impact of prep efficacy on msm art coverage ####

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

#### reduce MSM prep efficacy to 0 ####
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
               summary_name = "zero_msm_prep_efficacy" 
)

zero_msm_prep_efficacy <- read_csv("results/zero_msm_prep_efficacy.csv")

#### ART coverage ####

zero_prep_eff_art <- zero_msm_prep_efficacy %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageMSM",
         year >= 2010,
         year < 2100,
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "ARTcoverageMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "ARTcoverageMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_brewer("Scenario", aesthetics = c("colour", "fill"), palette = "Set1", labels = c("Status quo", "Zero MSM PrEP efficacy")) + 
  ggtitle("ART coverage (%) \n(MSM)") 

zero_prep_eff_art

#### PrEP coverage ####

zero_prep_eff_prep_coverage <- zero_msm_prep_efficacy %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageMSM",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "PrEPcoverageMSM",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "PrEPcoverageMSM",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("PrEP coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "Zero MSM PrEP efficacy"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(MSM)") 

#### MSM incidence ####

zero_prep_eff_inc <- zero_msm_prep_efficacy %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2010, 
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVincMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVincMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "Zero MSM PrEP efficacy"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 


#### MSM prevalence ####

zero_prep_eff_prev <- zero_msm_prep_efficacy %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "MSMprev18plus",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "MSMprev18plus",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "MSMprev18plus",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "Zero MSM PrEP efficacy"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(MSM)") 

ggarrange(zero_prep_eff_art, zero_prep_eff_prep_coverage, 
          zero_prep_eff_inc, zero_prep_eff_prev, common.legend = TRUE, 
          legend = "right")

#### reduce MSM prep eligibility to 0 ####
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
               prep_change_start = 2016, 
               summary_name = "zero_msm_prep_elig" 
)

zero_msm_prep_elig <- read_csv("results/zero_msm_prep_elig.csv")

#### ART coverage No PrEP elig ####

zero_prep_elig_art <- zero_msm_prep_elig %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageMSM",
         year >= 2010,
         year < 2100,
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "ARTcoverageMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "ARTcoverageMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_brewer("Scenario", aesthetics = c("colour", "fill"), palette = "Set1", labels = c("Status quo", "Zero MSM PrEP eligibility")) + 
  ggtitle("ART coverage (%) \n(MSM)") 

zero_prep_elig_art

#### PrEP coverage No PrEP elig ####

zero_prep_elig_prep_coverage <- zero_msm_prep_elig %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageMSM",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "PrEPcoverageMSM",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "PrEPcoverageMSM",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("PrEP coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "Zero MSM PrEP eligibility"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(MSM)") 

zero_prep_elig_prep_coverage

#### MSM incidence No PrEP elig ####

zero_prep_elig_inc <- zero_msm_prep_elig %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2010, 
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVincMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVincMSM",
    year >= 2010,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "Zero MSM PrEP eligibility"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 


#### MSM prevalence No PrEP elig ####

zero_prep_elig_prev <- zero_msm_prep_elig %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "MSMprev18plus",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "MSMprev18plus",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "MSMprev18plus",
    year >= 2010,
    year <= 2100,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "Zero MSM PrEP eligibility"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(MSM)") 

ggarrange(zero_prep_elig_art, zero_prep_elig_prep_coverage, 
          zero_prep_elig_inc, zero_prep_elig_prev, common.legend = TRUE, 
          legend = "right")



#### combined scenarios ####

baseline_summary <- baseline_summary %>% 
  filter(scenario == "baseline") %>% 
  mutate(modeled_scenario = "Status quo")

zero_msm_prep_elig <- zero_msm_prep_elig %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "No PrEP eligibility")

zero_msm_prep_efficacy <- zero_msm_prep_efficacy %>% 
  filter(scenario == "intervention") %>% 
  mutate(modeled_scenario = "No PrEP efficacy")

zero_msm_prep_combined <- bind_rows(baseline_summary, zero_msm_prep_efficacy,zero_msm_prep_elig)

zero_msm_prep_combined <- zero_msm_prep_combined %>% 
  mutate(modeled_scenario = factor(modeled_scenario, 
                                          levels = c("Status quo", "No PrEP efficacy", "No PrEP eligibility")))


#### ART coverage comparison ####

zero_prep_eff_art <- zero_msm_prep_combined %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageMSM",
         year >= 2010,
         year < 2100,
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1", labels = c("Status quo", "No MSM PrEP efficacy", "No MSM PrEP uptake")) + 
  ggtitle("ART coverage (%) \n(MSM)") 

zero_prep_eff_art

#### PrEP coverage comparison ####

zero_prep_eff_prep_coverage <- zero_msm_prep_combined %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageMSM",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("PrEP coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", , labels = c("Status quo", "No MSM PrEP efficacy", "No MSM PrEP uptake"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(MSM)") 

#### MSM incidence comparison ####

zero_prep_eff_inc <- zero_msm_prep_combined %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2010, 
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No MSM PrEP efficacy", "No MSM PrEP uptake"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 


#### MSM prevalence comparison ####

zero_prep_eff_prev <- zero_msm_prep_combined %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "MSMprev18plus",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No MSM PrEP efficacy", "No MSM PrEP uptake"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(MSM)") 

ggarrange(zero_prep_eff_art, zero_prep_eff_prep_coverage, 
          zero_prep_eff_inc, zero_prep_eff_prev, common.legend = TRUE, 
          legend = "right")

zero_prep_eff_prep_coverage_all <- zero_msm_prep_combined %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("PrEP coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", , labels = c("Status quo", "No MSM PrEP efficacy", "No MSM PrEP uptake"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(15-49 years)") 

zero_prep_eff_prep_coverage_all

zero_msm_prep_combined %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageFSW",
         year >= 2010,
         year <= 2100,
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2000, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("PrEP coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", , labels = c("Status quo", "No MSM PrEP efficacy", "No MSM PrEP uptake"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(FSW)") 
