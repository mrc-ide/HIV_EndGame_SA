#### changing prep and mmc ####
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(gridExtra)
library(metR)
library(ggpubr)
library(ggfx)
library(RColorBrewer)
library(cowplot)

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

#cumulative_baseline <- read_csv("results/cumulative_baseline.csv")
baseline_summary <- read_csv("results/baseline.csv")

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

#cumulative_prep <- read_csv("results/cumulative_prep.csv")
prep_summary <- read_csv("results/prep.csv")

#### prep incidence ####

prep_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVinc15to49",
    year >= 2020,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVinc15to49",
    year >= 2020,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/No_PrEP.png", device = "png", units = "cm", height = 13, width = 15)

prep_summary %>% 
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
  scale_fill_brewer("Scenario", labels = c("Status quo", "No PrEP"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage (MSM)") 

ggsave(filename = "unaids_figures/PrEP_coverage.png", device = "png", units = "cm", height = 13, width = 15)

prep_summary %>% 
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
  scale_fill_brewer("Scenario", aesthetics = c("colour", "fill"), palette = "Set1", labels = c("Status quo", "No PrEP from 2025")) + 
  ggtitle("ART coverage (%) \n(MSM)") 

#### changed mmc ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(0, 25, 50, 75, 100),
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
               change_prep = FALSE,
               prep_rel_rate = 0, 
               prep_change_start = 2025, 
               summary_name = "mmc" 
)

cumulative_mmc <- read_csv("results/cumulative_mmc.csv")
mmc_summary <- read_csv("results/mmc.csv")

#### mmc 

baseline_summary %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/baseline.png", device = "png", units = "cm", height = 13, width = 15)

mmc_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "TotalMMC",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Annual VMMC (thousands)", labels =(function(l) {round(l/1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Annual VMMC\n(thousands; 10+ years)") 

ggsave(filename = "unaids_figures/VMMC_discontinued.png", device = "png", units = "cm", height = 13, width = 15)
# circumcision coverage 
mmc_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 

ggsave(filename = "unaids_figures/No_VMMC.png", device = "png", units = "cm", height = 13, width = 15)

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

#cumulative_prep_mmc <- read_csv("results/cumulative_prep_mmc.csv")
prep_mmc_summary <- read_csv("results/prep_mmc.csv")

### mmc and prep incidence ####

prep_mmc_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP +\nNoVMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)")

ggsave(filename = "unaids_figures/inc_No_VMMC_No_PrEP.png", device = "png", units = "cm", height = 13, width = 15)

prep_mmc_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 
  
prep_mmc_summary %>%
  mutate(test_reduction = as.factor(test_reduction)) %>% 
    filter(pitc_reduction_year == 2025, 
           indicator == "PrEPcoverageAll",
           year >= 2020, 
           test_reduction %in% c(0), 
           scenario %in% c("baseline", "intervention")) %>% 
    ggplot(aes(year, mean, group = scenario, fill = scenario)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
    geom_line(aes(colour = scenario), show.legend = T) +
    scale_x_continuous("", expand = c(0, 0)) +
    expand_limits(y=0) + theme_classic() + 
    theme(axis.text = element_text(size = 11), 
          axis.title.y = element_text(size = 11), 
          axis.title.x = element_text(size = 11),
          legend.text = element_text(size = 11), 
          plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
          aspect.ratio=1, 
          legend.title = element_text(size = 11)) +
    scale_y_continuous("Adults on PrEP (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
    scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP reduced"), aesthetics = c("colour", "fill"), palette = "Set1") + 
    ggtitle("PrEP coverage (%; 15+ years)") 

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

#cumulative_prep_mmc_condom23 <- read_csv("results/cumulative_prep_mmc_condom23.csv")
prep_mmc_condom23_summary <- read_csv("results/prep_mmc_condom23.csv")


prep_mmc_condom23_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 

prep_mmc_condom23_summary %>%
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on PrEP (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP reduced"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage (%; 15+ years)") 

prep_mmc_condom23_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Protect sex acts (%)",expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "No PrEP +\nNo VMMC +\nCondom-use reduced to 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage \n(%; 15-49 years)")

ggsave(filename = "unaids_figures/CondomUse23.png", device = "png", units = "cm", height = 13, width = 15)

prep_mmc_condom23_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  geom_vline(aes(xintercept = c(2026)), lty = "dotted") + 
  geom_vline(aes(xintercept = c(2035)), lty = "dotted") + 
  geom_vline(aes(xintercept = c(2064)), lty = "dotted") + 
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP +\nNo VMMC +\nCondom-use\nreduced to 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)")

ggsave(filename = "unaids_figures/No_PrEP_No_VMMMC_CondomUse23.png", device = "png", units = "cm", height = 13, width = 15)


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

#cumulative_prep_mmc_condom28 <- read_csv("results/cumulative_prep_mmc_condom28.csv")
prep_mmc_condom28_summary <- read_csv("results/prep_mmc_condom28.csv")


prep_mmc_condom28_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 

prep_mmc_condom28_summary %>%
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on PrEP (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP reduced"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage (%; 15+ years)") 

prep_mmc_condom28_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Condom usage",expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "No PrEP +\nNo VMMC +\nCondom-use reduced to 28%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage \n(%; 15-49 years)")

ggsave(filename = "unaids_figures/No_PrEP_No_VMMMC_CondomUse23.png", device = "png", units = "cm", height = 13, width = 15)

prep_mmc_condom28_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP +\nNo VMMC +\nCondom-use\nreduced to 28%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)")

ggsave(filename = "unaids_figures/No_PrEP_No_VMMC_CondomUse28.png", device = "png", units = "cm", height = 13, width = 15)


#### changed prep AND MMC + art retention improved to 85.3% ####
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
               art_interrupt_rate_decrease = 7,
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
               summary_name = "prep_mmc_art85" 
)

cumulative_prep_mmc_art85 <- read_csv("results/cumulative_prep_mmc_art85.csv")
prep_mmc_art85_summary <- read_csv("results/prep_mmc_art85.csv")


prep_mmc_art85_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  ttheme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 

prep_mmc_art85_summary %>%
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on PrEP (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP reduced"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage (%; 15+ years)") 

prep_mmc_art85_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Condom usage",expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP & condom\nusage reduced &\nVMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage \n(%; 15-49 years)")

prep_mmc_art85_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("ART coverage",expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP & condom\nusage reduced &\nVMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(%; 15+ years)")

ggsave(filename = "unaids_figures/ARTcoverage85.png", device = "png", units = "cm", height = 13, width = 15)

prep_mmc_art85_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP +\nNo VMMC +\nART coverage\nincreased to 85%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)")

ggsave(filename = "unaids_figures/No_PrEP_No_VMMC_ART85.png", device = "png", units = "cm", height = 13, width = 15)

#### changed prep AND MMC + art retention improved to 91% ####
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
               art_interrupt_rate_decrease = 14,
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
               summary_name = "prep_mmc_art90" 
)

cumulative_prep_mmc_art90 <- read_csv("results/cumulative_prep_mmc_art90.csv")
prep_mmc_art90_summary <- read_csv("results/prep_mmc_art90.csv")


prep_mmc_art90_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 

prep_mmc_art90_summary %>%
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on PrEP (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP reduced"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage (%; 15+ years)") 

prep_mmc_art90_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Condom usage",expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP & condom\nusage reduced &\nVMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage \n(%; 15-49 years)")

prep_mmc_art90_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_label(data = filter(prep_mmc_art90_summary,
                          pitc_reduction_year == 2025, 
                          indicator == "ARTcoverageAdult",
                          year == 2035, 
                          test_reduction %in% c(0), 
                          scenario %in% c("intervention")),
            aes(x = year, y = mean, label = round(100*mean,2), fill = NULL), 
            show.legend = FALSE, label.size = NA) +scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("ART coverage",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP & condom\nusage reduced &\nVMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(%; 15+ years)")

prep_mmc_art90_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP +\nNo VMMC +\nART coverage\nincreased to 91%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)")

ggsave(filename = "unaids_figures/No_PrEP_No_VMMC_ART91.png", device = "png", units = "cm", height = 13, width = 15)

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

#cumulative_prep_mmc_art95 <- read_csv("results/cumulative_prep_mmc_art95.csv")

prep_mmc_art95_summary <- read_csv("results/prep_mmc_art95.csv")


prep_mmc_art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 

prep_mmc_art95_summary %>%
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on PrEP (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP reduced"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage (%; 15+ years)") 

prep_mmc_art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Condom usage",expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP & condom\nusage reduced &\nVMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage \n(%; 15-49 years)")

prep_mmc_art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  geom_label(data = filter(prep_mmc_art95_summary,
                           pitc_reduction_year == 2025,
                           indicator == "ARTcoverageAdult",
                           year == 2030,
                           test_reduction %in% c(0),
                           scenario %in% c("intervention")),
             aes(x = year, y = mean, label = round(100*mean,2), fill = NULL),
             show.legend = FALSE, label.size = NA) +scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("ART coverage",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0,1)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "ART retention\nincreased"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(%; 15+ years)")

prep_mmc_art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_line(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVinc15to49",
    year >= 2020,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, mean, colour = scenario)) +
  geom_ribbon(data = filter(
    baseline_summary,
    pitc_reduction_year == 2025,
    indicator == "HIVinc15to49",
    year >= 2020,
    test_reduction %in% c(0),
    scenario %in% c("baseline")),
    aes(year, ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "No PrEP +\nNo VMMC +\nART coverage\nincreased to 90%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)")

ggsave(filename = "unaids_figures/No_PrEP_No_VMMC_ART80.png", device = "png", units = "cm", height = 13, width = 15)


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

#cumulative_prep_mmc_art95 <- read_csv("results/cumulative_prep_mmc_art95.csv")

art95_summary <- read_csv("results/art95.csv")

art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "VMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(%; 15-49 years)") 

art95_summary %>%
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on PrEP (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP reduced"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage (%; 15+ years)") 

art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Condom usage",expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "PrEP & condom\nusage reduced &\nVMMC discontinued"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage \n(%; 15-49 years)")

art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_vline(aes(xintercept = 2030), lty = "dotted") + 
  scale_x_continuous("", expand = c(0, 0)) +
  geom_label(data = filter(art95_summary,
                           pitc_reduction_year == 2025,
                           indicator == "ARTcoverageAdult",
                           year == 2030,
                           test_reduction %in% c(0),
                           scenario %in% c("intervention")),
             aes(x = year, y = mean, label = round(100*mean,2), fill = NULL),
             show.legend = FALSE, label.size = NA) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("ART coverage",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0,1), breaks = c(0, 0.25, 0.50, 0.75, 0.95)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "ART retention\nincreased"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(%; 15+ years)")

art95_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario %in% c("baseline", "intervention")) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("Scenario", labels = c("Status quo", "90.2% ART \ncoverage in 2030"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)")

ggsave(filename = "unaids_figures/inc_ART95_in_2030.png", device = "png", units = "cm", height = 13, width = 15)


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


#### plot HIV incidence 15-49 ####

HIVinc15to49 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
         ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

HIVinc15to49
ggsave(plot = HIVinc15to49, filename = "unaids_figures/HIVinc15to49.png", device = "png", units = "cm", height = 17, width = 20)

HIVinc15to49_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC") ,aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

HIVinc15to49_art95
ggsave(plot = HIVinc15to49_art95, filename = "unaids_figures/HIVinc15to49_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### individual scenario adult incidence ####
# No PrEP
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_baseline.png", device = "png", units = "cm", height = 17, width = 20)

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_NoPrEP.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_NoPrEPNoVMMC.png", device = "png", units = "cm", height = 17, width = 20)


# No PrEP + No VMMC
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_NoPrEPNoVMMC.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP & No PrEP + No VMMC
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_NoPrEPANDNoPrEPNoVMMC.png", device = "png", units = "cm", height = 17, width = 20)


# No PrEP + No VMMC + Condom 28
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_NoPrEPNoVMMCCondom28.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC + Condom 23
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_NoPrEPNoVMMCCondom23.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC + 95-95-95
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "95-95-95")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_95ART.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC + 95-95-95 + No PrEP + VMMC
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95\nNo PrEP + No VMMC"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; 15-49 years)") 

ggsave(filename = "unaids_figures/HIVinc15to49_95ART_NoPrEP_NoVMMC.png", device = "png", units = "cm", height = 17, width = 20)


#### plot HIV incidence FSW ####

HIVincFSW <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

HIVincFSW

ggsave(plot = HIVincFSW, filename = "unaids_figures/HIVincFSW.png", device = "png", units = "cm", height = 17, width = 20)

HIVincFSW_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm"), legend.position = "bottom") +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95", "95-95-95 +\nNo PrEP + No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

HIVincFSW_art95

ggsave(plot = HIVincFSW, filename = "unaids_figures/HIVincFSW_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### individual incidence fsw ####
# Status quo
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

ggsave(filename = "unaids_figures/HIVincFSW_baseline.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

ggsave(filename = "unaids_figures/HIVincFSW_NoPrEP.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

ggsave(filename = "unaids_figures/HIVincFSW_NoPrEPNoVMMC.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC only
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

ggsave(filename = "unaids_figures/HIVincFSW_NoPrEPNoVMMConly.png", device = "png", units = "cm", height = 17, width = 20)

# "No PrEP + No VMMC + Condom usage 28%"
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

ggsave(filename = "unaids_figures/HIVincFSW_NoPrEPNoVMMCCondom28.png", device = "png", units = "cm", height = 17, width = 20)

# "No PrEP + No VMMC + Condom usage 28%"
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", "No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

ggsave(filename = "unaids_figures/HIVincFSW_NoPrEPNoVMMCCondom23.png", device = "png", units = "cm", height = 17, width = 20)

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%",
                                  "95-95-95 + No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95", "95-95-95 +\nNo PrEP + No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 


ggsave(filename = "unaids_figures/HIVincFSW_art95_noprepnovmmc.png", device = "png", units = "cm", height = 17, width = 20)


#### plot HIV incidence MSM ####

HIVincMSM <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

HIVincMSM

ggsave(plot = HIVincMSM, filename = "unaids_figures/HIVincMSM.png", device = "png", units = "cm", height = 17, width = 20)

HIVincMSM_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95", "95-95-95 +\nNo PrEP + No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

HIVincMSM_art95

ggsave(plot = HIVincMSM, filename = "unaids_figures/HIVincMSM_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### individial incidence MSM ####

# Status quo 

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

ggsave(filename = "unaids_figures/HIVincMSM_baseline.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

ggsave(filename = "unaids_figures/HIVincMSM_NoPrEP.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC 

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

ggsave(filename = "unaids_figures/HIVincMSM_NoPrEPNoVMMC.png", device = "png", units = "cm", height = 17, width = 20)


# No PrEP + No VMMC only

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0), limits = c(0, 0.032)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

ggsave(filename = "unaids_figures/HIVincMSM_NoPrEPNoVMMConly.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC + condom 28%

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0), limits = c(0, 0.032)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

ggsave(filename = "unaids_figures/HIVincMSM_NoPrEPNoVMMCCondom28.png", device = "png", units = "cm", height = 17, width = 20)

# No PrEP + No VMMC + condom 23%

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0), limits = c(0, 0.032)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

ggsave(filename = "unaids_figures/HIVincMSM_NoPrEPNoVMMCCondom23.png", device = "png", units = "cm", height = 17, width = 20)

# 95-95-95

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%",
                                  "95-95-95 + No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95", "95-95-95 +\nNo PrEP + No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 


ggsave(filename = "unaids_figures/HIVincMSM_art95.png", device = "png", units = "cm", height = 17, width = 20)

# 95-95-95

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", labels = c("Status quo", "95-95-95", "95-95-95 +\nNo PrEP + No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 


ggsave(filename = "unaids_figures/HIVincMSM_art95_NoprepNovmmc.png", device = "png", units = "cm", height = 17, width = 20)


#### plot HIV prevalence 15-49 ####

Prev15to49 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(15-49 years)") 

Prev15to49

ggsave(plot = Prev15to49, filename = "unaids_figures/Prev15to49.png", device = "png", units = "cm", height = 17, width = 20)

Prev15to49_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(15-49 years)") 

Prev15to49_art95

ggsave(plot = Prev15to49, filename = "unaids_figures/Prev15to49_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### individual prev ####

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence (%) \n(15-49 years)") 

ggsave(filename = "unaids_figures/Prev15to49_baseline.png", device = "png", units = "cm", height = 17, width = 20)

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence (%) \n(15-49 years)") 

ggsave(filename = "unaids_figures/Prev15to49_NoPrEP.png", device = "png", units = "cm", height = 17, width = 20)

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence (%) \n(15-49 years)") 

ggsave(filename = "unaids_figures/Prev15to49_baseline.png", device = "png", units = "cm", height = 17, width = 20)

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"),aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence (%) \n(15-49 years)") 

ggsave(filename = "unaids_figures/Prev15to49_NoPrEPNoVMMC.png", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV prevalence FSW ####

PrevFSW <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrevFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(FSW)") 

PrevFSW

ggsave(plot = PrevFSW, filename = "unaids_figures/PrevFSW.png", device = "png", units = "cm", height = 17, width = 20)

PrevFSW_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrevFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(FSW)") 

PrevFSW_art95

ggsave(plot = PrevFSW, filename = "unaids_figures/PrevFSW_art95.png", device = "png", units = "cm", height = 17, width = 20)


#### plot HIV prevalence MSM ####

MSMprev18plus <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "MSMprev18plus",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(MSM 18+ years)") 

MSMprev18plus

ggsave(filename = "unaids_figures/PrevMSM.png", device = "png", units = "cm", height = 17, width = 20)

MSMprev18plus_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "MSMprev18plus",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}),expand = c(0, 0)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence \n(MSM 18+ years)") 

MSMprev18plus_art95

ggsave(filename = "unaids_figures/PrevMSM_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV ART coverage 15-49 ####

ARTcoverageAdult <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(15+ years)")

ARTcoverageAdult

ggsave(filename = "unaids_figures/ARTcoverage15to49.png", device = "png", units = "cm", height = 17, width = 20)

ARTcoverageAdult_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(15+ years)")

ARTcoverageAdult_art95

ggsave(filename = "unaids_figures/ARTcoverage15to49_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### plotting individual ART coverage ####
# adults
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%",
                                  "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage (%)\n(15+ years)")


ggsave(filename = "unaids_figures/ARTcoverage15to49_art95.png", device = "png", units = "cm", height = 17, width = 20)

# fsw
all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "95-95-95")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage(%)\n(FSW)")

ggsave(filename = "unaids_figures/ARTcoverageFSW_959595.png", device = "png", units = "cm", height = 17, width = 20)
# MSM 

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "95-95-95")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 16)) +
  scale_y_continuous("",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage(%)\n(MSM)")

ggsave(filename = "unaids_figures/ARTcoverageMSM_959595.png", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV ART coverage FSW ####

ARTcoverageFSW <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(FSW)")

ARTcoverageFSW

ggsave(filename = "unaids_figures/ARTcoverageFSW.png", device = "png", units = "cm", height = 17, width = 20)

ARTcoverageFSW_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(FSW)")

ARTcoverageFSW_art95

ggsave(filename = "unaids_figures/ARTcoverageFSW_art95", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV ART coverage MSM ####

ARTcoverageMSM <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(MSM)")

ARTcoverageMSM

ggsave(plot = ARTcoverageMSM, filename = "unaids_figures/ARTcoverageMSM.png", device = "png", units = "cm", height = 17, width = 20)

ARTcoverageMSM_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "ARTcoverageMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("ART coverage (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0.5,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(MSM)")

ARTcoverageMSM_art95

ggsave(filename = "unaids_figures/ARTcoverageMSM_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV numbers on ART 15-49 ####

TotalAdultsOnART <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "TotalAdultsOnART",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC"
                                 # "No PrEP + No VMMC + Condom usage 28%",
                                 # "No PrEP + No VMMC + Condom usage 23%"
                                 )
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on ART (millons)",expand = c(0, 0), labels =(function(l) {round(l/1e6,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Adults on ART\n(millions; 15+ years)")

TotalAdultsOnART

ggsave(filename = "unaids_figures/AdultsOnART.png", device = "png", units = "cm", height = 17, width = 20)

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24", "NewHIV25to49", "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC"
                                 # "No PrEP + No VMMC + Condom usage 28%",
                                 # "No PrEP + No VMMC + Condom usage 23%"
         )
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        legend.position = "right",
        panel.spacing = unit(1.75, "lines")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("New HIV infections\nby age groups") + 
  facet_wrap(~(factor(indicator, 
                      levels =c("NewHIVU15", "NewHIV15to24", "NewHIV25to49", "NewHIV50"), 
                      labels = c("0-14", "15-24", "25-49", "50+"))), nrow = 1)
ggsave(filename = "unaids_figures/new_infection_trends_age.png", device = "png", 
       units = "cm", height = 12, width = 26)

TotalAdultsOnART_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "TotalAdultsOnART",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on ART (millons)",expand = c(0, 0), labels =(function(l) {round(l/1e6,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Adults on ART\n(millions; 15+ years)")

TotalAdultsOnART_art95

ggsave(filename = "unaids_figures/AdultsOnART_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV numbers on ART FSW ####

FSWonART <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "FSWonART",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("FSW on ART (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("FSW on ART\n(thousands)")

FSWonART

ggsave(filename = "unaids_figures/FSWOnART.png", device = "png", units = "cm", height = 17, width = 20)

FSWonART_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "FSWonART",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("FSW on ART (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("FSW on ART\n(thousands)")

FSWonART_art95

ggsave(filename = "unaids_figures/FSWonART_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV numbers on ART MSM ####

MSMonART <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "MSMonART",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("MSM on ART (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("MSM on ART\n(thousands)")

MSMonART

ggsave(filename = "unaids_figures/MSMOnART.png", device = "png", units = "cm", height = 17, width = 20)

#### plot HIV circumcision 15-49 ####

Circumcised15to49 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "Circumcised15to49",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP + No VMMC")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Circumcision coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0,1)) +
  scale_fill_brewer("", labels =c("Status quo", "No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Circumcision coverage\n(men; 15-49 years)")

Circumcised15to49

ggsave(filename = "unaids_figures/circumcision_15to49.png", device = "png", units = "cm", height = 17, width = 20)

#### plot condom usage adults ####

CondomUsage <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo","No PrEP","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Protected sex acts (%)",expand = c(0, 0), labels =(function(l) {round(l,2)}), limits = c(0,50)) +
  scale_fill_brewer("", labels = c("Status quo","No PrEP", "No PrEP + No VMMC", "Condom usage 28%", "Condom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(15+ years)")

CondomUsage

ggsave(filename = "unaids_figures/condomusage.png", device = "png", units = "cm", height = 17, width = 20)

CondomUsage_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsage",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%")
         ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Protected sex acts (%)",expand = c(0, 0), labels =(function(l) {round(l,2)}), limits = c(0,50)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(15+ years)")

CondomUsage_art95
#### plot condom usage FSW ####

CondomUsageFSW <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUsageFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Protected FSW sex acts (%)",expand = c(0, 0), labels =(function(l) {round(l,2)}), limits = c(0,100)) +
  scale_fill_brewer("", labels = c("Status quo","No PrEP","No PrEP + No VMMC", "Condom usage 28%", "Condom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(FSW - old approach)")

CondomUsageFSW

ggsave(filename = "unaids_figures/condomusage.png", device = "png", units = "cm", height = 17, width = 20)



#### comparing fsw condom usage from different calculation ####

FSWcondomUse <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("FSWcondomUse"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo","No PrEP","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Protected FSW sex acts (%)",expand = c(0, 0), labels =(function(l) {round(l*100,2)}), limits = c(0,1)) +
  scale_fill_brewer("", labels = c("Status quo","No PrEP","No PrEP + No VMMC", "Condom usage 28%", "Condom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(FSW)")
FSWcondomUse

FSWcondomUse_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("FSWcondomUse"),
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Protected FSW sex acts (%)",expand = c(0, 0), labels =(function(l) {round(l*100,2)}), limits = c(0,1)) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(FSW)")
FSWcondomUse_art95

#### plot condom usage MSM 15 to 24####

CondomUse15to24MSM <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUse15to24MSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Condom usage (%)",expand = c(0, 0), labels =(function(l) {round(l*100,2)}), limits = c(0,1)) +
  scale_fill_brewer("", labels = c("Status quo", "No PrEP + No VMMC", "Condom usage 28%", "Condom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(MSM; 15-24 years)")

CondomUse15to24MSM

ggsave(filename = "unaids_figures/condomusageMSM15to24.png", device = "png", units = "cm", height = 17, width = 20)


#### plot condom usage MSM 15 to 49 ####

CondomUse15to49MSM <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUse15to49MSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Condom usage (%)",expand = c(0, 0), labels =(function(l) {round(l*100,2)}), limits = c(0,1)) +
  scale_fill_brewer("", labels = c("Status quo","No PrEP","No PrEP + No VMMC", "Condom usage 28%", "Condom usage 23%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(MSM; 15-49 years)")

CondomUse15to49MSM

ggsave(filename = "unaids_figures/condomusageMSM15to49.png", device = "png", units = "cm", height = 17, width = 20)

#### plot condom usage MSM 15 to 49 ####

CondomUse15to49MSM_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "CondomUse15to49MSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo","No PrEP + No VMMC", "95-95-95", 
                                 "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Condom usage (%)",expand = c(0, 0), labels =(function(l) {round(l*100,2)}), limits = c(0,1)) +
  scale_fill_brewer("", labels = c("Status quo","No PrEP + No VMMC", "95-95-95", "95-95-95 + No PrEP + No VMMC"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Condom usage\n(MSM; 15-49 years)")

CondomUse15to49MSM_art95

#### plot HIV PrEP coverage 15-49 ####

PrEPcoverageAll <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("PrEP coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(Adults; 15-49 years)")

PrEPcoverageAll

ggsave(plot = PrEPcoverageAll, filename = "unaids_figures/prep_coverage_15to49.png", device = "png", units = "cm", height = 17, width = 20)

PrEPcoverageAll_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAll",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo","No PrEP + No VMMC", "95-95-95", 
                                 "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("PrEP coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(Adults; 15-49 years)")

PrEPcoverageAll_art95

#### plot HIV PrEP coverage FSW ####

PrEPcoverageFSW <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("PrEP coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(FSW)")

ggsave(filename = "unaids_figures/prep_coverage_fsw.png", device = "png", units = "cm", height = 17, width = 20)


PrEPcoverageFSW_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo","No PrEP + No VMMC", "95-95-95", 
                                 "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("PrEP coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(FSW)")

#### plot HIV PrEP coverage MSM ####

PrEPcoverageMSM <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP","No PrEP + No VMMC", "No PrEP + No VMMC + Condom usage 28%", 
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("PrEP coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(MSM)")

ggsave(plot = PrEPcoverageMSM, filename = "unaids_figures/prep_coverage_msm.png", device = "png", units = "cm", height = 17, width = 20)

PrEPcoverageMSM_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo","No PrEP + No VMMC", "95-95-95", 
                                 "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("PrEP coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(MSM)")

#### plot PrEP coverage AGYW ####

PrEPcoverageAGYW <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "PrEPcoverageAGYW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("PrEP coverage(%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("PrEP coverage\n(AGYW; 15-24 years)")

ggsave(plot = PrEPcoverageAGYW, filename = "unaids_figures/PrEPcoverageAGYW.png", device = "png", units = "cm", height = 17, width = 20)

#### total number of people on PrEP ####

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("AGYWonPrEP", "FSWonPrEP", "MSMonPrEP", "MenOnPrEP", "WomenOnPrEP"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("Number on PrEP (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Numbers on PrEP\n(thousands)") + 
  facet_wrap(~factor(indicator, levels = c("AGYWonPrEP", "FSWonPrEP", "MSMonPrEP", "MenOnPrEP", "WomenOnPrEP"), 
                      labels = c("AGYW (15-24 years)", "FSW", "MSM", "Men (15+ years)", "Women (15+ years)")),
             scales = "free_x")


all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("MenOnPrEP", "WomenOnPrEP"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("MenOnPrEP", "WomenOnPrEP"))) %>% 
  ggplot(aes(year, mean, group = indicator, fill = indicator)) +
  geom_area() +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("Number on PrEP (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", labels = c("Men (15 +)", "Women (15+)", "AGYW", "FSW", "MSM"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Numbers on PrEP\n(thousands)") + facet_wrap(~modeled_scenario) 


all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("MenOnPrEP", "WomenOnPrEP"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("MenOnPrEP", "WomenOnPrEP"))) %>% 
  ggplot(aes(year, mean, group = indicator, fill = indicator, colour = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        panel.spacing = unit(1.1, "lines")) +
  scale_y_continuous("Proportion of adults on PrEP (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("", labels = c("Men (15 +)", "Women (15+)", "AGYW", "FSW", "MSM"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Sex distribution of PrEP") + facet_wrap(~modeled_scenario) 

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("AGYWonPrEP", "FSWonPrEP", "MSMonPrEP", "MenOnPrEP", "WomenOnPrEP"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("MenOnPrEP", "WomenOnPrEP", "AGYWonPrEP", "FSWonPrEP", "MSMonPrEP"))) %>% 
  ggplot(aes(year, mean, group = indicator, fill = indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = indicator), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        panel.spacing = unit(1.1, "lines")) +
  scale_y_continuous("Number on PrEP (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", labels = c("Men (15 +)", "Women (15+)", "AGYW", "FSW", "MSM"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Numbers on PrEP\n(thousands)") + facet_wrap(~modeled_scenario) 

#### plot New HIV infections in adults ####

NewAdultHIV <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewAdultHIV",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("New HIV infections\n(thousands; 15+ years)")

NewAdultHIV

ggsave(plot = NewAdultHIV, filename = "unaids_figures/NewAdultHIV.png", device = "png", units = "cm", height = 17, width = 20)

NewAdultHIV_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewAdultHIV",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("New HIV infections\n(thousands; 15+ years)")

NewAdultHIV_art95

ggsave(filename = "unaids_figures/NewAdultHIV_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### plot New HIV infections in FSW ####

NewHIVinFSW <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewHIVinFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("New HIV infections\n(thousands; FSW)")

NewHIVinFSW

ggsave(plot = NewHIVinFSW, filename = "unaids_figures/NewHIVinFSW.png", device = "png", units = "cm", height = 17, width = 20)

NewHIVinFSW_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewHIVinFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("New HIV infections\n(thousands; FSW)")

NewHIVinFSW_art95

ggsave(plot = NewHIVinFSW_art95, filename = "unaids_figures/NewHIVinFSW_art95.png", device = "png", units = "cm", height = 17, width = 20)


#### plot New HIV infections in MSM ####

NewHIVinMSM <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewHIVinMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "No PrEP + No VMMC + Condom usage 28%",
                                 "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("New HIV infections\n(thousands; MSM)")

NewHIVinMSM

ggsave(plot = NewHIVinMSM, filename = "unaids_figures/NewHIVinMSM.png", device = "png", units = "cm", height = 17, width = 20)

NewHIVinMSM_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewHIVinMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP", "No PrEP + No VMMC",
                                  "No PrEP + No VMMC + Condom usage 28%",
                                  "No PrEP + No VMMC + Condom usage 23%")
  ) %>%  
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("New HIV infections\n(thousands; MSM)")

NewHIVinMSM_art95

ggsave(plot = NewHIVinMSM_art95, filename = "unaids_figures/NewHIVinFSW_art95.png", device = "png", units = "cm", height = 17, width = 20)

#### making combined figures ####

#### incidence, prevalence, ART coverage, New HIV infections ADULTS #### 

ggarrange(HIVinc15to49, Prev15to49, NewAdultHIV, ARTcoverageAdult, 
          common.legend = TRUE, legend = "right", labels = "AUTO")
ggsave(filename = "unaids_figures/adult_combined.png", device = "png", units = "cm", height = 17, width = 22)

#### incidence, prevalence, ART coverage, New HIV infections ADULTS - ART #### 

ggarrange(HIVinc15to49_art95, Prev15to49_art95, NewAdultHIV_art95, ARTcoverageAdult_art95, 
          common.legend = TRUE, legend = "right", labels = "AUTO")

ggsave(filename = "unaids_figures/adult_art95_combined.png", device = "png", units = "cm", height = 17, width = 22)

#### incidence, prevalence, ART coverage, New HIV infections FSW #### 

ggarrange(HIVincFSW, PrevFSW, NewHIVinFSW, ARTcoverageFSW,
          common.legend = TRUE, legend = "right", labels = "AUTO")

ggsave(filename = "unaids_figures/fsw_combined.png", device = "png", units = "cm", height = 17, width = 22)

#### incidence, prevalence, ART coverage, New HIV infections FSW - ART #### 

ggarrange(HIVincFSW_art95, PrevFSW_art95, NewHIVinFSW_art95, ARTcoverageFSW_art95,
          common.legend = TRUE, legend = "right", labels = "AUTO")

ggsave(filename = "unaids_figures/fsw_art95_combined.png", device = "png", units = "cm", height = 17, width = 22)

#### incidence, prevalence, ART coverage, New HIV infections MSM #### 

ggarrange(HIVincMSM, MSMprev18plus, NewHIVinMSM, ARTcoverageMSM,
          common.legend = TRUE, legend = "right", labels = "AUTO")

ggsave(filename = "unaids_figures/msm_combined.png", device = "png", units = "cm", height = 17, width = 22)


#### incidence, prevalence, ART coverage, New HIV infections MSM - ART #### 

ggarrange(HIVincMSM_art95, MSMprev18plus_art95, NewHIVinMSM_art95, ARTcoverageMSM_art95,
          common.legend = TRUE, legend = "right", labels = "AUTO")

ggsave(filename = "unaids_figures/msm_art95_combined.png", device = "png", units = "cm", height = 17, width = 22)

#### incidence, prevalence, ART coverage, New HIV infections ADULTS, FSW, MSM #### 

ggarrange(HIVinc15to49,HIVincFSW,HIVincMSM, Prev15to49, PrevFSW,MSMprev18plus, NewAdultHIV,NewHIVinFSW, NewHIVinMSM,ARTcoverageAdult, ARTcoverageFSW, ARTcoverageMSM,PrEPcoverageAll, PrEPcoverageFSW, PrEPcoverageMSM, CondomUsage, FSWcondomUse, CondomUse15to49MSM, 
          common.legend = TRUE, legend = "right", ncol = 3, nrow = 6)
ggsave(filename = "unaids_figures/all_populations_combined.png", device = "png", units = "cm", height = 30, width = 30)

#### incidence, prevalence, ART coverage, New HIV infections ADULTS, FSW, MSM - ART 95#### 

ggarrange(HIVinc15to49_art95,HIVincFSW_art95,HIVincMSM_art95, Prev15to49_art95, PrevFSW_art95,MSMprev18plus_art95, NewAdultHIV_art95,NewHIVinFSW_art95, NewHIVinMSM_art95,ARTcoverageAdult_art95, ARTcoverageFSW_art95, ARTcoverageMSM_art95,PrEPcoverageAll_art95, PrEPcoverageFSW_art95, PrEPcoverageMSM_art95,CondomUsage_art95, FSWcondomUse_art95, CondomUse15to49MSM_art95, 
          common.legend = TRUE, legend = "right", ncol = 3, nrow = 6)
ggsave(filename = "unaids_figures/all_populations_ART95_combined.png", device = "png", units = "cm", height = 30, width = 25)

#### incidence  ADULTS, FSW, MSM #### 

incidences <- ggarrange(HIVinc15to49,HIVincFSW,HIVincMSM, legend = "none", ncol = 3, nrow = 1)
incidences
#### prevalence  ADULTS, FSW, MSM #### 
prevalences <- ggarrange(Prev15to49, PrevFSW,MSMprev18plus, legend = "none", ncol = 3, nrow = 1)

#### new infections  ADULTS, FSW, MSM #### 
new_infections <- ggarrange(NewAdultHIV,NewHIVinFSW, NewHIVinMSM, legend = "none", ncol = 3, nrow = 1)

#### ART coverage  ADULTS, FSW, MSM #### 
art_coverages <- ggarrange(ARTcoverageAdult, ARTcoverageFSW, ARTcoverageMSM, legend = "none", ncol = 3, nrow = 1)

#### PrEP coverage  ADULTS, FSW, MSM #### 
prep_coverages <- ggarrange(PrEPcoverageAll, PrEPcoverageFSW, PrEPcoverageMSM, legend = "none", ncol = 3, nrow = 1)

#### Condom use  ADULTS, FSW, MSM #### 
condom_uses <- ggarrange(CondomUsage, FSWcondomUse, CondomUse15to49MSM, legend = "none", ncol = 3, nrow = 1)

#### combined ADULTS, FSW, MSM #### 

legend_inc <- get_legend(ARTcoverageMSM) 

ggarrange(incidences, prevalences, new_infections, art_coverages, prep_coverages, condom_uses, 
          nrow = 6, common.legend = TRUE, legend = "bottom",legend.grob = legend_inc)


ggsave(filename = "unaids_figures/all_populations_combined.png", device = "png", units = "cm", height = 30, width = 21)

#### PrEP coverage all, PrEP coverage FSW, PrEP coverage MSM, PrEP coverage AGYW #### 

ggarrange(PrEPcoverageAll, PrEPcoverageFSW, PrEPcoverageMSM, PrEPcoverageAGYW, 
          common.legend = TRUE, legend = "right", labels = "AUTO")
ggsave(filename = "unaids_figures/PrEPCoverage.png", device = "png", units = "cm", height = 17, width = 22)

#### circumcision and condom usage #### 

ggarrange(Circumcised15to49, CondomUsage, legend = "right", labels = "AUTO", nrow = 1,widths = c(1,1.15), heights = c(1,1.15))
ggsave(filename = "unaids_figures/Circumcision_Condoms.png", device = "png", units = "cm", height = 8.5, width = 22)

#### proportion of infections by age group ####

proportion_infection_age <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95", "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), 
                     labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario, 
                     # level = c("Status quo", "No PrEP", "No PrEP + No VMMC")
                     ), nrow =1, scales = "free_y")
ggsave(proportion_infection_age, filename = "unaids_figures/proportion_infections_age.png", device = "png", units = "cm", height = 10, width = 22)


proportion_infection_age_status_quo <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), 
                     labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("")

proportion_infection_age_NoPrEP <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("No PrEP")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), 
                     labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("")

proportion_infection_age_NoPrEPNoVMMC <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), 
                     labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("")

proportion_infection_age_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("95-95-95")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), 
                     labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("")

proportion_infection_age_art95_NoPrEPNoVMMC <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), 
                     labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("")

#### new infections by age group ####

new_infection_age <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95", "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario,
                     # level = c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ), nrow =1, scales = "free_y")
ggsave(new_infection_age, filename = "unaids_figures/new_infections_age.png", device = "png", units = "cm", height = 10, width = 30)

distribution_by_age <- ggarrange(proportion_infection_age,new_infection_age , ncol =1, legend = "right", common.legend = TRUE)

ggsave(distribution_by_age, filename = "unaids_figures/distribution_age.png", device = "png", units = "cm", height = 16, width = 24)

new_infection_age_status_quo <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("") 

new_infection_age_NoPrEP <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("No PrEP")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("") 

new_infection_age_NoPrEPNoVMMC <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("") 

new_infection_age_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("95-95-95")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("") 

new_infection_age_art95NoPrEPNoVMMC <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("") 

#### proportion of infections by each age and sex group ####

proportion_infections_age_sex <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                          "NewHIV25to49M", "NewHIV50F", "NewHIV50M"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95", "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                                                  "NewHIV25to49M", "NewHIV50F", "NewHIV50M"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age and sex", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24 Female", "15-24 Male", 
                               "25-49 Female", "25-49 Male", "50+ Female", "50+ Male"), 
                    palette = "Paired", direction = -1) + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1, scales = "free_y")
proportion_infections_age_sex
ggsave(proportion_infections_age_sex, filename = "unaids_figures/proportion_infections_age_sex.png", device = "png", units = "cm", height = 10, width = 22)

#### new HIV infections by each age and sex group ####

new_infections_age_sex <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                          "NewHIV25to49M", "NewHIV50F", "NewHIV50M"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95",  "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                                                  "NewHIV25to49M", "NewHIV50F", "NewHIV50M"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age and sex", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24 Female", "15-24 Male", 
                               "25-49 Female", "25-49 Male", "50+ Female", "50+ Male"), 
                    palette = "Paired", direction = -1) + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1, scales = "free_y")
new_infections_age_sex
ggsave(new_infections_age_sex, filename = "unaids_figures/new_infections_age_sex.png", device = "png", units = "cm", height = 10, width = 22)

distribution_by_age_sex <- ggarrange(proportion_infections_age_sex, new_infections_age_sex, ncol =1, 
                                     common.legend = TRUE, legend = "right")
ggsave(distribution_by_age_sex, filename = "unaids_figures/distribution_by_age_sex.png", device = "png", units = "cm", height = 16, width = 24)


#### new HIV infections lines - age and sex group ####

new_infection_age_sex_lines <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                          "NewHIV25to49M", "NewHIV50F", "NewHIV50M"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95",  "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                                                  "NewHIV25to49M", "NewHIV50F", "NewHIV50M"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour = indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.7, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age and sex", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24 Female", "15-24 Male", 
                               "25-49 Female", "25-49 Male", "50+ Female", "50+ Male"), 
                    palette = "Paired", direction = -1) + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1, scales = "free_y")
new_infection_age_sex_lines
ggsave(new_infection_age_sex_lines, filename = "unaids_figures/new_infection_age_sex_lines.png", device = "png", units = "cm", height = 10, width = 22)

#### new HIV infections lines - scenario by age and sex ####

new_infection_age_sex_lines_by_scenario <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                          "NewHIV25to49M", "NewHIV50F", "NewHIV50M"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95",  "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24F", "NewHIV15to24M", "NewHIV25to49F",
                                                  "NewHIV25to49M", "NewHIV50F", "NewHIV50M"))) %>% 
  ggplot(aes(year, mean, fill = modeled_scenario)) +
  geom_line(aes(colour = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 11, face = "bold"),
        panel.spacing = unit(1, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("", aesthetics = c("colour", "fill"), 
                    palette = "Set1") + 
  ggtitle("") + 
  facet_wrap(~factor(indicator), nrow = 1, scales = "free_y",labeller = as_labeller(c("NewHIVU15" = "0-14","NewHIV15to24F" = "15-24 Female", "NewHIV15to24M" = "15-24 Male", 
                                                                                      "NewHIV25to49F" = "25-49 Female", "NewHIV25to49M" = "25-49 Male", "NewHIV50F" =  "50+ Female",
                                                                                      "NewHIV50M" = "50+ Male")))
new_infection_age_sex_lines_by_scenario
ggsave(new_infection_age_sex_lines_by_scenario, filename = "unaids_figures/new_infection_age_sex_lines_by_scenario.png", device = "png", units = "cm", height = 10, width = 30)

    #### grid by age and sex ####

HIVinfectionsByAgeSex <- ggarrange(new_infections_age_sex, proportion_infections_age_sex, new_infection_age_sex_lines, 
          legend = "bottom", common.legend = TRUE, ncol =1)

annotate_figure(HIVinfectionsByAgeSex,
                fig.lab = "Status quo                             No PrEP                            No PrEP + No VMMC                95-95-95                      95-95-95 + No PrEP + No VMMC",
                fig.lab.pos = "top.left", fig.lab.size = 12, fig.lab.face = "bold")

ggsave(filename = "unaids_figures/HIVinfectionsByAgeSex.png", device = "png", units = "cm", height = 21, width = 30)

#### proportion infections by kp groups ####
proportion_infections_kp <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("Prop_NewHIV_FSW", "Prop_NewHIV_client", "Prop_NewHIV_MSM"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95", "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("Prop_NewHIV_FSW", "Prop_NewHIV_client", "Prop_NewHIV_MSM"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity") +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0,0.31)) +
  scale_fill_brewer("Key population", aesthetics = c("colour", "fill"), 
                    labels = c("FSW", "FSW-clients", "MSM"), 
                    palette = "Set1") + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1, scales = "free_y")
proportion_infections_kp

ggsave(filename = "unaids_figures/proportion_infections_kp.png", device = "png", units = "cm", height = 10, width = 30)


#### distributions of infections by kp groups ####

new_infections_kp <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVinFSW", "NewHIVclients", "NewHIVinMSM"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95",  "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVinFSW", "NewHIVclients", "NewHIVinMSM"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age and sex", aesthetics = c("colour", "fill"), 
                    labels = c("FSW", "FSW-clients", "MSM"), 
                    palette = "Set1") + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1, scales = "free_y")

new_infections_kp

all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVinFSW", "NewHIVclients", "NewHIVinMSM"),
         year >= 2020, 
         test_reduction %in% c(0), 
         !modeled_scenario %in% c("No PrEP + No VMMC + Condom usage 28%", "No PrEP + No VMMC + Condom usage 23%",
                                  "95-95-95", "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator,
                            levels = c("NewHIVinFSW", "NewHIVclients", "NewHIVinMSM")),
         ) %>%
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes(), stat = "identity") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2020, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(1.75, "lines")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Key population", aesthetics = c("colour", "fill"), 
                    labels = c("FSW", "FSW-clients", "MSM"), 
                    palette = "Set1") + 
  ggtitle("New HIV infections amongst key populations") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1)
ggsave(filename = "unaids_figures/new_infections_kp.png", device = "png", units = "cm", height = 10, width = 30)

#### new infections KP lines ####

new_infection_lines_kp <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVinFSW", "NewHIVclients", "NewHIVinMSM"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95",  "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVinFSW", "NewHIVclients", "NewHIVinMSM"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour = indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0.6, "lines"), 
        plot.margin = margin(r = 0.5,unit = "cm"), 
        legend.position = "bottom") +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0), labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age and sex", aesthetics = c("colour", "fill"), 
                    labels = c("FSW", "FSW-clients", "MSM"), 
                    palette = "Set1") + 
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1, scales = "free_y")

new_infection_lines_kp

#### grid of kp infections ####

kp_infection <- ggarrange(new_infections_kp,proportion_infections_kp,new_infection_lines_kp,
          common.legend = TRUE, legend = "right", ncol =1)

annotate_figure(kp_infection,
                fig.lab = "Status quo                             No PrEP                            No PrEP + No VMMC                95-95-95                      95-95-95 + No PrEP + No VMMC",
                fig.lab.pos = "top.left", fig.lab.size = 12, fig.lab.face = "bold")

ggsave(filename = "unaids_figures/kp_infection_grid.png", device = "png", units = "cm", height = 21, width = 30)

#### distributions of HIV disease burden by age group ####

burden_age <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("TotPaedHIV", "TotHIV15to24","TotHIV25to49",
                          "TotHIV50plus"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("TotPaedHIV", "TotHIV15to24","TotHIV25to49",
                                                  "TotHIV50plus"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2020, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(1.75, "lines")) +
  scale_y_continuous("PLWH (millions)",expand = c(0, 0),labels =(function(l) {round(l/1e6,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("PLWH in each age group") + 
  facet_wrap(~factor(modeled_scenario,
                     # level = c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ))
ggsave(burden_age, filename = "unaids_figures/burden_age.png", device = "png", units = "cm", height = 10, width = 22)

proportion_burden_age <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("TotPaedHIV", "TotHIV15to24","TotHIV25to49",
                          "TotHIV50plus"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("TotPaedHIV", "TotHIV15to24","TotHIV25to49",
                                                  "TotHIV50plus"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2020, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(1.75, "lines")) +
  scale_y_continuous("Proportion of total PLWH (%)",expand = c(0, 0),labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("Proportion of total PLWH in each age group") + 
  facet_wrap(~factor(modeled_scenario,
                     # level = c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ))
ggsave(proportion_burden_age, filename = "unaids_figures/proportion_burden_age.png", device = "png", units = "cm", height = 10, width = 22)

#### proportion of total PLWH by each age and sex group ####

proportion_burden_age_sex <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("TotPaedHIV", "TotHIV15to24F", "TotHIV15to24M", "TotHIV25to49F",
                          "TotHIV25to49M", "TotHIV50plusF", "TotHIV50plusM"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("TotPaedHIV", "TotHIV15to24F", "TotHIV15to24M", "TotHIV25to49F",
                                                  "TotHIV25to49M", "TotHIV50plusF", "TotHIV50plusM"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity", position = "fill") +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2020, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(1.75, "lines")) +
  scale_y_continuous("Proportion of new HIV infections (%)",expand = c(0, 0),labels =(function(l) {round(l*1e2,2)})) +
  scale_fill_brewer("Age and sex", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24 Female", "15-24 Male", 
                               "25-49 Female", "25-49 Male", "50+ Female", "50+ Male"), 
                    palette = "Paired", direction = -1) + 
  ggtitle("Proportion of total PLWH by age and sex group") + 
  facet_wrap(~factor(modeled_scenario))
proportion_burden_age_sex
ggsave(proportion_burden_age_sex, filename = "unaids_figures/proportion_burden_age_sex.png", device = "png", units = "cm", height = 10, width = 22)

#### total PLWH by each age and sex group ####

burden_age_sex <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("TotPaedHIV", "TotHIV15to24F", "TotHIV15to24M", "TotHIV25to49F",
                          "TotHIV25to49M", "TotHIV50plusF", "TotHIV50plusM"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("TotPaedHIV", "TotHIV15to24F", "TotHIV15to24M", "TotHIV25to49F",
                                                  "TotHIV25to49M", "TotHIV50plusF", "TotHIV50plusM"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2020, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(1.75, "lines")) +
  scale_y_continuous("PLWH (millions)",expand = c(0, 0), labels =(function(l) {round(l/1e6,2)})) +
  scale_fill_brewer("Age and sex", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24 Female", "15-24 Male", 
                               "25-49 Female", "25-49 Male", "50+ Female", "50+ Male"), 
                    palette = "Paired", direction = -1) + 
  ggtitle("PLWH in each age and sex group") + 
  facet_wrap(~factor(modeled_scenario))
burden_age_sex
ggsave(burden_age_sex, filename = "unaids_figures/burden_age_sex.png", device = "png", units = "cm", height = 10, width = 22)

distribution_by_age_sex <- ggarrange(proportion_infections_age_sex, new_infections_age_sex, ncol =1, 
                                     common.legend = TRUE, legend = "right")
ggsave(distribution_by_age_sex, filename = "unaids_figures/distribution_by_age_sex.png", device = "png", units = "cm", height = 16, width = 24)

#### incidences, proportions of new infection, new infections ####

# 3 x 5 grid 
# columns: Status quo, No PrEP, No PrEP + No VMMC, 95-95-95, 95-95-95 + No PrEP + No VMMMC
# row 1: incidences by age group (0to14, 15-24, 24-49, 50+)
# row 2: proportions of infections (0to14, 15-24, 24-49, 50+)
# row 3: absolute new infections (0to14, 15-24, 24-49, 50+) 

HIVinc_grid <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("HIVinc0to14","HIVinc15to24", "HIVinc25to49", "HIVinc50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "95-95-95",
                                 "95-95-95 + No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}), expand = c(0, 0)) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence by age \n(per 1000)") + facet_wrap(~indicator, nrow = 1, scales = "free")

HIVinc_grid
ggsave(plot = HIVinc_grid, filename = "unaids_figures/HIVinc_grid.png", device = "png", units = "cm", height = 17, width = 20)


HIVincFSW_grid <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincFSW",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "95-95-95",
                                 "95-95-95 + No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; FSW)") 

HIVincFSW_grid
ggsave(plot = HIVincFSW_grid, filename = "unaids_figures/HIVincFSW_grid.png", device = "png", units = "cm", height = 17, width = 20)


HIVincMSM_grid <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "HIVincMSM",
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                 "95-95-95",
                                 "95-95-95 + No PrEP + No VMMC")
  ) %>% 
  ggplot(aes(year, mean, group = modeled_scenario, fill = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = modeled_scenario), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; MSM)") 

HIVincMSM_grid
ggsave(plot = HIVincMSM_grid, filename = "unaids_figures/HIVincMSM_grid.png", device = "png", units = "cm", height = 17, width = 20)

HIVincGrid <- ggarrange(HIVinc15to49_grid, HIVincFSW_grid, HIVincMSM_grid)


new_infection_age <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_area(aes()) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2020, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(1.75, "lines")) +
  scale_y_continuous("New HIV infections (thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"), 
                    labels = c("0-14","15-24", 
                               "25-49", "50+"), 
                    palette = "Set1") + 
  ggtitle("New HIV infections by age group") + 
  facet_wrap(~factor(modeled_scenario,
                     # level = c("Status quo", "No PrEP", "No PrEP + No VMMC")
  ))


####  new infection line, new infections stacked, proportion new infections ####
new_infection_line <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo", "No PrEP", "No PrEP + No VMMC", "95-95-95", "95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour=indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        plot.margin = margin(r = 0.5,unit = "cm")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"),
                    labels = c("0-14","15-24",
                               "25-49", "50+"),
                    palette = "Set1") +
  ggtitle("") + 
  facet_wrap(~factor(modeled_scenario), nrow = 1, scales = "free_y")

new_infection_line_status_quo <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("Status quo")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour=indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"),
                    labels = c("0-14","15-24",
                               "25-49", "50+"),
                    palette = "Set1") +
  ggtitle("")

new_infection_line_NoPrEP <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("No PrEP")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour=indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"),
                    labels = c("0-14","15-24",
                               "25-49", "50+"),
                    palette = "Set1") +
  ggtitle("")

new_infection_line_NoPrEPNoVMMC <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour=indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"),
                    labels = c("0-14","15-24",
                               "25-49", "50+"),
                    palette = "Set1") +
  ggtitle("")

new_infection_line_art95 <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("95-95-95")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour=indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"),
                    labels = c("0-14","15-24",
                               "25-49", "50+"),
                    palette = "Set1") +
  ggtitle("")

new_infection_line_art95_NoPrEPNoVMMC <- all_scenarios %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                          "NewHIV50"),
         year >= 2020, 
         test_reduction %in% c(0), 
         modeled_scenario %in% c("95-95-95 + No PrEP + No VMMC")
  ) %>%  
  mutate(indicator = factor(indicator, levels = c("NewHIVU15", "NewHIV15to24","NewHIV25to49",
                                                  "NewHIV50"))) %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour=indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines")) +
  scale_y_continuous("New HIV infections\n(thousands)",expand = c(0, 0),labels =(function(l) {round(l/1e3,2)})) +
  scale_fill_brewer("Age group", aesthetics = c("colour", "fill"),
                    labels = c("0-14","15-24",
                               "25-49", "50+"),
                    palette = "Set1") +
  ggtitle("")
#### grid infections by age 1####

HIVinfectionsByAge1 <- ggarrange(new_infection_age,proportion_infection_age, new_infection_line,
          # widths = c(1.15, 1, 1), heights = c(1.15, 1,1),
          ncol =1, legend = "bottom", common.legend = TRUE)

annotate_figure(HIVinfectionsByAge1,
                fig.lab = "Status quo                             No PrEP                            No PrEP + No VMMC                95-95-95                      95-95-95 + No PrEP + No VMMC",
                fig.lab.pos = "top.left", fig.lab.size = 12, fig.lab.face = "bold")

ggsave(filename = "unaids_figures/HIVinfectionsByAge_grid1.png", device = "png", units = "cm", height = 21, width = 30)

#### grid infections by age ####

HIVinfectionsByAge <- ggarrange(new_infection_line_status_quo, 
          new_infection_line_NoPrEP, 
          new_infection_line_NoPrEPNoVMMC, 
          new_infection_line_art95, 
          new_infection_line_art95_NoPrEPNoVMMC, 
          new_infection_age_status_quo,
          new_infection_age_NoPrEP, 
          new_infection_age_NoPrEPNoVMMC,
          new_infection_age_art95,
          new_infection_age_art95NoPrEPNoVMMC,
          proportion_infection_age_status_quo,
          proportion_infection_age_NoPrEP,
          proportion_infection_age_NoPrEPNoVMMC,
          proportion_infection_age_art95,
          proportion_infection_age_art95_NoPrEPNoVMMC,
          # widths = c(1.15, 1, 1), heights = c(1.15, 1,1),
          ncol =5, nrow = 3, legend = "bottom", common.legend = TRUE)

annotate_figure(HIVinfectionsByAge,
                top = "                    Status quo                              No PrEP                             No PrEP + No VMMC                        95-95-95                   95-95-95 + No PrEP + VMMC",
                left = "Proportion of total new infections       Total new infections       New infections")


ggsave(filename = "unaids_figures/HIVinfectionsByAge_grid.png", device = "png", units = "cm", height = 13.46, width = 30.48)
