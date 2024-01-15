#### plotting incidence at 2100 and HIV elimination year ####
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

setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")
system("./thembisa")

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

cumulative_prep <- read_csv("results/cumulative_prep.csv")
prep_summary <- read_csv("results/prep.csv")


cumulative_costs <- bind_rows(cumulative_costs_0, cumulative_costs_25, 
                              cumulative_costs_50, cumulative_costs_75,
                              cumulative_costs_100)
write_csv(cumulative_costs, "~/Documents/clean_results/cumulative_costs.csv")


cost_summary_0 <- read_csv("results/costs_0.csv")
cost_summary_25 <- read_csv("results/costs_25.csv")
cost_summary_50 <- read_csv("results/costs_50.csv")
cost_summary_75 <- read_csv("results/costs_75.csv")
cost_summary_100 <- read_csv("results/costs_100.csv")

cost_summary <- bind_rows(cost_summary_0, cost_summary_25,
                          cost_summary_50, cost_summary_75,
                          cost_summary_100)

write_csv(cost_summary, "~/Documents/clean_results/cost_summary.csv")

cost_summary <- read_csv("~/Documents/clean_results/cost_summary.csv")
cumulative_costs <- read_csv("~/Documents/clean_results/cumulative_costs.csv")
# plwh_summary <- read_csv("results/plwh_on_art.csv")
# smoothed annual costs over time

annual_costs <- cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("intervention"), 
         pitc_reduction_year == 2025,
         year >= 2020,
         year <=2100,
         test_reduction %in% c(0, 25, 50, 75, 100),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity") + 
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")})) + 
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("colour", "fill")) +
  scale_linetype_manual(values = c("intervention" = "dashed"), 
                        name = "",
                        breaks = c("intervention"),
                        labels = c("Total")) +
  scale_x_continuous("Years\n", breaks = seq(2025, 2100, 25)) + theme_classic() +
  theme(axis.text.y = element_text(size = 11), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1,
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm")
  ) +
  ggtitle("Annual cost\nUndiscounted") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", 
             labeller = as_labeller(c(
               "0" = "Status quo\ngeneral HTS",
               "25" = "25% general\nHTS reduction",
               "50" = "50% general\nHTS reduction",
               "75" = "75% general\nHTS reduction",
               "100" = "100% general\nHTS reduction")),
             strip.position = "left")
ggsave(plot = annual_costs, "figures/annual_cost_dif.png", device = "png", units = "cm", height = 29.7, width = 20)

## difference from baseline - undiscounted
figA <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0,
         discount == "undiscounted") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean, group = indicator, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
           ) +
  scale_y_continuous("",
                     breaks = seq(-3e9, 4e9, 1e9), 
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")}),
                     limits = c(-3.5e9, 4.5e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction != 0,
                           discount == "undiscounted") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           discount == "undiscounted",
                           test_reduction != 0) %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  scale_shape_discrete("", labels = "Total") +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("Cumulative\nyears", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside") +
  ggtitle("Cumulative cost\ndifference\nUndiscounted\n(US$ billions)") +
  facet_wrap(~test_reduction, scale = "free_x", ncol =1, labeller = as_labeller(c("25" = "25% general\nHTS reduction",
                                                                "50" = "50% general\nHTS reduction",
                                                                "75" = "75% general\nHTS reduction",
                                                                "100" = "100% general\nHTS reduction")))
## difference from baseline - 3% discounted
figB <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0,
         discount == "3%") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean, group = indicator, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-3e9, 4e9, 1e9), 
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")}),
                     limits = c(-3.5e9, 4.5e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction != 0,
                           discount == "3%") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("absolute_dif"),
                              cumulative_years %in% c(4, 9, 24, 49),
                              discount == "3%",
                              test_reduction != 0) %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(cumulative_years, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  scale_shape_discrete("", labels = "Total") +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("Cumulative\nyears", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside") +
  ggtitle("Cumulative cost\ndifference\n3% Discounted\n(US$ billions)") +
  facet_wrap(~test_reduction, scale = "free_x", ncol =1, labeller = as_labeller(c("25" = "25% general\nHTS reduction",
                                                                                  "50" = "50% general\nHTS reduction",
                                                                                  "75" = "75% general\nHTS reduction",
                                                                                  "100" = "100% general\nHTS reduction")))

## difference from baseline - 3% discounted
figC <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0,
         discount == "6%") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean, group = indicator, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-3e9, 4e9, 1e9), 
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")}),
                     limits = c(-3.5e9, 4.5e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction != 0,
                           discount == "6%") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("absolute_dif"),
                              cumulative_years %in% c(4, 9, 24, 49),
                              discount == "6%",
                              test_reduction != 0) %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(cumulative_years, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  scale_shape_discrete("", labels = "Total") +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("Cumulative\nyears", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside") +
  ggtitle("Cumulative cost\ndifference\n6% Discounted\n(US$ billions)") +
  facet_wrap(~test_reduction, scale = "free_x", ncol =1, labeller = as_labeller(c("25" = "25% general\nHTS reduction",
                                                                                  "50" = "50% general\nHTS reduction",
                                                                                  "75" = "75% general\nHTS reduction",
                                                                                  "100" = "100% general\nHTS reduction")))
#### cumulative costs ####
cum_costs_25_red <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction == 25, 
         discount != "8.25%") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-4e9, 4e9, 2e9), 
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")}),
                     limits = c(-3.5e9, 4.5e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction == 25, 
                           discount != "8.25%") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("absolute_dif"),
                              cumulative_years %in% c(4, 9, 24, 49),
                              test_reduction == 25, 
                              discount != "8.25%") %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(cumulative_years, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  scale_shape_discrete("", labels = "Total") +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete(" ", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.36, "cm"),
        strip.text = element_text(size = 10, vjust = -0.25),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = "Undiscounted",
                                      "3%" = "3% discount",
                                      "6%" = "6% discount",
                                      "8.25%" = "8.25% discount")),
             strip.position = "top")

cum_costs_50_red <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction == 50, 
         discount != "8.25%") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-4e9, 4e9, 2e9), 
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")}),
                     limits = c(-3.5e9, 4.5e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction == 50, 
                           discount != "8.25%") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("absolute_dif"),
                              cumulative_years %in% c(4, 9, 24, 49),
                              test_reduction == 50, 
                              discount != "8.25%") %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(cumulative_years, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  scale_shape_discrete("", labels = "Total") +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete(" ", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.36, "cm"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = " ",
                                      "3%" = " ",
                                      "6%" = " ",
                                      "8.25%" = " ")),
             strip.position = "top")

cum_costs_75_red <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction == 75, 
         discount != "8.25%") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-4e9, 4e9, 2e9), 
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")}),
                     limits = c(-3.5e9, 4.5e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction == 75, 
                           discount != "8.25%") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("absolute_dif"),
                              cumulative_years %in% c(4, 9, 24, 49),
                              test_reduction == 75, 
                              discount != "8.25%") %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(cumulative_years, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  scale_shape_discrete("", labels = "Total") +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete(" ", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.36, "cm"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = " ",
                                      "3%" = " ",
                                      "6%" = " ",
                                      "8.25%" = " ")),
             strip.position = "top")

cum_costs_100_red <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction == 100, 
         discount != "8.25%") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-4e9, 4e9, 2e9), 
                     labels = (function(l) {paste0("$",round(l/1e9,1),"b")}),
                     limits = c(-3.5e9, 4.5e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction == 100, 
                           discount != "8.25%") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("absolute_dif"),
                              cumulative_years %in% c(4, 9, 24, 49),
                              test_reduction == 100, 
                              discount != "8.25%") %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(cumulative_years, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  scale_shape_discrete("", labels = "Total") +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("Cumulative years", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.36, "cm"),
        strip.text = element_text(size = 10),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = " ",
                                      "3%" = " ",
                                      "6%" = " ",
                                      "8.25%" = " ")),
             strip.position = "top")

cum_costs <- ggarrange(cum_costs_25_red, cum_costs_50_red,cum_costs_75_red,cum_costs_100_red, ncol =1, 
          common.legend = TRUE, legend = "right")
cum_costs <- annotate_figure(cum_costs, top = text_grob("Cumulative cost difference\n", 
                                          color = "black", face = "bold", size = 11.5, hjust = 0.70))
cum_costs
ggsave(plot = cum_costs, filename = "figures/cum_costs.png", device = "png", 
       units = "cm", height = 29, width = 20)

### annual costs ####

annual_cost_25 <- cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(25),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=FALSE) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(25),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                          indicator %in% c("cost_total"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(25),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)) + 
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("colour", "fill")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Total")) +
  scale_x_continuous(" ", breaks = seq(2025, 2100, 25)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 10, hjust = 0.5,vjust = -0.30),
        aspect.ratio=1,
        strip.text = element_text(size = 11, vjust = 1, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm"),
        legend.justification="left"
  ) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

annual_cost_50 <- cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(50),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=FALSE) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(50),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,
                            test_reduction %in% c(50),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)) + 
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("colour", "fill")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Total")) +
  scale_x_continuous(" ", breaks = seq(2025, 2100, 25)) + theme_classic() +
  theme(axis.text.y = element_text(size = 11), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 10, hjust = 0.5,vjust = -0.15),
        aspect.ratio=1,
        strip.text = element_text(size = 11, vjust = 1, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm"),
        legend.justification="left"
  ) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

annual_cost_75 <- cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(75),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=FALSE) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(75),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,
                            test_reduction %in% c(75),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)) + 
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("colour", "fill")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Total")) +
  scale_x_continuous(" ", breaks = seq(2025, 2100, 25)) + theme_classic() +
  theme(axis.text.y = element_text(size = 11), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 10, hjust = 0.5,vjust = -0.15),
        aspect.ratio=1,
        strip.text = element_text(size = 11, vjust = 1, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm"),
        legend.justification="left"
  ) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

annual_cost_100 <- cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(100),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=FALSE) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(100),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,
                            test_reduction %in% c(100),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)) + 
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("colour", "fill")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Total")) +
  scale_x_continuous(" ", breaks = seq(2025, 2100, 25)) + theme_classic() +
  theme(axis.text.y = element_text(size = 11), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 10, hjust = 0.5,vjust = -0.15),
        aspect.ratio=1,
        strip.text = element_text(size = 11, vjust = 1, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm"),
        legend.justification="left"
  ) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

annual_cost_25
annual_cost_50
annual_cost_75
annual_cost_100

annual_cost_combo <- ggarrange(annual_cost_25, annual_cost_50, annual_cost_75, annual_cost_100,
                               ncol =1, common.legend = TRUE, legend = "right", align = "h")
annual_cost_combo <- annotate_figure(annual_cost_combo, top = text_grob("Annual cost difference\n (undiscounted)", 
                                          color = "black", face = "bold", size = 11.5, hjust = 0.19))

annual_cost_combo
ggsave(plot = annual_cost_combo, filename = "figures/annual_cost_combo.png", device = "png", 
       units = "cm", height = 29, width = 20)

#### combining annual and cumulative costs ####

percent_combo <- ggarrange(annual_cost_combo, percent_change, ncol = 2, common.legend = TRUE, legend = "right",
                           widths = c(4.175,8.3), heights = c(4.175,8.3), align = "hv", labels = "AUTO",label.x = 0.08)

ggsave(plot = percent_combo, filename = "figures/percent_combo.png", device = "png", 
       units = "cm", height = 20, width = 20)

big_combo <- ggarrange(annual_cost_combo, cum_costs, ncol = 2, common.legend = TRUE, legend = "right",
                       widths = c(4,8.3), heights = c(4,8.3), align = "hv", labels = "AUTO")
ggsave(plot = big_combo, filename = "figures/big_combo.png", device = "png", 
       units = "cm", height = 20, width = 20)

## difference from baseline - undiscounted, 3%, 6% discounted

figAC <- ggarrange(figA, figB, figC, nrow = 1, ncol = 3, common.legend = TRUE, 
                   legend = "right", widths = c(1.27,1,1.00), heights = c(1.27,1,1.00))

ggsave(plot = figAC, filename = "figures/cost_dif_by_hts_dif_disc.png", device = "png", units = "cm", 
       height = 29.7, width = 20)

fullfig <- ggarrange(annual_cost_dif, figA, figB, figC, nrow = 1, ncol = 4, 
                     labels = "AUTO", common.legend = TRUE,legend = "right",
                     widths = c(2.1,1.45,1,1), heights = c(1.95,1.45,1,1))
ggsave(plot = fullfig, filename = "figures/full_cost_dif_by_hts.png", device = "png", units = "cm", 
       height = 18, width = 20)

## percentage change from baseline - undiscounted

cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("percent_change"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0,
         discount == "undiscounted") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(test_reduction, mean, group = indicator, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("Percentage of status quo cost (%)") + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("percent_change"),
                           cumulative_years %in% c(4, 9, 24, 49),
                           test_reduction != 0,
                           discount == "undiscounted") %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(test_reduction, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("percent_change"),
                              cumulative_years %in% c(4, 9, 24, 49),
                              discount == "undiscounted",
                              test_reduction != 0) %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(test_reduction, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  geom_hline(aes(yintercept = 0), lty = "dotted") +
  scale_shape_discrete("", labels = "Total") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("General\nHTS\nreduction", labels = c(25, 50, 75, 100)) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm")) +
  ggtitle("Realtive cost difference from status quo\nUndiscounted") +
  facet_wrap(~cumulative_years, nrow =1, labeller = as_labeller(c("4" = "5 years",
                                                                                     "9" = "10 years",
                                                                                     "24" = "25 years",
                                                                                     "49" = "50 years")))


## difference from baseline 50 years
cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("absolute_dif"),
         cumulative_years %in% c(49),
         test_reduction != 0) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(test_reduction, mean, group = indicator, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("Cost (US$,billions)",
                     labels = (function(l) {round(l/1e9,1)}),                       breaks = seq(-2e9, 4e9, 2e9)) + 
  geom_point(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                           scenario %in% c("absolute_dif"),
                           cumulative_years %in% c(49),
                           test_reduction != 0) %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(test_reduction, mean, shape = "scenario"), fill = "black") +
  geom_errorbar(data = filter(cumulative_costs,indicator %in% c("cost_total"),
                              scenario %in% c("absolute_dif"),
                              cumulative_years %in% c(49),
                              test_reduction != 0) %>% 
                  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                         test_reduction = as.factor(test_reduction),
                         cumulative_years = as.factor(cumulative_years)),
                aes(test_reduction, ymin = lower_CI, ymax = upper_CI, fill = NULL), width = 0.10, colour = "black", alpha = 0.5, show.legend = FALSE) +
  geom_hline(aes(yintercept = 0), lty = "dotted") +
  scale_shape_discrete("", labels = "Total") +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("General\nHTS\nreduction", labels = c(25, 50, 75, 100)) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0),
        legend.spacing.y = unit(-0.36, "cm")) +
  ggtitle("Change from baseline\nCumulative costs over 50 years") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), nrow =1, scales = "free_y", labeller = as_labeller(c("undiscounted" = "Undiscounted",
                                                                                                                                     "3%" = "Discounted 3%",
                                                                                                                                     "6%" = "Discounted 6%",
                                                                                                                                     "8.25%" = "Discounted 8.25%")))

# facet_wrapped plots

cumulative_costs %>% 
  filter(indicator %in% c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care"),
         discount == "undiscounted",
         scenario %in% c("percent_change"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(test_reduction, mean, group = cumulative_years, fill = cumulative_years)) +
  geom_col(aes(color = cumulative_years), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("Change from baseline (%)") + 
  scale_color_brewer("Cumulative years",
                     palette = "Dark2",
                     direction = -1, 
                     labels = c(5, 10, 25, 50), 
                     aesthetics = c("colour", "fill")) +
  scale_x_discrete("General\nHTS\nreduction", labels = c(25, 50, 75, 100)) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Percent change from baseline\nCumulative costs\nUndiscounted") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_total_testing" = "Testing",
                                                                          "cost_total_treatment" = "Treatment",
                                                                          "cost_total_care" = "Care",
                                                                          "cost_total" = "Total")))

cumulative_costs %>% 
  filter(indicator %in% c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care"),
         discount == "3%",
         scenario %in% c("percent_change"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(test_reduction, mean, group = cumulative_years, fill = cumulative_years)) +
  geom_col(aes(color = cumulative_years), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("Change from baseline (%)") + 
  scale_color_brewer("Cumulative years",
                     palette = "Dark2",
                     direction = -1, 
                     labels = c(5, 10, 25, 50), 
                     aesthetics = c("colour", "fill")) +
  scale_x_discrete("General\nHTS\nreduction", labels = c(25, 50, 75, 100)) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Percent change from baseline\nCumulative costs\n3% discounted") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_total_testing" = "Testing",
                                                                     "cost_total_treatment" = "Treatment",
                                                                     "cost_total_care" = "Care",
                                                                     "cost_total" = "Total")))

cumulative_costs %>% 
  filter(indicator %in% c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care"),
         discount == "6%",
         scenario %in% c("percent_change"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(test_reduction, mean, group = cumulative_years, fill = cumulative_years)) +
  geom_col(aes(color = cumulative_years), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("Change from baseline (%)") + 
  scale_color_brewer("Cumulative years",
                     palette = "Dark2",
                     direction = -1, 
                     labels = c(5, 10, 25, 50), 
                     aesthetics = c("colour", "fill")) +
  scale_x_discrete("General\nHTS\nreduction", labels = c(25, 50, 75, 100)) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Percent change from baseline\nCumulative costs\n6% discounted") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_total_testing" = "Testing",
                                                                     "cost_total_treatment" = "Treatment",
                                                                     "cost_total_care" = "Care",
                                                                     "cost_total" = "Total")))

cumulative_costs %>% 
  filter(indicator %in% c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care"),
         discount == "8.25%",
         scenario %in% c("percent_change"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction != 0) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(test_reduction, mean, group = cumulative_years, fill = cumulative_years)) +
  geom_col(aes(color = cumulative_years), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("Change from baseline (%)") + 
  scale_color_brewer("Cumulative years",
                     palette = "Dark2",
                     direction = -1, 
                     labels = c(5, 10, 25, 50), 
                     aesthetics = c("colour", "fill")) +
  scale_x_discrete("General\nHTS\nreduction", labels = c(25, 50, 75, 100)) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Percent change from baseline\nCumulative costs\n8.25% discounted") + 
  facet_wrap(~indicator, scales = "free_y", labeller = as_labeller(c("cost_total_testing" = "Testing",
                                                                     "cost_total_treatment" = "Treatment",
                                                                     "cost_total_care" = "Care",
                                                                     "cost_total" = "Total")))

annual_cost_line <- cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(indicator %in% c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care"),
         scenario %in% c("intervention"), 
         pitc_reduction_year == 2025,
         year >= 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         discount == "undiscounted") %>% 
  mutate(indicator = factor(indicator, levels = c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care"))) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0), breaks = seq(2025, 2100, 25)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.text = element_text(size = 10, vjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside") +
  scale_y_continuous("Cost (US$ millions)", labels =(function(l) {paste0("$",round(l/1e6,1),"m")}),expand = c(0, 0)) +
  scale_fill_brewer("General\nHTS\nreduction", 
                    labels = c("None\n(Status quo)", "25%,", "50%", "75%", "100%"), 
                    aesthetics = c("colour", "fill"), 
                    palette = "Set1",
                    direction = 1) + 
  ggtitle("Annual costs (2023 US$ millions)\n(Undiscounted)") + 
  facet_wrap(~indicator, scale = "free", 
             nrow = 1,
             strip.position = "top",
             labeller = as_labeller(c("cost_total_testing" = "Testing",
                                      "cost_total_treatment" = "Treatment",
                                      "cost_total_care" = "Inpatient",
                                      "cost_total" = "Total")))

ggsave(plot = annual_cost_line, filename = "figures/annual_cost_line.png", width = 20, device = "png", units = "cm")

total_annual_costs <- ggarrange(annual_cost_line, annual_percentage_fig,
          ncol =1, align = "h", labels = "AUTO")

ggsave(plot = total_annual_costs, 
       filename = "figures/total_annual_costs.png", 
       width = 20, 
       height = 20,
       device = "png", 
       units = "cm")

cost_summary %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"), 
         test_reduction %in% c(100),
         discount == "undiscounted",
         scenario %in% c("intervention", "baseline"),
         cumulative_years == 9) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(indicator, mean, group = scenario, fill = scenario)) +
  geom_col(aes(color = scenario), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("", 
                     labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Scenario",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Scenario", palette = "Dark2",direction = -1) +
  scale_x_discrete(name = " ", labels = c("Care", "Testing", "Total", "Treatment")) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Cumulative costs\nover 10-years\n(millions)")

costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"), 
         test_reduction %in% c(100),
         discount == "undiscounted",
         scenario %in% c("intervention", "baseline"),
         cumulative_years == 24) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(indicator, mean, group = scenario, fill = scenario)) +
  geom_col(aes(color = scenario), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("", 
                     labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Scenario",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Scenario", palette = "Dark2",direction = -1) +
  scale_x_discrete(name = " ", labels = c("Care", "Testing", "Treatment")) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Cumulative costs\nover 25-years\n(millions)")

costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"), 
         test_reduction %in% c(100),
         discount == "undiscounted",
         scenario %in% c("intervention", "baseline"),
         cumulative_years == 49) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(indicator, mean, group = scenario, fill = scenario)) +
  geom_col(aes(color = scenario), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("", 
                     labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Scenario",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Scenario", palette = "Dark2",direction = -1) +
  scale_x_discrete(name = " ", labels = c("Care", "Testing", "Treatment")) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Cumulative costs\nover 50-years\n(millions)")

costs %>% 
  filter(indicator %in% c("cost_total_testing", "cost_total_treatment", "cost_total_care"), 
         test_reduction %in% c(100),
         discount == "undiscounted",
         scenario %in% c("intervention", "baseline"),
         cumulative_years == 74) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(indicator, mean, group = scenario, fill = scenario)) +
  geom_col(aes(color = scenario), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.34, show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("", 
                     labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Scenario",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Scenario", palette = "Dark2",direction = -1) +
  scale_x_discrete(name = " ", labels = c("Care", "Testing", "Treatment")) + theme_classic() +
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        plot.margin = margin(0,0,0,0)) +
  ggtitle("Cumulative costs\nover 75-years\n(millions)")

plwh_on_art_25_htc_red <- read_csv("results/plwh_on_art_25_htc_red.csv")
plwh_on_art_0_25_100_htc_red <- read_csv("results/plwh_on_art_0_25_100_htc_red.csv")
plwh_on_art_0_25_100_htc_red <- bind_rows(plwh_on_art_0_25_100_htc_red, plwh_on_art_25_htc_red)
write_csv(plwh_on_art_0_25_100_htc_red, "THEMBISAv18/results/plwh_on_art.csv")

plwh_cumulative <- read_csv("results/cumulative_plwh_on_art_red.csv")
plwh_summary <- read_csv("results/plwh_on_art_red.csv")

setwd("~/Documents/HIV_EndGame_SA")
write_csv(plwh_summary,"results/cost_analysis.csv")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")

costs_only <- plwh_summary %>% 
  filter(year >= 2025, 
         year <= 2100, 
         scenario == "intervention", 
         grepl("cost_", indicator)) %>% 
  select(year, indicator, test_reduction, mean, lower_CI, upper_CI)

costs_only <- costs_only %>% 
  mutate(indicator = sub("^cost_", "", costs_only$indicator))%>% 
  rename("cost" = "indicator")

select_cost_indicators <- plwh_summary %>% 
  filter(year >= 2025,
         year <= 2100, 
         scenario == "intervention") %>% 
  select(year, indicator, test_reduction, mean) %>% 
  filter(indicator %in% c("ANCtestNeg", "ANCtestPos", "StartingART_15",
                          "Adult_1st_line_FU", "Total2LAdultsOnART",
                          "StartingART10to14", "FollowUpART10to14", 
                          "StartingART1to2", "FollowUpART1to2", 
                          "StartingART3to5", "FollowUpART3to5",
                          "StartingART6to9", "FollowUpART6to9",
                          "StartingART1", "StartingART0", "AdultHIVtestsNeg",
                          "AdultHIVtestsPos", "OnART200to349", "OnART350to499",
                          "OnARTover500", "OnARTunder200", 
                          "PreART200to349M", "PreART200to349F", "DiscART200to349",
                          "PreART350to499M", "PreART350to499F", "DiscART350to499",
                          "PreARTover500M", "PreARTover500F", "DiscARTover500",
                          "PreARTunder200M","PreARTunder200F", "DiscARTunder200",
                          "TotalAIDSdeathsadult"
                          )) %>% 
  pivot_wider(names_from = indicator , values_from = mean) %>%
  mutate(PreART200to349 = PreART200to349M + PreART200to349F + DiscART200to349,
         PreART350to499 = PreART350to499M + PreART350to499F + DiscART350to499,
         PreARTover500 = PreARTover500M + PreARTover500F + DiscARTover500,
         PreARTunder200 = PreARTunder200M + PreARTunder200F + DiscARTunder200) %>% 
  select(-c(PreART200to349M, PreART200to349F, DiscART200to349, 
            PreART350to499M, PreART350to499F, DiscART350to499,
            PreARTover500M, PreARTover500F, DiscARTover500,
            PreARTunder200M, PreARTunder200F, DiscARTunder200)) %>% 
  pivot_longer(-(year:test_reduction), names_to = "indicator", values_to = "mean")
  
select_cost_indicators <- select_cost_indicators %>% 
  mutate(indicator = factor(indicator, 
                            levels = c("ANCtestNeg", "ANCtestPos", "StartingART_15",
                                       "Adult_1st_line_FU", "Total2LAdultsOnART",
                                       "StartingART10to14", "FollowUpART10to14", 
                                       "StartingART1to2", "FollowUpART1to2", 
                                       "StartingART3to5", "FollowUpART3to5",
                                       "StartingART6to9", "FollowUpART6to9",
                                       "StartingART1", "StartingART0", "AdultHIVtestsNeg",
                                       "AdultHIVtestsPos", "OnART200to349", "OnART350to499",
                                       "OnARTover500", "OnARTunder200", 
                                       "PreART200to349","PreART350to499",
                                       "PreARTover500", "PreARTunder200",
                                       "TotalAIDSdeathsadult"),
                            labels = c("ANCtestNeg", "ANCtestPos", "StartingART_15",
                                       "Adult_1st_line_FU", "Total2LAdultsOnART",
                                       "StartingART10to14", "FollowUpART10to14", 
                                       "StartingART1to2", "FollowUpART1to2", 
                                       "StartingART3to5", "FollowUpART3to5",
                                       "StartingART6to9", "FollowUpART6to9",
                                       "StartingART1", "StartingART0", "AdultHIVtestsNeg",
                                       "AdultHIVtestsPos", "OnART200to349", "OnART350to499",
                                       "OnARTover500", "OnARTunder200", 
                                       "PreART200to349","PreART350to499",
                                       "PreARTover500","PreARTunder200", 
                                       "TotalAIDSdeathsadult"))) %>% 
  arrange(indicator)
costs_only <- costs_only %>% mutate(cost = as.factor(cost)) %>% arrange(cost)
select_cost_indicators <- select_cost_indicators %>% rename(mean_value = mean)
costs_only <- costs_only %>% select(-c(year, test_reduction, lower_CI, upper_CI)) %>% 
  rename(mean_cost = mean)
combined_units_costs <- cbind(select_cost_indicators, costs_only)

write_csv(combined_units_costs, "results/hts_reduction_costs.csv")
install.packages("writexl")
library(writexl)
write_xlsx(combined_units_costs, "results/hts_reduction_costs.xlsx")

plwh_summary %>% 
  arrange(test_reduction) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "baseline", 
         pitc_reduction_year == 2025, 
         indicator == "TotalAdultsOnART",
         year >= 2020, 
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = F) +
  scale_x_continuous("",expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on ART", labels =(function(l) {paste(round(l/1e6,1), "million")}),expand = c(0, 0)) +
  scale_fill_manual(name = "General\nHTS\nreduction", breaks = c(0, 25, 50, 75, 100), values = c(`0` = "black", `25` = "#377eb8", `50` = "#4daf4a", `75` = "#984ea3",  `100` = "#ff7f00"), 
                    labels= c("None", 25, 50, 75, 100), aesthetics = c("colour", "fill")) + 
  ggtitle("Adults on ART\n(15+ years)")
ggsave("unaids_figures/art_need.png", device = "png", units = "cm", height = 17, width = 20)


plwh_summary %>% 
  arrange(test_reduction) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "baseline", 
         pitc_reduction_year == 2025, 
         indicator == "StartingART_15",
         year >= 2020, 
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = F) +
  scale_x_continuous("",expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults starting ART", labels =(function(l) {paste(round(l/1e3,1), "thousand")}),expand = c(0, 0)) +
  scale_fill_manual(name = "General\nHTS\nreduction", breaks = c(0, 25, 50, 75, 100), values = c(`0` = "black", `25` = "#377eb8", `50` = "#4daf4a", `75` = "#984ea3",  `100` = "#ff7f00"), 
                    labels= c("None", 25, 50, 75, 100), aesthetics = c("colour", "fill")) + 
  ggtitle("Adults starting ART\n(15+ years)")
ggsave("unaids_figures/starting_art_.png", device = "png", units = "cm", height = 17, width = 20)

art_needs_costs %>% 
  arrange(test_reduction) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "cost_art_adults_1st_line_1st_yr",
         year >= 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous("",expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Annual cost (US$)", labels =(function(l) {paste(round(l/1e6,1))}),expand = c(0, 0)) +
  scale_fill_manual(name = "General\nHTS\nreduction", breaks = c(0, 25, 50, 75, 100), values = c(`0` = "black", `25` = "#377eb8", `50` = "#4daf4a", `75` = "#984ea3",  `100` = "#ff7f00"), 
                    labels= c("None", 25, 50, 75, 100), aesthetics = c("colour", "fill")) + 
  ggtitle("Cost: Adult 1st-line 1st-year\n(US$ millions; 15+ years)")
ggsave("unaids_figures/cost_art_adults_1st_line_1st_yr.png", device = "png", units = "cm", height = 17, width = 20)

plwh_on_art_0_25_100_htc_red %>% filter(indicator == "TotalAdultsOnART", 
                                    year %in% c(2075), 
                                    scenario == "intervention", 
                                    test_reduction%in% c(0, 25, 100),
                                    pitc_reduction_year == 2025) %>%
  mutate(mean = round(mean/10**6,2), 
         lower_CI = round(lower_CI/10**6,2), 
         upper_CI = round(upper_CI/10**6,2)) %>% 
  select(year, test_reduction, mean, lower_CI, upper_CI) %>% 
  arrange(test_reduction)

plwh_on_art_0_25_100_htc_red %>% arrange(test_reduction) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TotalAdultsOnART",
         year >= 2020, 
         test_reduction %in% c(0)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = F) +
  scale_x_continuous("",expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on ART (millions)", labels =(function(l) {paste(round(l/1e6,1), "million")}),expand = c(0, 0)) +
  scale_fill_manual(name = "General\nHTS\nreduction", breaks = c(0, 25, 100), values = c(`0` = "black", `25` = "red", `100` = "#377eb8"), 
                    labels= c("None", "25%", "100%"), aesthetics = c("colour", "fill")) + 
  ggtitle("Adults on ART\n(15+ years)")
ggsave("unaids_figures/baseline_art_need.png", device = "png", units = "cm", height = 13, width = 15)

plwh_summary %>% 
  arrange(test_reduction) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator %in% c("cost_art_adults_1st_line_1st_yr", "cost_art_adults_1st_line_fu", 
                          "cost_art_adults_2nd_line"),
         year >= 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous("",expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults starting ART (thousands)", labels =(function(l) {paste(round(l/1e3,1), "thousand")}),expand = c(0, 0)) +
  scale_fill_manual(name = "General\nHTS\nreduction", breaks = c(0, 25, 50, 75, 100), values = c(`0` = "black", `25` = "#377eb8", `50` = "#4daf4a", `75` = "#984ea3",  `100` = "#ff7f00"), 
                    labels= c("None", 25, 50, 75, 100), aesthetics = c("colour", "fill")) + 
  ggtitle("Adults starting ART\n(15+ years)") +
  facet_wrap(facets = "indicator", scales = "free_y")
ggsave("unaids_figures/starting_art_.png", device = "png", units = "cm", height = 17, width = 20)





plwh_on_art_0_25_100_htc_red %>% arrange(test_reduction) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TotalAdultsOnART",
         year >= 2020, 
         test_reduction %in% c(0, 25)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = F) +
  scale_x_continuous("",expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 16), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 16), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on ART (millions)", labels =(function(l) {paste(round(l/1e6,1), "million")}),expand = c(0, 0)) +
  scale_fill_manual(name = "General\nHTS\nreduction", breaks = c(0, 25, 100), values = c(`0` = "black", `25` = "#377eb8", `100` = "#ff7f00"), 
                    labels= c("None", "25%", "100%"), aesthetics = c("colour", "fill")) + 
  ggtitle("Adults on ART\n(15+ years)")
ggsave("unaids_figures/art_numbers25red.png", device = "png", units = "cm", height = 13, width = 15)

plwh_on_art_0_25_100_htc_red %>% arrange(test_reduction) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TotalAdultsOnART",
         year >= 2020, 
         test_reduction %in% c(0, 25, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  scale_x_continuous("",expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Adults on ART (millions)", labels =(function(l) {paste(round(l/1e6,1), "million")}),expand = c(0, 0)) +
  scale_fill_manual(name = "General\nHTS\nreduction", 
                    breaks = c("baseline", "0", "25", "50", "75", "100"), values = c( "0" ="black", "25" = "#e41a1c","50" =  "#377eb8", "75" = "#4daf4a","100" =  "#984ea3"), labels= c("Status quo", "None", 25, 50, 75, 100), 
                    aesthetics = c("colour", "fill")) +
  ggtitle("Adults on ART\n(15+ years)")
ggsave("unaids_figures/art_numbers100red.png", device = "png", units = "cm", height = 13, width = 15)




#### figure 1 single plots ####

fig1a <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per \n1000 (15-49 years)", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) 
  
suppfig1a <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
    filter(scenario == "intervention", 
           indicator == "HIVinc15to49",
           year > 2020, 
           test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
    ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
    geom_line(aes(colour = test_reduction), show.legend = T) +
    geom_hline(aes(yintercept = 0.001), lty = "dotted") +
    xlab("") +
    expand_limits(y=0) + theme_classic() + 
    theme(axis.text = element_text(size = 12), 
          axis.title.y = element_text(size = 12), 
          axis.title.x = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_y_continuous("HIV incidence per 1000 (15-49 years)", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
    scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
    scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
    theme(legend.title = element_blank()) + 
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                                `2030` = "Testing reduced in 2030",
                                                                `2035` = "Testing reduced in 2035",
                                                                `2040` = "Testing reduced in 2040", 
                                                                `2045` = "Testing reduced in 2045", 
                                                                `2050` = "Testing reduced in 2050")))

fig1b <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (15-49 years)") +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank())

suppfig1b <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (15-49 years)") +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) +
facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                              `2030` = "Testing reduced in 2030",
                                                              `2035` = "Testing reduced in 2035",
                                                              `2040` = "Testing reduced in 2040", 
                                                              `2045` = "Testing reduced in 2045", 
                                                              `2050` = "Testing reduced in 2050")))

fig1c <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (over 15 years)", limits = c(0.5, 1)) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) 

suppfig1c <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (over 15 years)", limits = c(0.5, 1)) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) + 
facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                              `2030` = "Testing reduced in 2030",
                                                              `2035` = "Testing reduced in 2035",
                                                              `2040` = "Testing reduced in 2040", 
                                                              `2045` = "Testing reduced in 2045", 
                                                              `2050` = "Testing reduced in 2050")))


fig1d <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV tests per 100 (over 15 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) 

suppfig1d <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV tests per 100 (over 15 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) +
facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                              `2030` = "Testing reduced in 2030",
                                                              `2035` = "Testing reduced in 2035",
                                                              `2040` = "Testing reduced in 2040", 
                                                              `2045` = "Testing reduced in 2045", 
                                                              `2050` = "Testing reduced in 2050")))

#### figure 1 panel ####
  
grid.arrange(fig1a, fig1b, fig1c, fig1d)



   
#### plotting incidence only ####
library(tidyr)  

incidence_change <- data.frame(expand_grid(pitc_reduction_year = unique(test_reduction_only$pitc_reduction_year), 
                                             pitc_reduction_percentage = unique(test_reduction_only$test_reduction),
                                           year = NA,
                                             first_mean = NA,
                                             first_lower_ci = NA,
                                             first_upper_ci = NA,
                                             second_mean = NA, 
                                             second_lower_ci = NA,
                                             second_upper_ci = NA))




for (i in 1:nrow(incidence_change)){
  incidences <- test_reduction_only %>% filter(scenario == "intervention",
                                               indicator == "HIVinc15to49",
                                               year == incidence_change$pitc_reduction_year[i],
                                 pitc_reduction_year == incidence_change$pitc_reduction_year[i],
                                 test_reduction == incidence_change$pitc_reduction_percentage[i])
  incidence_change$year[i] <- incidences$year
  incidence_change$first_mean[i] <- incidences$mean
  incidence_change$first_lower_ci[i] <- incidences$lower_CI
  incidence_change$first_upper_ci[i] <- incidences$upper_CI
  
  incidences2 <- test_reduction_only %>% filter(scenario == "intervention",
                                               indicator == "HIVinc15to49",
                                               year == incidence_change$pitc_reduction_year[i]+50,
                                               pitc_reduction_year == incidence_change$pitc_reduction_year[i],
                                               test_reduction == incidence_change$pitc_reduction_percentage[i])
  incidence_change$year[i] <- incidences2$year
  incidence_change$second_mean[i] <- incidences2$mean
  incidence_change$second_lower_ci[i] <- incidences2$lower_CI
  incidence_change$second_upper_ci[i] <- incidences2$upper_CI
  
}

incidence_change <- incidence_change %>% mutate(change_mean = first_mean - second_mean,
                                                change_lower_ci = first_lower_ci - second_lower_ci,
                                                change_upper_ci = first_upper_ci - second_upper_ci)
incidence_change %>% 
  filter(pitc_reduction_percentage == 0 | pitc_reduction_percentage == 20 | pitc_reduction_percentage == 80 | pitc_reduction_percentage == 100) %>%
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         pitc_reduction_percentage = as.factor(pitc_reduction_percentage)) %>% 
  ggplot(aes(pitc_reduction_year, change_mean, group = pitc_reduction_percentage,color = pitc_reduction_percentage)) + 
  geom_crossbar(aes(ymin = change_lower_ci, ymax = change_upper_ci, fill = pitc_reduction_percentage),alpha = 0.5, show.legend = T,width = 0.1) +
  geom_point(aes(color = pitc_reduction_percentage), shape = 18, size =2, show.legend = T, color = "white") + 
  # scale_fill_discrete(labels = c("No testing reduction", "20% testing reduction", "80% testing reduction", "100% testing reduction")) + 
  # scale_color_discrete(labels = c("No testing reduction", "20% testing reduction", "80% testing reduction", "100% testing reduction")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.title = element_blank()) + 
  scale_y_continuous("Change in HIV incidence per 1000 (15-49 years) over 50 years", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) + 
  xlab("Testing reduction year")

test_reduction_cumulative <- read_csv("results/cumulative_test_reduction_only_summary.csv")

# infections bar plot ####

test_reduction_cumulative %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif", 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(test_reduction, mean, group = pitc_reduction_year, fill = pitc_reduction_year)) +
  geom_col(aes(color = pitc_reduction_year), position =  position_dodge(width = 1), alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), show.legend = F, position =  position_dodge(width = 1), alpha = 0.5) +
  scale_y_continuous("Additional HIV infections (millions)", 
                     labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Testing \nreduction \nyear",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Testing \nreduction \nyear", palette = "Dark2",direction = -1) +
  xlab("Testing reduction (%)") + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 13),
        legend.text = element_text(size = 12)) + theme_classic()

# line graph ####

test_reduction_cumulative %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "intervention") %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  ggplot(aes(test_reduction, mean, group = pitc_reduction_year, fill = pitc_reduction_year)) +
  geom_line(aes(color = pitc_reduction_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.1) +
  scale_color_brewer("Testing \nreduction \nyear",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Testing \nreduction \nyear", palette = "Dark2",direction = -1) +
  xlab("Testing reduction (%)") + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 13),
        legend.text = element_text(size = 12)) + theme_classic()

# deaths
# bar plot
test_reduction_cumulative %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif", 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(test_reduction, mean, group = pitc_reduction_year, fill = pitc_reduction_year)) +
  geom_col(aes(color = pitc_reduction_year), position =  position_dodge(width = 1), alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), show.legend = F, position =  position_dodge(width = 1), alpha = 0.5) +
  scale_y_continuous("Additional AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Testing \nreduction \nyear",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Testing \nreduction \nyear", palette = "Dark2",direction = -1) +
  xlab("Testing reduction (%)") + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 13),
        legend.text = element_text(size = 12)) + theme_classic()

test_reduction_cumulative %>% 
  filter(indicator == "TotalAIDSdeathsadult",
         scenario == "absolute_dif", test_reduction != 10, test_reduction !=30, test_reduction !=50, test_reduction !=70, test_reduction !=90) %>% 
           ggplot(aes(as.factor(test_reduction), mean, group = pitc_reduction_year, fill = as.factor(pitc_reduction_year))) +
           geom_crossbar(aes(ymin = lower_CI, ymax = upper_CI, color = as.factor(pitc_reduction_year)), show.legend = T,width = 0.5, position = position_dodge(width = 1), alpha = 0.5) +
           geom_point(aes(color = as.factor(pitc_reduction_year)), shape = 18, size =3, position = position_dodge(width = 1), show.legend = F, color = "white") + theme_classic() +
           scale_y_continuous("Additional AIDS-related deaths (millions)", 
                              labels = (function(l) {round(l/1e6,1)})) +
           scale_color_discrete(name = "Testing \nreduction \nyear", type ="viridis") +
           scale_fill_discrete(name = "Testing \nreduction \nyear", type ="viridis") +
           xlab("Testing reduction (%)") + theme(axis.text = element_text(size = 14), 
                                              axis.title.y = element_text(size = 13), 
                                              axis.title.x = element_text(size = 13),
                                              legend.text = element_text(size = 12))
         


test_reduction_only_inc_elim <- find_inc_and_elimination(test_reduction_only)
write_csv(test_reduction_only_inc_elim, "results/test_reduction_only_inc_elim.csv")

#### HIV elimination year ####

test_reduction_only_inc_elim <- read_csv("results/test_reduction_only_inc_elim.csv")

test_reduction_only_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                        pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = pitc_reduction_year, color = pitc_reduction_year)) + 
  geom_line(aes(color = pitc_reduction_year)) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10) + expand_limits(y = 2100) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("Testing \nreduction \nyear")

#### HIV incidence at 2100 line graph ####

test_reduction_only_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = pitc_reduction_year, fill = pitc_reduction_year)) + 
  geom_line(aes(color = pitc_reduction_year)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = pitc_reduction_year), alpha = 0.05, show.legend = F) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("Testing \nreduction \nyear")

#### HIV incidence in 2100 as bar chart ####

test_reduction_only_inc_elim %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         pitc_reduction_percentage = as.factor(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_percentage %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = pitc_reduction_year, fill = pitc_reduction_year)) + 
  geom_col(aes(color = pitc_reduction_year), position =  position_dodge(width = 1), alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), show.legend = F, position =  position_dodge(width = 1), alpha = 0.5) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_brewer("Testing \nreduction \nyear",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Testing \nreduction \nyear", palette = "Dark2",direction = -1)


# HIV incidence after 50 years

incidence_change %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  ggplot(aes(pitc_reduction_percentage, second_mean, group = pitc_reduction_year, fill = pitc_reduction_year)) + 
  geom_line(aes(color = pitc_reduction_year)) +
  geom_ribbon(aes(ymin = second_lower_ci, ymax = second_upper_ci, fill = pitc_reduction_year), alpha = 0.05, show.legend = F) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years)\n50 years after testing reduction", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("Testing \nreduction \nyear")


#### decreasing ART retention ####

art_change_values <- seq(0, 8, 2)

for (i in art_change_values){
  run_on_cluster(pitc_reduction_years = seq(2025, 2050, 5), 
                 pitc_reduction_percentage = seq(0,100,10),
                 condom_usage_reduction = FALSE, 
                 fsw_condom_usage_decrease = 0,
                 st_condom_usage_decrease = 0, 
                 lt_condom_usage_decrease = 0,
                 condom_incr_start = 2025,
                 art_coverage_increase = FALSE,
                 art_interrupt_rate_decrease = 2/100,
                 art_incr_start = 2025,
                 summary_name = paste0("decrease_art_retention_with_test_reduction", i),
                 cumulative_years = 50,
                 art_coverage_decrease = TRUE,
                 art_interrupt_rate_increase = i/100,
                 art_decr_start = 2025
  )
}

filepaths <- paste0("results/decrease_art_retention_with_test_reduction", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
decrease_art_retention_with_test_reduction <- bind_rows(temp, .id = "art_int_reduction")

write_csv(decrease_art_retention_with_test_reduction, "results/decrease_art_retention_with_test_reduction_comb.csv")
decrease_art_retention_with_test_reduction <- read_csv("results/decrease_art_retention_with_test_reduction_comb.csv")

decrease_art_retention_with_test_reduction %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, indicator == "HIVinc15to49", year > 1990) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") + 
  xlab("") +
  scale_y_continuous("HIV incidence per 1000 (15-49 years)", labels = (function(l) {round(l*1e3,1)})) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 13), 
        axis.title.x = element_text(size = 13)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No Testing reduction",
                                                           "10" = "10% Testing reduction",
                                                           "20" = "20% Testing reduction",
                                                           "30" = "30% Testing reduction",
                                                           "40" = "40% Testing reduction",
                                                           "50" = "50% Testing reduction",
                                                           "60" = "60% Testing reduction",
                                                           "70" = "70% Testing reduction",
                                                           "80" = "80% Testing reduction",
                                                           "90" = "90% Testing reduction",
                                                           "100" = "100% Testing reduction")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \nTesting \nonly")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nTesting \nonly")) + theme(legend.title = element_blank())


n_art_int_values <- length(art_change_values)
art_reduction_dfs <- replicate(n_art_int_values, tibble())
for (i in 1:n_art_int_values) {
  art_reduction_dfs[[i]] <- find_inc_and_elimination(filter(decrease_art_retention_with_test_reduction, 
                                                            art_int_reduction == art_change_values[i]))
}
names(art_reduction_dfs) <- art_change_values
art_reduction_inc_elim <- bind_rows(art_reduction_dfs, .id = "art_int_reduction")


# plotting 
art_reduction_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                        pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = art_int_reduction, color = art_int_reduction)) + 
  geom_line(aes(color = art_int_reduction)) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10, na.value = 2200,limits = c(2040, 2100)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("ART \ninterruption \nrate \nincrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                                `2030` = "Testing reduced in 2030",
                                                                `2035` = "Testing reduced in 2035",
                                                                `2040` = "Testing reduced in 2040", 
                                                                `2045` = "Testing reduced in 2045", 
                                                                `2050` = "Testing reduced in 2050")))

# hiv incidence at 2100
art_reduction_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = art_int_reduction, fill = art_int_reduction)) + 
  geom_line(aes(color = art_int_reduction)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = art_int_reduction), alpha = 0.10, show.legend = F) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("ART \ninterruption \nrate \nincrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                                `2030` = "Testing reduced in 2030",
                                                                `2035` = "Testing reduced in 2035",
                                                                `2040` = "Testing reduced in 2040", 
                                                                `2045` = "Testing reduced in 2045", 
                                                                `2050` = "Testing reduced in 2050")))

#### increasing ART retention ####

art_change_values <- seq(0, 8, 2)

for (i in art_change_values){
  run_on_cluster(pitc_reduction_years = seq(2025, 2050, 5), 
                 pitc_reduction_percentage = seq(0,100,10),
                 condom_usage_reduction = FALSE, 
                 fsw_condom_usage_decrease = 0,
                 st_condom_usage_decrease = 0, 
                 lt_condom_usage_decrease = 0,
                 condom_incr_start = 2025,
                 art_coverage_increase = TRUE,
                 art_interrupt_rate_decrease = i/100,
                 art_incr_start = 2025,
                 summary_name = paste0("increase_art_retention_with_test_reduction", i),
                 cumulative_years = 50,
                 art_coverage_decrease = FALSE,
                 art_interrupt_rate_increase = 2/100,
                 art_decr_start = 2025
  )
}

filepaths <- paste0("results/increase_art_retention_with_test_reduction", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
increase_art_retention_with_test_reduction <- bind_rows(temp, .id = "art_int_reduction")

write_csv(increase_art_retention_with_test_reduction, "results/increase_art_retention_with_test_reduction.csv")
increase_art_retention_with_test_reduction <- read_csv("results/increase_art_retention_with_test_reduction.csv")

increase_art_retention_with_test_reduction %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, indicator == "HIVinc15to49", year > 1990) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") + 
  xlab("") +
  scale_y_continuous("HIV incidence per 1000 (15-49 years)", labels = (function(l) {round(l*1e3,1)})) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 13), 
        axis.title.x = element_text(size = 13)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No Testing reduction",
                                                           "10" = "10% Testing reduction",
                                                           "20" = "20% Testing reduction",
                                                           "30" = "30% Testing reduction",
                                                           "40" = "40% Testing reduction",
                                                           "50" = "50% Testing reduction",
                                                           "60" = "60% Testing reduction",
                                                           "70" = "70% Testing reduction",
                                                           "80" = "80% Testing reduction",
                                                           "90" = "90% Testing reduction",
                                                           "100" = "100% Testing reduction")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \nTesting \nonly")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nTesting \nonly")) + theme(legend.title = element_blank())


n_art_int_values <- length(art_change_values)
art_improvement_dfs <- replicate(n_art_int_values, tibble())
for (i in 1:n_art_int_values) {
  art_improvement_dfs[[i]] <- find_inc_and_elimination(filter(increase_art_retention_with_test_reduction, 
                                                            art_int_reduction == art_change_values[i]))
}
names(art_improvement_dfs) <- art_change_values
art_improvement_inc_elim <- bind_rows(art_improvement_dfs, .id = "art_int_improvement")

# joining art_improvement and art_reduction dfs 

art_change_inc_elim <- find_inc_and_elimination(art_change_summary)

art_change_inc_elim <- art_change_inc_elim %>% mutate(art_int_rate = case_when(
  future_variability == "art_deterioration" ~ (0.22*((1+(as.numeric(future_value)))**10)),
  future_variability == "art_improvement" ~ (0.22*((1-(as.numeric(future_value)))**10))))
art_change_inc_elim <- art_change_inc_elim %>% mutate(art_ret_rate = 1 - art_int_rate)
unique(art_change_inc_elim$art_ret_rate)
write_csv(art_change_inc_elim, "results/art_change_inc_elim.csv")

art_change_inc_elim <- read_csv("results/art_change_inc_elim.csv")

# plotting 
art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                  pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = art_ret_rate, color = art_ret_rate)) + 
  geom_line(aes(color = art_ret_rate)) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10,limits = c(2040, 2100)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_continuous("ART \ninterruption \nrate in 2035",type = "viridis") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                                `2030` = "Testing reduced in 2030",
                                                                `2035` = "Testing reduced in 2035",
                                                                `2040` = "Testing reduced in 2040", 
                                                                `2045` = "Testing reduced in 2045", 
                                                                `2050` = "Testing reduced in 2050")))

# hiv incidence at 2100
art_change_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  #filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = art_change, fill = art_change)) + 
  geom_line(aes(color = art_change)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = art_change), alpha = 0.10, show.legend = F) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("ART \ninterruption \nchange") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                                `2030` = "Testing reduced in 2030",
                                                                `2035` = "Testing reduced in 2035",
                                                                `2040` = "Testing reduced in 2040", 
                                                                `2045` = "Testing reduced in 2045", 
                                                                `2050` = "Testing reduced in 2050")))
#### read in csvs from cluster ####

  # haven't run it on the cluster - will add read_csv() when I do

#### heatmaps ####
library(metR)
library(ggfx)
art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_ret_rate)) + 
  with_blur(
    geom_raster(aes(fill = log(1000*mean_incidence_2100)), interpolate = FALSE),
    sigma = 10
  ) +
  #geom_raster(aes(fill = log(1000*mean_incidence_2100))) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", binwidth = 0.5) + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white") +
  geom_hline(aes(yintercept = 1-0.22), lty = "dotted") +
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
                       ) +
  scale_y_continuous(name ="Female ART retention rate",
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.78, 0.8, 0.9),
                     labels = c("0.1", "0.2", "0.3", "0.4","0.5", "0.6", "0.7","Baseline", "0.8", "0.9"),
                     trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic()

art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year),
                               ) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_ret_rate, z = round(1000*mean_incidence_2100,2))) + 
  geom_raster(aes(fill = round(1000*mean_incidence_2100,2)), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_hline(aes(yintercept = 0.78), lty = "dotted") +
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Mean \nHIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "white",
                       high = "#FF0000", 
                       midpoint = 1) +
  ylab("Female ART interruption rate") + 
  scale_y_continuous(name ="Female ART retention rate", breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.78, 0.8, 0.9),
                     labels = c("0.1", "0.2", "0.3", "0.4","0.5", "0.6", "0.7","Baseline", "0.8", "0.9"),
                     trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic()

art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = art_ret_rate, z = elimination_year)) + 
  with_blur(sigma = 8,stack = TRUE,
    geom_raster(aes(fill = elimination_year), interpolate = TRUE)
  ) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  geom_hline(aes(yintercept = 0.78), lty = "dotted") +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 2055) +
  scale_y_continuous(name ="Female ART retention rate", breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.78, 0.8, 0.9),
                     labels = c("0.1", "0.2", "0.3", "0.4","0.5", "0.6", "0.7","Baseline", "0.8", "0.9"),
                     trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic()



#### condom reduction  ####

#### read csv from cluster ####
condom_reduction_summary <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_reduction_summary.csv")
condom_reduction_inc_elim <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_reduction_inc_elim.csv")
unique_future_values <- unique(condom_reduction_summary$future_value)


#### calculating absolute condom usage values
overall_condom_usage_range <- condom_reduction_summary %>% filter(indicator == "CondomUsage", 
                                                                  test_reduction == 0, pitc_reduction_year == 2025,
                                                                  year == 2035, scenario == "intervention") %>% select(mean)
condom_usage_reduction_labels <- round(overall_condom_usage_range$mean,1)

condom_reduction_summary <- condom_reduction_summary %>% mutate(overall_condom_usage = case_when(future_value == unique_future_values[1] ~ condom_usage_reduction_labels[1],
                                                                                                 future_value == unique_future_values[2] ~ condom_usage_reduction_labels[2],
                                                                                                 future_value == unique_future_values[3] ~ condom_usage_reduction_labels[3],
                                                                                                 future_value == unique_future_values[4] ~ condom_usage_reduction_labels[4],
                                                                                                 future_value == unique_future_values[5] ~ condom_usage_reduction_labels[5],
                                                                                                 future_value == unique_future_values[6] ~ condom_usage_reduction_labels[6],
                                                                                                 future_value == unique_future_values[7] ~ condom_usage_reduction_labels[7],
                                                                                                 future_value == unique_future_values[8] ~ condom_usage_reduction_labels[8],
                                                                                                 future_value == unique_future_values[9] ~ condom_usage_reduction_labels[9],
                                                                                                 future_value == unique_future_values[10] ~ condom_usage_reduction_labels[10],
                                                                                                 future_value == unique_future_values[11] ~ condom_usage_reduction_labels[11],
                                                                                                 future_value == unique_future_values[12] ~ condom_usage_reduction_labels[12],
                                                                                                 future_value == unique_future_values[13] ~ condom_usage_reduction_labels[13],
                                                                                                 future_value == unique_future_values[14] ~ condom_usage_reduction_labels[14],
                                                                                                 future_value == unique_future_values[15] ~ condom_usage_reduction_labels[15],
                                                                                                 future_value == unique_future_values[16] ~ condom_usage_reduction_labels[16],
                                                                                                 future_value == unique_future_values[17] ~ condom_usage_reduction_labels[17],
                                                                                                 future_value == unique_future_values[18] ~ condom_usage_reduction_labels[18],
                                                                                                 future_value == unique_future_values[19] ~ condom_usage_reduction_labels[19],
                                                                                                 future_value == unique_future_values[20] ~ condom_usage_reduction_labels[20],
                                                                                                 future_value == unique_future_values[21] ~ condom_usage_reduction_labels[21],
                                                                                                 future_value == unique_future_values[22] ~ condom_usage_reduction_labels[22],
                                                                                                 future_value == unique_future_values[23] ~ condom_usage_reduction_labels[23],
                                                                                                 future_value == unique_future_values[24] ~ condom_usage_reduction_labels[24],
                                                                                                 future_value == unique_future_values[25] ~ condom_usage_reduction_labels[25],
                                                                                                 future_value == unique_future_values[26] ~ condom_usage_reduction_labels[26],
                                                                                                 future_value == unique_future_values[27] ~ condom_usage_reduction_labels[27],
                                                                                                 future_value == unique_future_values[28] ~ condom_usage_reduction_labels[28],
                                                                                                 future_value == unique_future_values[29] ~ condom_usage_reduction_labels[29],
                                                                                                 future_value == unique_future_values[30] ~ condom_usage_reduction_labels[30]))

condom_reduction_inc_elim <- condom_reduction_inc_elim %>% mutate(overall_condom_usage = case_when(future_value == unique_future_values[1] ~ condom_usage_reduction_labels[1],
                                                                                                   future_value == unique_future_values[2] ~ condom_usage_reduction_labels[2],
                                                                                                   future_value == unique_future_values[3] ~ condom_usage_reduction_labels[3],
                                                                                                   future_value == unique_future_values[4] ~ condom_usage_reduction_labels[4],
                                                                                                   future_value == unique_future_values[5] ~ condom_usage_reduction_labels[5],
                                                                                                   future_value == unique_future_values[6] ~ condom_usage_reduction_labels[6],
                                                                                                   future_value == unique_future_values[7] ~ condom_usage_reduction_labels[7],
                                                                                                   future_value == unique_future_values[8] ~ condom_usage_reduction_labels[8],
                                                                                                   future_value == unique_future_values[9] ~ condom_usage_reduction_labels[9],
                                                                                                   future_value == unique_future_values[10] ~ condom_usage_reduction_labels[10],
                                                                                                   future_value == unique_future_values[11] ~ condom_usage_reduction_labels[11],
                                                                                                   future_value == unique_future_values[12] ~ condom_usage_reduction_labels[12],
                                                                                                   future_value == unique_future_values[13] ~ condom_usage_reduction_labels[13],
                                                                                                   future_value == unique_future_values[14] ~ condom_usage_reduction_labels[14],
                                                                                                   future_value == unique_future_values[15] ~ condom_usage_reduction_labels[15],
                                                                                                   future_value == unique_future_values[16] ~ condom_usage_reduction_labels[16],
                                                                                                   future_value == unique_future_values[17] ~ condom_usage_reduction_labels[17],
                                                                                                   future_value == unique_future_values[18] ~ condom_usage_reduction_labels[18],
                                                                                                   future_value == unique_future_values[19] ~ condom_usage_reduction_labels[19],
                                                                                                   future_value == unique_future_values[20] ~ condom_usage_reduction_labels[20],
                                                                                                   future_value == unique_future_values[21] ~ condom_usage_reduction_labels[21],
                                                                                                   future_value == unique_future_values[22] ~ condom_usage_reduction_labels[22],
                                                                                                   future_value == unique_future_values[23] ~ condom_usage_reduction_labels[23],
                                                                                                   future_value == unique_future_values[24] ~ condom_usage_reduction_labels[24],
                                                                                                   future_value == unique_future_values[25] ~ condom_usage_reduction_labels[25],
                                                                                                   future_value == unique_future_values[26] ~ condom_usage_reduction_labels[26],
                                                                                                   future_value == unique_future_values[27] ~ condom_usage_reduction_labels[27],
                                                                                                   future_value == unique_future_values[28] ~ condom_usage_reduction_labels[28],
                                                                                                   future_value == unique_future_values[29] ~ condom_usage_reduction_labels[29],
                                                                                                   future_value == unique_future_values[30] ~ condom_usage_reduction_labels[30]))
#### condom promotion ####

#### read csv from cluster ####
condom_promotion_summary <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_promotion_summary.csv")
condom_promotion_inc_elim <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_promotion_inc_elim.csv")
unique_future_values <- unique(condom_promotion_summary$future_value)
overall_condom_usage_range <- condom_promotion_summary %>% filter(indicator == "CondomUsage", 
                                                                  test_reduction == 0, pitc_reduction_year == 2025,
                                                                  year == 2035, scenario == "intervention") %>% select(mean)
condom_usage_reduction_labels <- round(overall_condom_usage_range$mean,1)
condom_promotion_summary <- condom_promotion_summary %>% mutate(overall_condom_usage = case_when(future_value == unique_future_values[1] ~ condom_usage_reduction_labels[1],
                                                                                                 future_value == unique_future_values[2] ~ condom_usage_reduction_labels[2],
                                                                                                 future_value == unique_future_values[3] ~ condom_usage_reduction_labels[3],
                                                                                                 future_value == unique_future_values[4] ~ condom_usage_reduction_labels[4],
                                                                                                 future_value == unique_future_values[5] ~ condom_usage_reduction_labels[5],
                                                                                                 future_value == unique_future_values[6] ~ condom_usage_reduction_labels[6],
                                                                                                 future_value == unique_future_values[7] ~ condom_usage_reduction_labels[7],
                                                                                                 future_value == unique_future_values[8] ~ condom_usage_reduction_labels[8],
                                                                                                 future_value == unique_future_values[9] ~ condom_usage_reduction_labels[9],
                                                                                                 future_value == unique_future_values[10] ~ condom_usage_reduction_labels[10],
                                                                                                 future_value == unique_future_values[11] ~ condom_usage_reduction_labels[11],
                                                                                                 future_value == unique_future_values[12] ~ condom_usage_reduction_labels[12],
                                                                                                 future_value == unique_future_values[13] ~ condom_usage_reduction_labels[13],
                                                                                                 future_value == unique_future_values[14] ~ condom_usage_reduction_labels[14],
                                                                                                 future_value == unique_future_values[15] ~ condom_usage_reduction_labels[15],
                                                                                                 future_value == unique_future_values[16] ~ condom_usage_reduction_labels[16],
                                                                                                 future_value == unique_future_values[17] ~ condom_usage_reduction_labels[17],
                                                                                                 future_value == unique_future_values[18] ~ condom_usage_reduction_labels[18],
                                                                                                 future_value == unique_future_values[19] ~ condom_usage_reduction_labels[19],
                                                                                                 future_value == unique_future_values[20] ~ condom_usage_reduction_labels[20],
                                                                                                 future_value == unique_future_values[21] ~ condom_usage_reduction_labels[21],
                                                                                                 future_value == unique_future_values[22] ~ condom_usage_reduction_labels[22],
                                                                                                 future_value == unique_future_values[23] ~ condom_usage_reduction_labels[23],
                                                                                                 future_value == unique_future_values[24] ~ condom_usage_reduction_labels[24],
                                                                                                 future_value == unique_future_values[25] ~ condom_usage_reduction_labels[25],
                                                                                                 future_value == unique_future_values[26] ~ condom_usage_reduction_labels[26],
                                                                                                 future_value == unique_future_values[27] ~ condom_usage_reduction_labels[27],
                                                                                                 future_value == unique_future_values[28] ~ condom_usage_reduction_labels[28],
                                                                                                 future_value == unique_future_values[29] ~ condom_usage_reduction_labels[29],
                                                                                                 future_value == unique_future_values[30] ~ condom_usage_reduction_labels[30]))

condom_promotion_inc_elim <- condom_promotion_inc_elim %>% mutate(overall_condom_usage = case_when(future_value == unique_future_values[1] ~ condom_usage_reduction_labels[1],
                                                                                                   future_value == unique_future_values[2] ~ condom_usage_reduction_labels[2],
                                                                                                   future_value == unique_future_values[3] ~ condom_usage_reduction_labels[3],
                                                                                                   future_value == unique_future_values[4] ~ condom_usage_reduction_labels[4],
                                                                                                   future_value == unique_future_values[5] ~ condom_usage_reduction_labels[5],
                                                                                                   future_value == unique_future_values[6] ~ condom_usage_reduction_labels[6],
                                                                                                   future_value == unique_future_values[7] ~ condom_usage_reduction_labels[7],
                                                                                                   future_value == unique_future_values[8] ~ condom_usage_reduction_labels[8],
                                                                                                   future_value == unique_future_values[9] ~ condom_usage_reduction_labels[9],
                                                                                                   future_value == unique_future_values[10] ~ condom_usage_reduction_labels[10],
                                                                                                   future_value == unique_future_values[11] ~ condom_usage_reduction_labels[11],
                                                                                                   future_value == unique_future_values[12] ~ condom_usage_reduction_labels[12],
                                                                                                   future_value == unique_future_values[13] ~ condom_usage_reduction_labels[13],
                                                                                                   future_value == unique_future_values[14] ~ condom_usage_reduction_labels[14],
                                                                                                   future_value == unique_future_values[15] ~ condom_usage_reduction_labels[15],
                                                                                                   future_value == unique_future_values[16] ~ condom_usage_reduction_labels[16],
                                                                                                   future_value == unique_future_values[17] ~ condom_usage_reduction_labels[17],
                                                                                                   future_value == unique_future_values[18] ~ condom_usage_reduction_labels[18],
                                                                                                   future_value == unique_future_values[19] ~ condom_usage_reduction_labels[19],
                                                                                                   future_value == unique_future_values[20] ~ condom_usage_reduction_labels[20],
                                                                                                   future_value == unique_future_values[21] ~ condom_usage_reduction_labels[21],
                                                                                                   future_value == unique_future_values[22] ~ condom_usage_reduction_labels[22],
                                                                                                   future_value == unique_future_values[23] ~ condom_usage_reduction_labels[23],
                                                                                                   future_value == unique_future_values[24] ~ condom_usage_reduction_labels[24],
                                                                                                   future_value == unique_future_values[25] ~ condom_usage_reduction_labels[25],
                                                                                                   future_value == unique_future_values[26] ~ condom_usage_reduction_labels[26],
                                                                                                   future_value == unique_future_values[27] ~ condom_usage_reduction_labels[27],
                                                                                                   future_value == unique_future_values[28] ~ condom_usage_reduction_labels[28],
                                                                                                   future_value == unique_future_values[29] ~ condom_usage_reduction_labels[29],
                                                                                                   future_value == unique_future_values[30] ~ condom_usage_reduction_labels[30]))



#### joining condom usage dfs ####

condom_change_summary <- bind_rows(condom_promotion_summary, condom_reduction_summary)
condom_change_inc_elim <- bind_rows(condom_promotion_inc_elim, condom_reduction_inc_elim)

# plotting 

condom_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                    pitc_reduction_year = as.factor(pitc_reduction_year), 
                                    overall_condom_usage = as.factor(overall_condom_usage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = overall_condom_usage, color = overall_condom_usage)) + 
  geom_line(aes(color = overall_condom_usage)) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10, na.value = 2200,limits = c(2040, 2100)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("Condom \nusage \nprobability \ndecrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                                `2030` = "Testing reduced in 2030",
                                                                `2035` = "Testing reduced in 2035",
                                                                `2040` = "Testing reduced in 2040", 
                                                                `2045` = "Testing reduced in 2045", 
                                                                `2050` = "Testing reduced in 2050")))
# hiv incidence at 2100
condom_change_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                     overall_condom_usage = as.factor(overall_condom_usage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = overall_condom_usage, fill = overall_condom_usage)) + 
  geom_line(aes(color = overall_condom_usage)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = overall_condom_usage), alpha = 0.10, show.legend = F) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("Condom \nusage \nprobability \ndecrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced in 2025",
                                                                `2030` = "Testing reduced in 2030",
                                                                `2035` = "Testing reduced in 2035",
                                                                `2040` = "Testing reduced in 2040", 
                                                                `2045` = "Testing reduced in 2045", 
                                                                `2050` = "Testing reduced in 2050")))



#### condom heatmaps + contours ####
library(metR)

condom_heatmap_inc <- condom_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                                       pitc_reduction_year = as.factor(pitc_reduction_year),
                                                       overall_condom_usage = as.numeric(overall_condom_usage),
                                                       pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage))

condom_heatmap_inc + 
  with_blur(
    sigma = 10, 
    geom_raster(aes(fill = log(1000*mean_incidence_2100)),interpolate = TRUE)) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black") + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white") +
  geom_hline(aes(yintercept = 33.0), lty = "dotted")+
  scale_fill_gradient2("Mean \nHIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "Baseline", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic()

condom_heatmap_year <- condom_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                                       pitc_reduction_year = as.factor(pitc_reduction_year),
                                                       overall_condom_usage = as.numeric(overall_condom_usage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage, z = elimination_year))

condom_heatmap_year + with_blur(
  geom_raster(aes(fill = elimination_year), interpolate = TRUE),
  sigma = 10) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 2055) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "Baseline", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic()


