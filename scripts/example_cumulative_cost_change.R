# load packages

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

# read dataframes
cost_summary <- read_csv("~/Documents/clean_results/cost_summary.csv")
cumulative_change_from_baseline_total <- read_csv("results/cumulative_change_from_baseline_total.csv")


#### Figure 2A ####

### annual costs by component over time ####

# plot one graph of annual costs for each  of the HTS reductions

# 25% HTS reduction - annual costs
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


# 50% HTS reduction - annual costs

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

# 75% hts reduction - annual costs

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

# 100% hts reduction - annual costs

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


#### combine each annual cost graph together ####
annual_cost_combo <- ggarrange(annual_cost_25, annual_cost_50, annual_cost_75, annual_cost_100,
                               ncol =1, common.legend = TRUE, legend = "right", align = "h")

# annotate to add title 
annual_cost_combo <- annotate_figure(annual_cost_combo, top = text_grob("Annual cost difference\n (undiscounted)", 
                                                                        color = "black", face = "bold", size = 11.5, hjust = 0.19))

#### Figure 2B ####

# produce one graph for each row of 25%, 50%, 75% & 100% HTS reduction with columns for discount rate

# 25% HTS reduction - cumulative change
percent_change_25 <- cumulative_change_from_baseline_total %>% 
  filter(test_reduction == 25,
         indicator %in% c("Testing", "Treatment", "Inpatient")) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, percentage_change_baseline, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-6, 6, 3),
                     limits = c(-6,7),
                     labels = (function(l) {paste0(l,"%")})) + 
  geom_point(data = filter(cumulative_change_from_baseline_total,
                           indicator %in% c("Total"),
                           scenario == "Total",
                           test_reduction == 25) %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, percentage_change_baseline, colour = scenario), shape = 15) +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_discrete_manual("",
                        breaks = c("Total", "Testing", "Treatment", "Inpatient"), 
                        values = c("Total" = "black", "Testing" = "#d95f02", "Treatment" = "#1b9e77", "Inpatient" = "#7570b3"), 
                        labels= c("Total", "Testing", "Treatment", "Inpatient"), 
                        aesthetics = c("fill", "colour", "shape")) +
  scale_x_discrete(" ", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.15, "cm"),
        legend.text.align = 0,
        strip.text = element_text(size = 10, vjust = 0),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = "Undiscounted",
                                      "3%" = "3% discount",
                                      "6%" = "6% discount",
                                      "8.25%" = "8.25% discount")),
             strip.position = "top")

# 50% HTS reduction - cumulative change
percent_change_50 <- cumulative_change_from_baseline_total %>% 
  filter(test_reduction == 50,
         indicator %in% c("Testing", "Treatment", "Inpatient")) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, percentage_change_baseline, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-6, 6, 3),
                     limits = c(-6,7),
                     labels = (function(l) {paste0(l,"%")})) + 
  geom_point(data = filter(cumulative_change_from_baseline_total,
                           indicator %in% c("Total"),
                           scenario == "Total",
                           test_reduction == 50) %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, percentage_change_baseline, colour = scenario), shape = 15) +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_discrete_manual("",
                        breaks = c("Total", "Testing", "Treatment", "Inpatient"), 
                        values = c("Total" = "black", "Testing" = "#d95f02", "Treatment" = "#1b9e77", "Inpatient" = "#7570b3"), 
                        labels= c("Total", "Testing", "Treatment", "Inpatient"), 
                        aesthetics = c("fill", "colour", "shape")) +
  scale_x_discrete(" ", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.15, "cm"),
        strip.text = element_text(size = 10, vjust = 0),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = " ",
                                      "3%" = " ",
                                      "6%" = " ",
                                      "8.25%" = " ")),
             strip.position = "top")

# 75% HTS reduction - cumulative change
percent_change_75 <- cumulative_change_from_baseline_total %>% 
  filter(test_reduction == 75,
         indicator %in% c("Testing", "Treatment", "Inpatient")) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, percentage_change_baseline, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-6, 6, 3),
                     limits = c(-6,7),
                     labels = (function(l) {paste0(l,"%")})) + 
  geom_point(data = filter(cumulative_change_from_baseline_total,
                           indicator %in% c("Total"),
                           scenario == "Total",
                           test_reduction == 75) %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, percentage_change_baseline, colour = scenario), shape = 15) +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_discrete_manual("",
                        breaks = c("Total", "Testing", "Treatment", "Inpatient"), 
                        values = c("Total" = "black", "Testing" = "#d95f02", "Treatment" = "#1b9e77", "Inpatient" = "#7570b3"), 
                        labels= c("Total", "Testing", "Treatment", "Inpatient"), 
                        aesthetics = c("fill", "colour", "shape")) +
  scale_x_discrete(" ", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.15, "cm"),
        strip.text = element_text(size = 10, vjust = 0),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%", "8.25%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = " ",
                                      "3%" = " ",
                                      "6%" = " ",
                                      "8.25%" = " ")),
             strip.position = "top")

# 100% HTS reduction - cumulative change
percent_change_100 <- cumulative_change_from_baseline_total %>% 
  filter(test_reduction == 100,
         indicator %in% c("Testing", "Treatment", "Inpatient")) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, percentage_change_baseline, fill = indicator)) +
  geom_bar(aes(), position =  "stack", stat = "identity", 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     breaks = seq(-6, 6, 3),
                     limits = c(-6,7),
                     labels = (function(l) {paste0(l,"%")})) + 
  geom_point(data = filter(cumulative_change_from_baseline_total,
                           indicator %in% c("Total"),
                           scenario == "Total",
                           test_reduction == 100) %>% 
               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                      test_reduction = as.factor(test_reduction),
                      cumulative_years = as.factor(cumulative_years)),
             aes(cumulative_years, percentage_change_baseline, colour = scenario), shape = 15) +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  scale_discrete_manual("",
                        breaks = c("Total", "Testing", "Treatment", "Inpatient"), 
                        values = c("Total" = "black", "Testing" = "#d95f02", "Treatment" = "#1b9e77", "Inpatient" = "#7570b3"), 
                        labels= c("Total", "Testing", "Treatment", "Inpatient"), 
                        aesthetics = c("fill", "colour", "shape")) +
  scale_x_discrete("Cumulative years from 2025", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,-0.4), "cm"),
        legend.spacing.y = unit(-0.15, "cm"),
        strip.text = element_text(size = 10, vjust = 0),
        strip.background = element_blank(),
        strip.placement = "inside") +
  facet_wrap(~factor(discount,levels = c("undiscounted", "3%", "6%")), 
             scale = "free_x", nrow = 1,
             labeller = as_labeller(c("undiscounted" = " ",
                                      "3%" = " ",
                                      "6%" = " ")),
             strip.position = "top")

#### combine each row of cumulative change ####

percent_change <- ggarrange(percent_change_25, percent_change_50, percent_change_75, percent_change_100,
                            ncol =1, common.legend = TRUE, legend = "right", align = "h")


#### annotate cumulative change figure ####
percent_change <- annotate_figure(percent_change, top = text_grob("Percentage of baseline\ncumulative cost", 
                                                                  color = "black", face = "bold", size = 11.5, hjust = 0.70))


#### Combined Figure 2 ####
# combining annual and cumulative costs 

percent_combo <- ggarrange(annual_cost_combo, percent_change, ncol = 2, common.legend = TRUE, legend = "right",
                           widths = c(4.175,8.3), heights = c(4.175,8.3), align = "hv", labels = "AUTO",label.x = 0.08)

ggsave(plot = percent_combo, filename = "figures/percent_combo.png", device = "png", 
       units = "cm", height = 20, width = 20)

