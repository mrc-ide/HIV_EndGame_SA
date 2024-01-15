#### inpatient care cost breakdown ####


#### 75 years ####
cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(25),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(25),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,
                            test_reduction %in% c(25),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
                     ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(50),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(50),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,
                            test_reduction %in% c(50),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
  ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(75),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(75),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,
                            test_reduction %in% c(75),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
  ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         test_reduction %in% c(100),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,
                          test_reduction %in% c(100),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,
                            test_reduction %in% c(100),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
  ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

#### 5 years ####
cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2030,
         test_reduction %in% c(25),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,year <=2030,
                          test_reduction %in% c(25),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,year <=2030,
                            test_reduction %in% c(25),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
                     ) + 
  scale_x_continuous(" ") + theme_classic() +
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2030,,
         test_reduction %in% c(50),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,year <=2030,
                          test_reduction %in% c(50),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,year <=2030,
                            test_reduction %in% c(50),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
  ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2030,
         test_reduction %in% c(75),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,year <=2030,
                          test_reduction %in% c(75),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,year <=2030,
                            test_reduction %in% c(75),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
  ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2030,
         test_reduction %in% c(100),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_bar(aes(colour = indicator), position = "stack", stat = "identity",show.legend=T) + 
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  geom_line(data = filter(cost_summary,
                          indicator %in% c("cost_total_care"),
                          scenario %in% c("annual_absolute_dif"),
                          year >= 2025,year <=2030,
                          test_reduction %in% c(100),
                          discount == "undiscounted") %>%
              mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                     test_reduction = as.factor(test_reduction)),
            aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  geom_ribbon(data = filter(cost_summary,
                            indicator %in% c("cost_total_care"),
                            scenario %in% c("annual_absolute_dif"),
                            year >= 2025,year <=2030,
                            test_reduction %in% c(100),
                            discount == "undiscounted") %>%
                mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                       test_reduction = as.factor(test_reduction)),
              aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
              show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
  ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  scale_linetype_manual(values = c("annual_absolute_dif" = "dashed"), 
                        name = "",
                        breaks = c("annual_absolute_dif"),
                        labels = c("Inpatient total")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))


#### 10 years ####


#### lines ####

#### 75 years ####

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <=2100,
         !test_reduction %in% c(0),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour = indicator), stat = "identity",show.legend=T) + 
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  # geom_line(data = filter(cost_summary,
  #                         indicator %in% c("cost_total_care"),
  #                         scenario %in% c("annual_absolute_dif"),
  #                         year >= 2025,
  #                         test_reduction %in% c(25),
  #                         discount == "undiscounted") %>%
  #             mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
  #                    test_reduction = as.factor(test_reduction)),
  #           aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  # geom_ribbon(data = filter(cost_summary,
  #                           indicator %in% c("cost_total_care"),
  #                           scenario %in% c("annual_absolute_dif"),
  #                           year >= 2025,
  #                           test_reduction %in% c(25),
  #                           discount == "undiscounted") %>%
  #               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
  #                      test_reduction = as.factor(test_reduction)),
  #             aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
  #             show.legend = FALSE, alpha = 0.25, fill = "black") +
  scale_y_continuous(" ",
                     labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                     # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
  ) + 
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))

#### 25 years ####

cost_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(grepl("cost_inpatient", indicator) |
           indicator == "cost_palliative_care",
         scenario %in% c("annual_absolute_dif"), 
         pitc_reduction_year == 2025,
         year >= 2025,
         year <2050,
         !test_reduction %in% c(0),
         discount == "undiscounted") %>% 
  ggplot(aes(year, mean, fill = indicator)) +
  geom_line(aes(colour = indicator), stat = "identity",show.legend=T) + 
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.25) +
  geom_hline(aes(yintercept = 0), lty = "solid", colour = "black") +
  # geom_line(data = filter(cost_summary,
  #                         indicator %in% c("cost_total_care"),
  #                         scenario %in% c("annual_absolute_dif"),
  #                         year >= 2025,
  #                         test_reduction %in% c(25),
  #                         discount == "undiscounted") %>%
  #             mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
  #                    test_reduction = as.factor(test_reduction)),
  #           aes(year, mean, fill = NULL, linetype = scenario), show.legend=FALSE, color = "black") +
  # geom_ribbon(data = filter(cost_summary,
  #                           indicator %in% c("cost_total_care"),
#                           scenario %in% c("annual_absolute_dif"),
#                           year >= 2025,
#                           test_reduction %in% c(25),
#                           discount == "undiscounted") %>%
#               mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
#                      test_reduction = as.factor(test_reduction)),
#             aes(year, ymin = lower_CI, ymax = upper_CI, fill = NULL, linetype = scenario), 
#             show.legend = FALSE, alpha = 0.25, fill = "black") +
scale_y_continuous(" ",
                   labels = (function(l) {paste0("$",round(l/1e6,1),"m")}),
                   # breaks = seq(-400e6, 400e6, 200e6), limits = c(-300e6, 400e6)
) + 
  scale_x_continuous(" ", breaks = seq(2025, 2100, 5)) + theme_classic() +
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
        legend.spacing.y = unit(-0.20, "cm"),
        legend.justification="left"
  ) +
  scale_fill_brewer("",
                    palette = "Paired",
                    direction = -1, 
                    aesthetics = c("colour", "fill"), 
                    labels = c("On ART; CD4 200-349",
                               "On ART; CD4 350-499",
                               "On ART; CD4 >500",
                               "On ART; CD4 <200",
                               "Pre-ART; CD4 200-349",
                               "Pre-ART; CD4 350-499",
                               "Pre-ART; CD4 >500", 
                               "Pre-ART; CD4 <200",
                               "Palliative care")) +
  ggtitle(" ") +
  facet_wrap(~test_reduction, ncol =1, scale = "free_x", labeller = as_labeller(c("25" = "25% General\nHTS reduction",
                                                                                  "50" = "50% General\nHTS reduction",
                                                                                  "75" = "75% General\nHTS reduction",
                                                                                  "100" = "100% General\nHTS reduction")),
             strip.position = "left") + 
  theme(plot.margin = unit(c(0,0.3,0,0), "cm"))



cost_summary %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "annual_percent_change", 
         pitc_reduction_year == 2025, 
         indicator == "TotalHIVtests",
         year >= 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100), 
         discount == "undiscounted") %>% 
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
  scale_y_continuous("Change from baseline (%)",expand = c(0, 0)) +
  scale_fill_brewer("General\nHTS\nreduction", labels = c("None", "25%", "50%", "75%", "100%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Relative change from status quo\n(% of status quo)")

cost_summary %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "annual_absolute_dif", 
         pitc_reduction_year == 2025, 
         indicator == "TotalHIVtests",
         year >= 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100), 
         discount == "undiscounted") %>% 
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
  scale_y_continuous("Difference from baseline\n(million HIV tests)",labels = (function(l) {round(l/1e6,1)}), expand = c(0, 0)) +
  scale_fill_brewer("General\nHTS\nreduction", labels = c("None", "25%", "50%", "75%", "100%"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Absoltute difference from status quo\n(% of status quo)")
