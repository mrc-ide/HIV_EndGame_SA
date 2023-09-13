#### cumulative_cost_from_summary ####

# baseline total cost

cost_cum_base <- cost_summary %>% 
  filter(indicator %in% c("cost_total"),
         scenario %in% c("baseline"),
         test_reduction %in% c(25, 50, 75, 100),
         year >= 2025) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value)) %>% 
  group_by(scenario, indicator, pitc_reduction_year, test_reduction, discount) %>% 
  summarise(cumulative_mean = cumsum(mean))
cost_cum_base$year <- c(rep(seq(2025,2100,1), 12))
cost_cum_base <- cost_cum_base %>% 
  filter(year %in% c(2029, 2034, 2049, 2074)) %>% 
  mutate(cumulative_years = year - 2024) %>% 
  relocate(cumulative_years, .before = scenario) 

# intervention total cost 

cost_cum_intervention <- cost_summary %>% 
  filter(indicator %in% c("cost_total"),
         scenario %in% c("annual_absolute_dif"),
         test_reduction %in% c(25, 50, 75, 100),
         year >= 2025) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value)) %>% 
  group_by(scenario, indicator, pitc_reduction_year, test_reduction, discount) %>% 
  summarise(cumulative_mean = cumsum(mean))
cost_cum_intervention$year <- c(rep(seq(2025,2100,1), 12))
cost_cum_intervention <- cost_cum_intervention %>% 
  filter(year %in% c(2029, 2034, 2049, 2074)) %>% 
  mutate(cumulative_years = year - 2024) %>% 
  relocate(cumulative_years, .before = scenario) 

# intervention testing cost

cost_cum_testing <- cost_summary %>% 
  filter(indicator %in% c("cost_total_testing"),
         scenario %in% c("annual_absolute_dif"),
         test_reduction %in% c(25, 50, 75, 100),
         year >= 2025) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value)) %>% 
  group_by(scenario, indicator, pitc_reduction_year, test_reduction, discount) %>% 
  summarise(cumulative_mean = cumsum(mean))
cost_cum_testing$year <- c(rep(seq(2025,2100,1), 12))
cost_cum_testing <- cost_cum_testing %>% 
  filter(year %in% c(2029, 2034, 2049, 2074)) %>% 
  mutate(cumulative_years = year - 2024) %>% 
  relocate(cumulative_years, .before = scenario) 

# intervention treatment cost

cost_cum_treatment <- cost_summary %>% 
  filter(indicator %in% c("cost_total_treatment"),
         scenario %in% c("annual_absolute_dif"),
         test_reduction %in% c(25, 50, 75, 100),
         year >= 2025) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value)) %>% 
  group_by(scenario, indicator, pitc_reduction_year, test_reduction, discount) %>% 
  summarise(cumulative_mean = cumsum(mean))
cost_cum_treatment$year <- c(rep(seq(2025,2100,1), 12))
cost_cum_treatment <- cost_cum_treatment %>% 
  filter(year %in% c(2029, 2034, 2049, 2074)) %>% 
  mutate(cumulative_years = year - 2024) %>% 
  relocate(cumulative_years, .before = scenario) 

# intervention treatment inpatient

cost_cum_care <- cost_summary %>% 
  filter(indicator %in% c("cost_total_care"),
         scenario %in% c("annual_absolute_dif"),
         test_reduction %in% c(25, 50, 75, 100),
         year >= 2025) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value)) %>% 
  group_by(scenario, indicator, pitc_reduction_year, test_reduction, discount) %>% 
  summarise(cumulative_mean = cumsum(mean))
cost_cum_care$year <- c(rep(seq(2025,2100,1), 12))
cost_cum_care <- cost_cum_care %>% 
  filter(year %in% c(2029, 2034, 2049, 2074)) %>% 
  mutate(cumulative_years = year - 2024) %>% 
  relocate(cumulative_years, .before = scenario) 

cost_cum_intervention$percentage_change_baseline <- (cost_cum_intervention$cumulative_mean  / cost_cum_base$cumulative_mean) * 100
cost_cum_care$percentage_change_baseline <- (cost_cum_care$cumulative_mean / cost_cum_base$cumulative_mean) * 100
cost_cum_testing$percentage_change_baseline <- (cost_cum_testing$cumulative_mean / cost_cum_base$cumulative_mean) * 100
cost_cum_treatment$percentage_change_baseline <- (cost_cum_treatment$cumulative_mean / cost_cum_base$cumulative_mean) * 100

cumulative_change_from_baseline_total <- bind_rows(cost_cum_intervention,cost_cum_testing,cost_cum_treatment,cost_cum_care)

cumulative_change_from_baseline_total <- cumulative_change_from_baseline_total %>% 
  mutate(indicator = factor(indicator,
                            labels = c("Total", "Testing", "Treatment", "Inpatient"),
                            levels = c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care")),
         scenario = factor(scenario, 
                           labels = c("Total")))

#### cumulative change from baseline from cumulative costs ####

# total_baseline <- cumulative_costs %>% 
#   filter(indicator %in% c("cost_total"),
#          scenario %in% c("baseline"),
#          cumulative_years %in% c(4, 9, 24, 49),
#          test_reduction %in% c(25,50,75,100),
#          discount != "8.25%")
# 
# total_intervention  <- cumulative_costs %>% 
#   filter(indicator %in% c("cost_total"),
#          scenario %in% c("absolute_dif"),
#          cumulative_years %in% c(4, 9, 24, 49),
#          test_reduction %in% c(25,50,75,100),
#          discount != "8.25%")
#   
# total_care <- cumulative_costs %>% 
#   filter(indicator %in% c("cost_total_care"),
#          scenario %in% c("absolute_dif"),
#          cumulative_years %in% c(4, 9, 24, 49),
#          test_reduction %in% c(25,50,75,100),
#          discount != "8.25%")
# 
# total_testing <- cumulative_costs %>% 
#   filter(indicator %in% c("cost_total_testing"),
#          scenario %in% c("absolute_dif"),
#          cumulative_years %in% c(4, 9, 24, 49),
#          test_reduction %in% c(25,50,75,100),
#          discount != "8.25%")
# 
# total_treatment <- cumulative_costs %>% 
#   filter(indicator %in% c("cost_total_treatment"),
#          scenario %in% c("absolute_dif"),
#          cumulative_years %in% c(4, 9, 24, 49),
#          test_reduction %in% c(25,50,75,100),
#          discount != "8.25%")
# 
# total_intervention$percentage_change_baseline <- (total_intervention$mean  / total_baseline$mean) * 100
# total_care$percentage_change_baseline <- (total_care$mean / total_baseline$mean) * 100
# total_testing$percentage_change_baseline <- (total_testing$mean / total_baseline$mean) * 100
# total_treatment$percentage_change_baseline <- (total_treatment$mean / total_baseline$mean) * 100
# 
# cumulative_change_from_baseline_total <- bind_rows(total_intervention,total_care,total_testing,total_treatment)
# 
# cumulative_change_from_baseline_total <- cumulative_change_from_baseline_total %>% 
#   mutate(indicator = factor(indicator,
#                             labels = c("Total", "Testing", "Treatment", "Inpatient"),
#                             levels = c("cost_total", "cost_total_testing", "cost_total_treatment", "cost_total_care")),
#          scenario = factor(scenario, 
#                            labels = c("Total")))

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

percent_change_25

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

percent_change_100

percent_change <- ggarrange(percent_change_25, percent_change_50, percent_change_75, percent_change_100,
          ncol =1, common.legend = TRUE, legend = "right", align = "h")

percent_change <- annotate_figure(percent_change, top = text_grob("Percentage of baseline\ncumulative cost", 
                                                   color = "black", face = "bold", size = 11.5, hjust = 0.70))
#### proportion total ####

proportion_total <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total"),
         scenario %in% c("intervention"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction %in% c(0,25,50,75,100),
         discount != "8.25%")

proportion_care <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_care"),
         scenario %in% c("intervention"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction %in% c(0,25,50,75,100),
         discount != "8.25%")

proportion_testing <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_testing"),
         scenario %in% c("intervention"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction %in% c(0,25,50,75,100),
         discount != "8.25%")

proportion_treatment <- cumulative_costs %>% 
  filter(indicator %in% c("cost_total_treatment"),
         scenario %in% c("intervention"),
         cumulative_years %in% c(4, 9, 24, 49),
         test_reduction %in% c(0,25,50,75,100),
         discount != "8.25%")

proportion_total$proportion_total <- proportion_total$mean / proportion_total$mean
proportion_care$proportion_total <- proportion_care$mean / proportion_total$mean
proportion_testing$proportion_total <- proportion_testing$mean / proportion_total$mean
proportion_treatment$proportion_total <- proportion_treatment$mean / proportion_total$mean

proportion_totals <- bind_rows(proportion_total,
                               proportion_care, 
                               proportion_testing,
                               proportion_treatment) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value, pitc_reduction_year, median, scenario)) %>% 
  mutate(cost_source = case_when(indicator == "cost_total" ~ "total",
                                 indicator == "cost_total_care" ~ "care",
                                 indicator == "cost_total_testing" ~ "testing",
                                 indicator == "cost_total_treatment" ~ "treatment")) %>% 
  relocate(cost_source, .before = indicator) %>% 
  select(-indicator) %>% 
  rename(mean_cost = mean) %>% 
  mutate(cumulative_years = cumulative_years + 1) %>% 
  mutate(percentage_total = round((proportion_total * 100),2))

write_csv(proportion_totals, "results/proportion_total.csv")

proportion_totals %>% 
  filter(cost_source %in% c("testing", "treatment", "care"),
         discount == "undiscounted") %>% 
  mutate(test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(cumulative_years, mean_cost, fill = cost_source)) +
  geom_bar(aes(), position =  "fill", stat = "identity" 
           #width = 0.85, alpha = 0.75
  ) +
  scale_y_continuous("",
                     labels = (function(l) {paste0(l*100,"%")}), expand = c(0,0)) +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("Cumulative years from 2025", labels = c(5, 10, 25, 50)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = unit(c(0.5,0,0,0), "cm"),
        legend.spacing.y = unit(-0.15, "cm"),
        strip.text = element_text(size = 10, vjust = 1, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside") +
  facet_wrap(~test_reduction, 
             scale = "free_x", ncol = 1,
             labeller = as_labeller(c(
               `0` = "Status quo\ngeneral HTS",
               `25` = "25% general\nHTS reduction",
               `50` = "50% general\nHTS reduction",
               `75` = "75% general\nHTS reduction",
               `100` = "100% general\nHTS reduction")),
             strip.position = "left")
  
proportion_total_fig <- proportion_totals %>% 
  filter(cost_source %in% c("testing", "treatment", "care"),
         discount == "undiscounted") %>% 
  mutate(test_reduction = as.factor(test_reduction),
         cumulative_years = as.factor(cumulative_years)) %>% 
  ggplot(aes(test_reduction, mean_cost, fill = cost_source)) +
  geom_bar(aes(), position =  "fill", stat = "identity",
           width = .9
  ) +
  scale_y_continuous("",
                     labels = (function(l) {paste0(l*100,"%")}),expand = c(0,0)) +
  scale_fill_brewer("",
                    palette = "Dark2",
                    direction = -1, 
                    labels = c("Care", "Testing", "Treatment"),
                    aesthetics = c("fill")) +
  scale_x_discrete("General HTS reduction (%)", labels = c(0, 25, 50, 75, 100)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = unit(c(0.5,0,0,0), "cm"),
        legend.spacing.y = unit(-0.15, "cm"),
        strip.text = element_text(size = 10, vjust = 1, face = "bold"),
        strip.background = element_blank(),
        strip.placement = "outside") +
  facet_wrap(~cumulative_years, 
             scale = "free_x", ncol = 1,
             labeller = as_labeller(c(
               `5` = "5 years",
               `10` = "10 years",
               `25` = "25 years",
               `50` = "50 years")),
             strip.position = "left") + 
  ggtitle("Percentage of total\ncumulative costs\n(undiscounted)")
ggsave(plot = proportion_total_fig, filename = "figures/proportion_total.png", device = "png", height = 20, units = "cm")


#### annual percentage cost ####
annual_percentage_total <- cost_summary %>% 
  filter(indicator %in% c("cost_total"),
         scenario %in% c("intervention"),
         test_reduction %in% c(0, 25,50,75,100),
         discount != "8.25%", 
         year >= 2020)

annual_percentage_care <- cost_summary %>% 
  filter(indicator %in% c("cost_total_care"),
         scenario %in% c("intervention"),
         test_reduction %in% c(0, 25,50,75,100),
         discount != "8.25%", 
         year >= 2020)

annual_percentage_testing <- cost_summary %>% 
  filter(indicator %in% c("cost_total_testing"),
         scenario %in% c("intervention"),
         test_reduction %in% c(0, 25,50,75,100),
         discount != "8.25%", 
         year >= 2020)

annual_percentage_treatment <- cost_summary %>% 
  filter(indicator %in% c("cost_total_treatment"),
         scenario %in% c("intervention"),
         test_reduction %in% c(0, 25,50,75,100),
         discount != "8.25%", 
         year >= 2020)

annual_percentage_total$proportion_total <- annual_percentage_total$mean / annual_percentage_total$mean
annual_percentage_care$proportion_total <- annual_percentage_care$mean / annual_percentage_total$mean
annual_percentage_testing$proportion_total <- annual_percentage_testing$mean / annual_percentage_total$mean
annual_percentage_treatment$proportion_total <- annual_percentage_treatment$mean / annual_percentage_total$mean

annual_percentage <- bind_rows(annual_percentage_total,
                               annual_percentage_care, 
                               annual_percentage_testing,
                               annual_percentage_treatment) %>% 
  select(-c(upper_CI, lower_CI, future_variability, future_value, pitc_reduction_year, scenario)) %>% 
  mutate(cost_source = case_when(indicator == "cost_total" ~ "total",
                                 indicator == "cost_total_care" ~ "care",
                                 indicator == "cost_total_testing" ~ "testing",
                                 indicator == "cost_total_treatment" ~ "treatment")) %>% 
  relocate(cost_source, .before = indicator) %>% 
  select(-indicator) %>% 
  rename(mean_cost = mean) %>%
  mutate(percentage_total = round((proportion_total * 100),2))


annual_percentage_fig <- annual_percentage %>% 
  filter(cost_source %in% c("testing", "treatment", "care"),
         discount == "undiscounted",
         year <=2100) %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(year, mean_cost, fill = cost_source, colour = cost_source)) +
  geom_bar(aes(), position =  "fill", stat = "identity", 
           width = 1
  ) +
  scale_y_continuous("",
                     labels = (function(l) {paste0(l*100,"%")}), expand = c(0,0)) +
  scale_discrete_manual("",
                        breaks = c("total", "testing", "treatment", "care"), 
                        values = c("total" = "black", "testing" = "#d95f02", "treatment" = "#1b9e77", "care" = "#7570b3"), 
                        labels= c("Total", "Testing", "Treatment", "Inpatient"), 
                        aesthetics = c("fill", "colour")) +
  scale_x_continuous("", breaks = seq(2020, 2100, 40), expand = c(0,0)) + theme_classic() +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 11.5, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.text = element_text(size = 9.5, vjust = 1),
        strip.background = element_blank(),
        strip.placement = "outside") +
  facet_wrap(~test_reduction, 
             scale = "free", nrow = 1,
             labeller = as_labeller(c(
               `0` = "Status quo\ngeneral HTS",
               `25` = "25% general\nHTS reduction",
               `50` = "50% general\nHTS reduction",
               `75` = "75% general\nHTS reduction",
               `100` = "100% general\nHTS reduction")),
             strip.position = "top") + 
  ggtitle("Percentage of total annual costs\n(Undiscounted)")

ggsave(plot = annual_percentage_fig, filename = "figures/annual_percentage_total.png", device = "png", height = 20, units = "cm")


percentage_total <- ggarrange(annual_percentage_fig, proportion_total_fig, common.legend = TRUE, legend = "right", labels = "AUTO")
ggsave(plot = percentage_total, filename = "figures/percentage_total.png", device = "png", width = 20, height = 20, units = "cm")

