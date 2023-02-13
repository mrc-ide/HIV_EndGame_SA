source(here("scripts/cluster_function.R"))

art_change_values <- seq(0,4,1)

for (i in art_change_values){
  run_on_cluster(pitc_reduction_years = 2025, 
                 pitc_reduction_percentage = c(0, 20, 40, 60, 80, 100),
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
combined_summary <- bind_rows(temp, .id = "art_int_reduction")

combined_summary %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, indicator == "art_interrupt_rate", art_int_reduction != 0) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  xlab("") +
  scale_y_continuous("Proportion of PLWH on ART who are ART interrupters") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(art_int_reduction))
  
run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(0, 20, 40, 60, 80, 100),
               condom_usage_reduction = FALSE, 
               fsw_condom_usage_decrease = 0,
               st_condom_usage_decrease = 0, 
               lt_condom_usage_decrease = 0,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 2/100,
               art_incr_start = 2025,
               summary_name = paste0("decrease_art_retention_with_test_reduction"),
               cumulative_years = 50,
               art_coverage_decrease = TRUE,
               art_interrupt_rate_increase = 2/100,
               art_decr_start = 2025)

decrease_art_retention_with_test_reduction <- read_csv("results/decrease_art_retention_with_test_reduction.csv")
               
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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No PITC reduction", 
                                                           "20" = "20% PITC reduction", 
                                                           "40" = "40% PITC redcution",
                                                           "60" = "60% PITC redcution", 
                                                           "80" = "80% PITC redcution",
                                                           "100" = "100% PITC redcution")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \nretention")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nretention")) + theme(legend.title = element_blank())

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(0, 20, 40, 60, 80, 100),
               condom_usage_reduction = FALSE, 
               fsw_condom_usage_decrease = 0,
               st_condom_usage_decrease = 0, 
               lt_condom_usage_decrease = 0,
               condom_incr_start = 2025,
               art_coverage_increase = TRUE,
               art_interrupt_rate_decrease = 2/100,
               art_incr_start = 2025,
               summary_name = paste0("increase_art_retention_with_test_reduction"),
               cumulative_years = 50,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 2/100,
               art_decr_start = 2025)

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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No PITC reduction", 
                                                           "20" = "20% PITC reduction", 
                                                           "40" = "40% PITC redcution",
                                                           "60" = "60% PITC redcution", 
                                                           "80" = "80% PITC redcution",
                                                           "100" = "100% PITC redcution")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Improved \nretention")) + 
  scale_color_discrete(labels = c("Baseline", "Improved \nretention")) + theme(legend.title = element_blank())


run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(0, 20, 40, 60, 80, 100),
               condom_usage_reduction = TRUE, 
               fsw_condom_usage_decrease = (0.07/3),
               st_condom_usage_decrease = 0.07, 
               lt_condom_usage_decrease = 0.07,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 2/100,
               art_incr_start = 2025,
               summary_name = "reduced_condom_usage_with_test_reduction",
               cumulative_years = 50,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 2/100,
               art_decr_start = 2025
)

reduced_condom_usage_with_test_reduction <- read_csv("results/reduced_condom_usage_with_test_reduction.csv")

reduced_condom_usage_with_test_reduction %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, indicator == "HIVinc15to49", year > 1990) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") + 
  xlab("") +
  scale_y_continuous("HIV incidence per 1000 (15-49 years)", labels = (function(l) {round(l*1e3,1)})) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No PITC reduction", 
                                                           "20" = "20% PITC reduction", 
                                                           "40" = "40% PITC redcution",
                                                           "60" = "60% PITC redcution", 
                                                           "80" = "80% PITC redcution",
                                                           "100" = "100% PITC redcution")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \ncondom \nusage")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \ncondom \nusage")) + theme(legend.title = element_blank())

run_on_cluster(pitc_reduction_years = c(2025,2030, 2035, 2040, 2045, 2050), 
               pitc_reduction_percentage = seq(0, 100, 10),
               condom_usage_reduction = FALSE, 
               fsw_condom_usage_decrease = (0.07/3),
               st_condom_usage_decrease = 0.07, 
               lt_condom_usage_decrease = 0.07,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 2/100,
               art_incr_start = 2025,
               summary_name = "test_reduction_only",
               cumulative_years = 50,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 2/100,
               art_decr_start = 2025
)

test_reduction_only <- read_csv("results/test_reduction_only.csv")

test_reduction_only %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2030, indicator == "HIVinc15to49", year > 1990) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") + 
  xlab("") +
  scale_y_continuous("HIV incidence per 1000 (15-49 years)", labels = (function(l) {round(l*1e3,1)})) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No PITC reduction", 
                                                           "20" = "20% PITC reduction", 
                                                           "40" = "40% PITC redcution",
                                                           "60" = "60% PITC redcution", 
                                                           "80" = "80% PITC redcution",
                                                           "100" = "100% PITC redcution")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + theme(legend.title = element_blank())
