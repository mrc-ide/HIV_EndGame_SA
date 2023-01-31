source(here("scripts/cluster_function.R"))

art_change_values <- seq(1,4,1)

for (i in art_change_values){
  run_on_cluster(pitc_reduction_years = 2025, 
                 pitc_reduction_percentage = 0,
                 condom_usage_reduction = FALSE, 
                 fsw_condom_usage_decrease = 0,
                 st_condom_usage_decrease = 0, 
                 lt_condom_usage_decrease = 0,
                 condom_incr_start = 2025,
                 art_coverage_increase = FALSE,
                 art_interrupt_rate_decrease = i/100,
                 art_incr_start = 2025,
                 summary_name = paste0("decrease_art_retention_", i),
                 cumulative_years = 50,
                 art_coverage_decrease = TRUE,
                 art_interrupt_rate_increase = 0.01,
                 art_decr_start = 2035
  )
}


filepaths <- paste0("results/decrease_art_retention_", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
combined_summary <- bind_rows(temp, .id = "art_int_reduction")

summary_1 <- read_csv("results/decrease_art_retention_1.csv")

summary_1 %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, indicator == "art_interrupt_rate") %>% 
  ggplot(aes(year, mean, group = pitc_reduction_year, fill = pitc_reduction_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = pitc_reduction_year), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = pitc_reduction_year), show.legend = F) +
  xlab("") +
  scale_y_continuous("Proportion of PLWH on ART who are ART interrupters") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14))
  
summary_2 <- read_csv("results/decrease_art_retention_2.csv")

summary_2 %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, indicator == "art_interrupt_rate") %>% 
  ggplot(aes(year, mean, group = pitc_reduction_year, fill = pitc_reduction_year)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = pitc_reduction_year), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = pitc_reduction_year), show.legend = F) +
  xlab("") +
  scale_y_continuous("Proportion of PLWH on ART who are ART interrupters") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14))
