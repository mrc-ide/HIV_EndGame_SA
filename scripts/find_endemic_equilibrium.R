source("cluster_function_thembisav18.R")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa.exe -O2")

run_on_cluster(pitc_reduction_years = c(2025,2050), 
               pitc_reduction_percentage = c(0),
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
  filter(indicator == "HIVinc15to49", year == 2100, test_reduction == 100) %>% 
  ggplot(aes(pitc_reduction_year, mean, group = scenario, color = scenario)) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), alpha = 0.25, show.legend = F) +
  geom_point(aes(colour = scenario), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") + 
  xlab("PITC reduction year") +
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
