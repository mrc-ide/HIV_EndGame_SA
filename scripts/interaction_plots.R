#### plotting incidence at 2100 and HIV elimination year ####

source("cluster_function_thembisav18.R")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa.exe -O2")


run_on_cluster(pitc_reduction_years = c(2025,2030, 2035, 2040, 2045, 2050), 
               pitc_reduction_percentage = c(0, 20, 40, 60, 80, 100),
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
  scale_fill_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + theme(legend.title = element_blank())


test_reduction_only_inc_elim <- find_inc_and_elimination(test_reduction_only)

### HIV elimination year 

test_reduction_only_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                        pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = pitc_reduction_year, color = pitc_reduction_year)) + 
  geom_line(aes(color = pitc_reduction_year)) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10) + expand_limits(y = 2100) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("PITC \nreduction \nyear")

# HIV incidence at 2100

test_reduction_only_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = pitc_reduction_year, fill = pitc_reduction_year)) + 
  geom_line(aes(color = pitc_reduction_year)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = pitc_reduction_year), alpha = 0.10, show.legend = F) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("PITC \nreduction \nyear")

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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No PITC reduction",
                                                           "10" = "10% PITC reduction",
                                                           "20" = "20% PITC reduction",
                                                           "30" = "30% PITC reduction",
                                                           "40" = "40% PITC reduction",
                                                           "50" = "50% PITC reduction",
                                                           "60" = "60% PITC reduction",
                                                           "70" = "70% PITC reduction",
                                                           "80" = "80% PITC reduction",
                                                           "90" = "90% PITC reduction",
                                                           "100" = "100% PITC reduction")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + theme(legend.title = element_blank())


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
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10, na.value = 2200,limits = c(2040, 2100)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("ART \ninterruption \nrate \nincrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "PITC reduced in 2025",
                                                                `2030` = "PITC reduced in 2030",
                                                                `2035` = "PITC reduced in 2035",
                                                                `2040` = "PITC reduced in 2040", 
                                                                `2045` = "PITC reduced in 2045", 
                                                                `2050` = "PITC reduced in 2050")))

# hiv incidence at 2100
art_reduction_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = art_int_reduction, fill = art_int_reduction)) + 
  geom_line(aes(color = art_int_reduction)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = art_int_reduction), alpha = 0.10, show.legend = F) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("ART \ninterruption \nrate \nincrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "PITC reduced in 2025",
                                                                `2030` = "PITC reduced in 2030",
                                                                `2035` = "PITC reduced in 2035",
                                                                `2040` = "PITC reduced in 2040", 
                                                                `2045` = "PITC reduced in 2045", 
                                                                `2050` = "PITC reduced in 2050")))

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
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(test_reduction),labeller = as_labeller(c("0" = "No PITC reduction",
                                                           "10" = "10% PITC reduction",
                                                           "20" = "20% PITC reduction",
                                                           "30" = "30% PITC reduction",
                                                           "40" = "40% PITC reduction",
                                                           "50" = "50% PITC reduction",
                                                           "60" = "60% PITC reduction",
                                                           "70" = "70% PITC reduction",
                                                           "80" = "80% PITC reduction",
                                                           "90" = "90% PITC reduction",
                                                           "100" = "100% PITC reduction")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + theme(legend.title = element_blank())


n_art_int_values <- length(art_change_values)
art_improvement_dfs <- replicate(n_art_int_values, tibble())
for (i in 1:n_art_int_values) {
  art_improvement_dfs[[i]] <- find_inc_and_elimination(filter(increase_art_retention_with_test_reduction, 
                                                            art_int_reduction == art_change_values[i]))
}
names(art_improvement_dfs) <- art_change_values
art_improvement_inc_elim <- bind_rows(art_improvement_dfs, .id = "art_int_improvement")

# joining art_improvement and art_reduction dfs 

art_reduction_inc_elim <- art_reduction_inc_elim %>% mutate(art_int_rate = (0.22*((1+(as.numeric(art_int_reduction)/100))**10)))
art_improvement_inc_elim <- art_improvement_inc_elim %>% mutate(art_int_rate = (0.22*((1-(as.numeric(art_int_improvement)/100))**10)))
art_change_inc_elim <- bind_rows(art_improvement_inc_elim, art_reduction_inc_elim)
art_change_inc_elim <- art_change_inc_elim %>% 
  select(-c(art_int_improvement, art_int_reduction))



# plotting 
art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                  pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = art_int_rate, color = art_int_rate)) + 
  geom_line(aes(color = art_int_rate)) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10,limits = c(2040, 2100)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_continuous("ART \ninterruption \nrate in 2035",type = "viridis") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "PITC reduced in 2025",
                                                                `2030` = "PITC reduced in 2030",
                                                                `2035` = "PITC reduced in 2035",
                                                                `2040` = "PITC reduced in 2040", 
                                                                `2045` = "PITC reduced in 2045", 
                                                                `2050` = "PITC reduced in 2050")))

# hiv incidence at 2100
art_change_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  #filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = art_change, fill = art_change)) + 
  geom_line(aes(color = art_change)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = art_change), alpha = 0.10, show.legend = F) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("ART \ninterruption \nchange") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "PITC reduced in 2025",
                                                                `2030` = "PITC reduced in 2030",
                                                                `2035` = "PITC reduced in 2035",
                                                                `2040` = "PITC reduced in 2040", 
                                                                `2045` = "PITC reduced in 2045", 
                                                                `2050` = "PITC reduced in 2050")))

#### heatmaps ####

art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_int_rate, z = log(1000*mean_incidence_2100))) + 
  geom_raster(aes(fill = log(1000*mean_incidence_2100)), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Log mean \nHIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                                                          low = "blue",
                                                          mid = "#FFFFCC",
                                                          high = "#FF0000", 
                                                          midpoint = 0) +
  ylab("Female ART interruption rate") + scale_y_continuous(trans = "logit") +
  xlab("PITC reduction (%)") + theme_classic()

art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_int_rate, z = round(1000*mean_incidence_2100,2))) + 
  geom_raster(aes(fill = round(1000*mean_incidence_2100,2)), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Mean \nHIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 1) +
  ylab("Female ART interruption rate") + scale_y_continuous(trans = "logit") +
  xlab("PITC reduction (%)") + theme_classic()

art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = art_int_rate, z = elimination_year)) + 
  geom_raster(aes(fill = elimination_year), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 2055) +
  ylab("Female ART interruption rate") + scale_y_continuous(trans = "logit") +
  xlab("PITC reduction (%)") + theme_classic()



#### condom reduction values ####

condom_reduction_values <- seq(0, 14, 2)

for (i in condom_reduction_values){
  run_on_cluster(pitc_reduction_years = 2025, 
                 pitc_reduction_percentage = seq(0,100,10),
                 condom_usage_reduction = TRUE, 
                 fsw_condom_usage_decrease = i/300,
                 st_condom_usage_decrease = i/100, 
                 lt_condom_usage_decrease = i/100,
                 condom_incr_start = 2025,
                 art_coverage_increase = FALSE,
                 art_interrupt_rate_decrease = i/100,
                 art_incr_start = 2025,
                 summary_name = paste0("reduce_condom_usage_with_test_reduction", i),
                 cumulative_years = 50,
                 art_coverage_decrease = FALSE,
                 art_interrupt_rate_increase = 2/100,
                 art_decr_start = 2025
  )
}

filepaths <- paste0("results/reduce_condom_usage_with_test_reduction", condom_reduction_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- condom_reduction_values
reduce_condom_usage_with_test_reduction <- bind_rows(temp, .id = "condom_usage_reduction")

write_csv(reduce_condom_usage_with_test_reduction, "results/reduce_condom_usage_with_test_reduction.csv")
reduce_condom_usage_with_test_reduction <- read_csv("results/reduce_condom_usage_with_test_reduction.csv")

reduce_condom_usage_with_test_reduction %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, indicator == "CondomUsage", year > 1990) %>% 
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
                                                           "10" = "10% PITC reduction",
                                                           "20" = "20% PITC reduction",
                                                           "30" = "30% PITC reduction",
                                                           "40" = "40% PITC reduction",
                                                           "50" = "50% PITC reduction",
                                                           "60" = "60% PITC reduction",
                                                           "70" = "70% PITC reduction",
                                                           "80" = "80% PITC reduction",
                                                           "90" = "90% PITC reduction",
                                                           "100" = "100% PITC reduction")), 
             ncol = 2) +
  scale_fill_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + 
  scale_color_discrete(labels = c("Baseline", "Reduced \nPITC \nonly")) + theme(legend.title = element_blank())

overall_condom_usage_range <- reduce_condom_usage_with_test_reduction %>% filter(indicator == "CondomUsage", 
                                                   test_reduction == 0, pitc_reduction_year == 2025,
                                                   year == 2035, scenario == "intervention") %>% select(mean)



n_comdom_reduction_values <- length(condom_reduction_values)
condom_reduction_dfs <- replicate(n_comdom_reduction_values, tibble())
for (i in 1:n_comdom_reduction_values) {
  condom_reduction_dfs[[i]] <- find_inc_and_elimination(filter(reduce_condom_usage_with_test_reduction, 
                                                               condom_usage_reduction == condom_reduction_values[i]))
}
names(condom_reduction_dfs) <- overall_condom_usage_range$mean
condom_reduction_inc_elim <- bind_rows(condom_reduction_dfs, .id = "overall_condom_usage")

write_csv(condom_reduction_inc_elim, "condom_reduction_inc_elim.csv")
 # plotting 

condom_usage_reduction_labels <- round(overall_condom_usage_range$mean,1)

condom_reduction_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                    pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = overall_condom_usage, color = overall_condom_usage)) + 
  geom_line(aes(color = overall_condom_usage)) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10, na.value = 2200,limits = c(2040, 2100)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("Condom \nusage \nprobability \ndecrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "PITC reduced in 2025",
                                                                `2030` = "PITC reduced in 2030",
                                                                `2035` = "PITC reduced in 2035",
                                                                `2040` = "PITC reduced in 2040", 
                                                                `2045` = "PITC reduced in 2045", 
                                                                `2050` = "PITC reduced in 2050")))

# hiv incidence at 2100
condom_reduction_inc_elim %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                     condom_usage_reduction = factor(condom_usage_reduction,
                                                                     levels = as.character(condom_reduction_values))) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = condom_usage_reduction, fill = condom_usage_reduction)) + 
  geom_line(aes(color = condom_usage_reduction)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = condom_usage_reduction), alpha = 0.10, show.legend = F) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("Condom \nusage \nprobability \ndecrease") +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "PITC reduced in 2025",
                                                                `2030` = "PITC reduced in 2030",
                                                                `2035` = "PITC reduced in 2035",
                                                                `2040` = "PITC reduced in 2040", 
                                                                `2045` = "PITC reduced in 2045", 
                                                                `2050` = "PITC reduced in 2050")))

unique(as.numeric(condom_reduction_inc_elim$overall_condom_usage))

# heatmaps
condom_reduction_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                     pitc_reduction_year = as.factor(pitc_reduction_year),
                                     overall_condom_usage = as.numeric(overall_condom_usage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage, z = overall_condom_usage)) + 
  geom_raster(aes(fill = elimination_year), interpolate = TRUE) + geom_text(aes(label = elimination_year), color = "white", size = 2.5) +
  scale_fill_continuous("HIV \nelimination \nyear",type = "viridis", limits = c(2040, 2100)) +
  ylab("Overall condom usage (%)") +
  xlab("PITC reduction (%)") + theme_classic()



condom_reduction_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                     pitc_reduction_year = as.factor(pitc_reduction_year),
                                     condom_usage_reduction = factor(condom_usage_reduction,
                                                                     labels = condom_usage_reduction_labels, 
                                                                     levels = as.character(condom_reduction_values))) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = condom_usage_reduction, fill = round(1000*mean_incidence_2100,2))) + 
  geom_raster(interpolate = TRUE) + geom_text(aes(label = round(1000*mean_incidence_2100,2)), color = "black", size = 2.5) +
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                                                          low = "blue",
                                                          mid = "#FFFFCC",
                                                          high = "#FF0000", 
                                                          midpoint = 1) +
  ylab("Reduction in condom usage probability (%)") +
  xlab("PITC reduction (%)") + theme_classic()


# trying contours

v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour()

unique(as.numeric(condom_reduction_inc_elim$condom_usage_reduction))
condom_reduction_inc_elim %>% mutate(overall_condom_usage = )



condom_heatmap_inc <- condom_reduction_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                                       pitc_reduction_year = as.factor(pitc_reduction_year),
                                                       overall_condom_usage = as.numeric(overall_condom_usage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage, z = log(1000*mean_incidence_2100)))

condom_heatmap_inc + geom_raster(aes(fill = log(1000*mean_incidence_2100)), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Log mean \nHIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log") +
  xlab("PITC reduction (%)") + theme_classic()

condom_heatmap_year <- condom_reduction_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                                       pitc_reduction_year = as.factor(pitc_reduction_year),
                                                       overall_condom_usage = as.numeric(overall_condom_usage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage, z = elimination_year))

condom_heatmap_year + geom_raster(aes(fill = elimination_year), interpolate = TRUE) + 
  geom_contour2(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 2055) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log") +
  xlab("PITC reduction (%)") + theme_classic()


