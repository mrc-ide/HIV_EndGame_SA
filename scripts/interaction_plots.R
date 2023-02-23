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

art_change_inc_elim <- bind_rows(art_improvement_inc_elim, art_reduction_inc_elim)
art_change_inc_elim <- art_change_inc_elim %>%
  mutate(art_int_improvement = if_else(!is.na(art_int_improvement), paste0("–", as.character(art_int_improvement), "%"), art_int_improvement)) %>% 
  mutate(art_int_reduction = if_else(!is.na(art_int_reduction), paste0(as.character(art_int_reduction), "%"), art_int_reduction)) %>% 
  mutate(art_change = art_int_improvement) %>% 
  mutate(art_change = if_else(is.na(art_int_improvement), art_int_reduction, art_change)) %>% 
  select(-c(art_int_improvement, art_int_reduction))

art_change_inc_elim$art_change[which(art_change_inc_elim$art_change == "–0%")] <- "0%"
art_change_inc_elim <- art_change_inc_elim %>% mutate(art_change = factor(art_change, levels = c("8%", "6%", "4%", "2%", "0%", "–2%", "–4%", "–6%", "–8%")))


# plotting 
art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                  pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = art_change, color = art_change)) + 
  geom_line(aes(color = art_change)) +
  xlab("PITC reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("Year HIV eliminaton attained", n.breaks = 10,limits = c(2040, 2100)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  scale_color_discrete("ART \ninterruption \nrate \nchange") +
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
  # filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_change, fill = round(1000*mean_incidence_2100,2))) + 
  geom_tile() + geom_text(aes(label = round(1000*mean_incidence_2100,2)), color = "black", size = 2.5) +
  facet_wrap(~pitc_reduction_year) + scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                                                          low = "blue",
                                                          mid = "#FFFFCC",
                                                          high = "#FF0000", 
                                                          midpoint = 1) +
  ylab("Change in relative rate of ART interruption") +
  xlab("PITC reduction (%)") + theme_classic()

art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = art_change, fill = mean_incidence_2100)) + 
  geom_tile() + geom_text(aes(label = round(1000*mean_incidence_2100,2)), color = "white", size = 2.5) +
  facet_wrap(~pitc_reduction_year) + scale_fill_continuous("Mean HIV \nincidence \nin 2100",type = "viridis") +
  ylab("Change in relative rate of ART interruption") +
  xlab("PITC reduction (%)") + theme_classic()

art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  # filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_change, fill = elimination_year)) + 
  geom_tile() + 
  facet_wrap(~pitc_reduction_year) + scale_fill_continuous("HIV \nelimination \nyear", limits = c(2040, 2100),type = "viridis") +
  ylab("Change in Relative Rate of ART Interruption") + 
  xlab("PITC reduction (%)")

#### condom reduction values ####

condom_reduction_values <- seq(0, 14, 2)

for (i in condom_reduction_values){
  run_on_cluster(pitc_reduction_years = seq(2025, 2050, 5), 
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

write_csv(reduce_condom_usage_with_test_reduction, "results/reduce_condom_usage_with_test_reduction")


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


n_comdom_reduction_values <- length(condom_reduction_values)
condom_reduction_dfs <- replicate(n_comdom_reduction_values, tibble())
for (i in 1:n_comdom_reduction_values) {
  condom_reduction_dfs[[i]] <- find_inc_and_elimination(filter(reduce_condom_usage_with_test_reduction, 
                                                               condom_usage_reduction == condom_reduction_values[i]))
}
names(condom_reduction_dfs) <- condom_reduction_values
condom_reduction_inc_elim <- bind_rows(condom_reduction_dfs, .id = "condom_usage_reduction")


 # plotting 
condom_reduction_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                    pitc_reduction_year = as.factor(pitc_reduction_year),
                                    condom_usage_reduction = factor(condom_usage_reduction, 
                                                                    levels = as.character(condom_reduction_values))) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, elimination_year, group = condom_usage_reduction, color = condom_usage_reduction)) + 
  geom_line(aes(color = condom_usage_reduction)) +
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



