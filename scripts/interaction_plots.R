#### plotting incidence at 2100 and HIV elimination year ####
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)

source("R/cluster_function_orderly.R")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa.exe -O2")
system("./thembisa.exe")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = 100,
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
               cumulative_years = 75,
               summary_name = "quick_test_75_years" 
)

test_reduction_only <- read_csv("THEMBISAv18/results/test_reduction_only_summary.csv")

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

art_change_values <- seq(0, 14, 0.5)

for (i in art_change_values){
  run_on_cluster(pitc_reduction_years = seq(2030, 2050, 5), 
                 pitc_reduction_percentage = seq(0,100,5),
                 condom_usage_reduction = FALSE, 
                 fsw_condom_usage_decrease = 0,
                 st_condom_usage_decrease = 0, 
                 lt_condom_usage_decrease = 0,
                 condom_incr_start = 2025,
                 art_coverage_increase = FALSE,
                 art_interrupt_rate_decrease = 0/100,
                 art_incr_start = 2025,
                 summary_name = paste0("decrease_art_retention_with_test_reduction", i),
                 cumulative_years = 50,
                 art_coverage_decrease = TRUE,
                 art_interrupt_rate_increase = i/100,
                 art_decr_start = seq(2025)
  )
}

filepaths <- paste0("results/decrease_art_retention_", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
decrease_art_retention_summary <- bind_rows(temp, .id = "art_int_reduction")

write_csv(decrease_art_retention_summary, "results/decrease_art_retention_summary.csv")
decrease_art_retention_summary <- read_csv("results/decrease_art_retention_summary.csv")

decrease_art_retention_summary %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
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

art_change_values <- seq(0, 14, 0.5)

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

filepaths <- paste0("results/increase_art_retention_", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
increase_art_retention_summary <- bind_rows(temp, .id = "art_int_reduction")

write_csv(increase_art_retention_summary, "results/increase_art_retention_summary.csv")
increase_art_retention_summary <- read_csv("results/increase_art_retention_summary.csv")

#### combine art change summary ####

art_change_summary <- bind_rows(increase_art_retention_summary, decrease_art_retention_summary)
art_change_summary <- art_change_summary %>% mutate(art_int_rate = case_when(
  future_variability == "art_deterioration" ~ (0.1486*((1+(as.numeric(future_value)))**10)),
  future_variability == "art_improvement" ~ (0.1486*((1-(as.numeric(future_value)))**10))))
art_change_summary <- art_change_summary %>% mutate(art_ret_rate = 1 - art_int_rate)
unique(art_change_summary$art_ret_rate)
write_csv(art_change_summary, "results/art_change_summary.csv")


#### calculating ART coverage in ART retention rate changes ####
art_coverage_range <- art_change_summary %>% filter(indicator == "ARTcoverageAdult", 
                                                    test_reduction == 0, pitc_reduction_year == 2025,
                                                    year == 2035, scenario == "intervention") %>% select(mean)
art_coverage_labels <- round(art_coverage_range$mean)
unique_future_values_art <- unique(art_change_summary$future_value)
art_change_summary <- art_change_summary %>% mutate(art_coverage_2035 = case_when(future_value == unique_future_values_art[1] & future_variability == "art_improvement" ~ art_coverage_labels[1],
                                                                                  future_value == unique_future_values_art[2]  & future_variability == "art_improvement" ~ art_coverage_labels[2],
                                                                                  future_value == unique_future_values_art[3]  & future_variability == "art_improvement" ~ art_coverage_labels[3],
                                                                                  future_value == unique_future_values_art[4]  & future_variability == "art_improvement" ~ art_coverage_labels[4],
                                                                                  future_value == unique_future_values_art[5]  & future_variability == "art_improvement" ~ art_coverage_labels[5],
                                                                                  future_value == unique_future_values_art[6]  & future_variability == "art_improvement" ~ art_coverage_labels[6],
                                                                                  future_value == unique_future_values_art[7]  & future_variability == "art_improvement" ~ art_coverage_labels[7],
                                                                                  future_value == unique_future_values_art[8]  & future_variability == "art_improvement" ~ art_coverage_labels[8],
                                                                                  future_value == unique_future_values_art[9]  & future_variability == "art_improvement" ~ art_coverage_labels[9],
                                                                                  future_value == unique_future_values_art[10]  & future_variability == "art_improvement" ~ art_coverage_labels[10],
                                                                                  future_value == unique_future_values_art[11]  & future_variability == "art_improvement" ~ art_coverage_labels[11],
                                                                                  future_value == unique_future_values_art[12]  & future_variability == "art_improvement" ~ art_coverage_labels[12],
                                                                                  future_value == unique_future_values_art[13]  & future_variability == "art_improvement" ~ art_coverage_labels[13],
                                                                                  future_value == unique_future_values_art[14]  & future_variability == "art_improvement" ~ art_coverage_labels[14],
                                                                                  future_value == unique_future_values_art[15]  & future_variability == "art_improvement" ~ art_coverage_labels[15],
                                                                                  future_value == unique_future_values_art[16]  & future_variability == "art_improvement" ~ art_coverage_labels[16],
                                                                                  future_value == unique_future_values_art[17]  & future_variability == "art_improvement" ~ art_coverage_labels[17],
                                                                                  future_value == unique_future_values_art[18]  & future_variability == "art_improvement" ~ art_coverage_labels[18],
                                                                                  future_value == unique_future_values_art[19]  & future_variability == "art_improvement" ~ art_coverage_labels[19],
                                                                                  future_value == unique_future_values_art[20]  & future_variability == "art_improvement" ~ art_coverage_labels[20],
                                                                                  future_value == unique_future_values_art[21]  & future_variability == "art_improvement" ~ art_coverage_labels[21],
                                                                                  future_value == unique_future_values_art[22]  & future_variability == "art_improvement" ~ art_coverage_labels[22],
                                                                                  future_value == unique_future_values_art[23]  & future_variability == "art_improvement" ~ art_coverage_labels[23],
                                                                                  future_value == unique_future_values_art[24]  & future_variability == "art_improvement" ~ art_coverage_labels[24],
                                                                                  future_value == unique_future_values_art[25]  & future_variability == "art_improvement" ~ art_coverage_labels[25],
                                                                                  future_value == unique_future_values_art[26]  & future_variability == "art_improvement" ~ art_coverage_labels[26],
                                                                                  future_value == unique_future_values_art[27]  & future_variability == "art_improvement" ~ art_coverage_labels[27],
                                                                                  future_value == unique_future_values_art[28]  & future_variability == "art_improvement" ~ art_coverage_labels[28],
                                                                                  future_value == unique_future_values_art[29]  & future_variability == "art_improvement" ~ art_coverage_labels[29],
                                                                                  future_value == unique_future_values_art[1] & future_variability == "art_deterioration" ~ art_coverage_labels[30],
                                                                                  future_value == unique_future_values_art[2]  & future_variability == "art_deterioration" ~ art_coverage_labels[31],
                                                                                  future_value == unique_future_values_art[3]  & future_variability == "art_deterioration" ~ art_coverage_labels[32],
                                                                                  future_value == unique_future_values_art[4]  & future_variability == "art_deterioration" ~ art_coverage_labels[33],
                                                                                  future_value == unique_future_values_art[5]  & future_variability == "art_deterioration" ~ art_coverage_labels[34],
                                                                                  future_value == unique_future_values_art[6]  & future_variability == "art_deterioration" ~ art_coverage_labels[35],
                                                                                  future_value == unique_future_values_art[7]  & future_variability == "art_deterioration" ~ art_coverage_labels[36],
                                                                                  future_value == unique_future_values_art[8]  & future_variability == "art_deterioration" ~ art_coverage_labels[37],
                                                                                  future_value == unique_future_values_art[9]  & future_variability == "art_deterioration" ~ art_coverage_labels[38],
                                                                                  future_value == unique_future_values_art[10]  & future_variability == "art_deterioration" ~ art_coverage_labels[39],
                                                                                  future_value == unique_future_values_art[11]  & future_variability == "art_deterioration" ~ art_coverage_labels[40],
                                                                                  future_value == unique_future_values_art[12]  & future_variability == "art_deterioration" ~ art_coverage_labels[41],
                                                                                  future_value == unique_future_values_art[13]  & future_variability == "art_deterioration" ~ art_coverage_labels[42],
                                                                                  future_value == unique_future_values_art[14]  & future_variability == "art_deterioration" ~ art_coverage_labels[43],
                                                                                  future_value == unique_future_values_art[15]  & future_variability == "art_deterioration" ~ art_coverage_labels[44],
                                                                                  future_value == unique_future_values_art[16]  & future_variability == "art_deterioration" ~ art_coverage_labels[45],
                                                                                  future_value == unique_future_values_art[17]  & future_variability == "art_deterioration" ~ art_coverage_labels[46],
                                                                                  future_value == unique_future_values_art[18]  & future_variability == "art_deterioration" ~ art_coverage_labels[47],
                                                                                  future_value == unique_future_values_art[19]  & future_variability == "art_deterioration" ~ art_coverage_labels[48],
                                                                                  future_value == unique_future_values_art[20]  & future_variability == "art_deterioration" ~ art_coverage_labels[49],
                                                                                  future_value == unique_future_values_art[21]  & future_variability == "art_deterioration" ~ art_coverage_labels[50],
                                                                                  future_value == unique_future_values_art[22]  & future_variability == "art_deterioration" ~ art_coverage_labels[51],
                                                                                  future_value == unique_future_values_art[23]  & future_variability == "art_deterioration" ~ art_coverage_labels[52],
                                                                                  future_value == unique_future_values_art[24]  & future_variability == "art_deterioration" ~ art_coverage_labels[53],
                                                                                  future_value == unique_future_values_art[25]  & future_variability == "art_deterioration" ~ art_coverage_labels[54],
                                                                                  future_value == unique_future_values_art[26]  & future_variability == "art_deterioration" ~ art_coverage_labels[55],
                                                                                  future_value == unique_future_values_art[27]  & future_variability == "art_deterioration" ~ art_coverage_labels[56],
                                                                                  future_value == unique_future_values_art[28]  & future_variability == "art_deterioration" ~ art_coverage_labels[57],
                                                                                  future_value == unique_future_values_art[29]  & future_variability == "art_deterioration" ~ art_coverage_labels[58]))

art_change_inc_elim <- art_change_inc_elim %>% mutate(art_coverage_2035 = case_when(future_value == unique_future_values_art[1] & future_variability == "art_improvement" ~ art_coverage_labels[1],
                                                                                    future_value == unique_future_values_art[2]  & future_variability == "art_improvement" ~ art_coverage_labels[2],
                                                                                    future_value == unique_future_values_art[3]  & future_variability == "art_improvement" ~ art_coverage_labels[3],
                                                                                    future_value == unique_future_values_art[4]  & future_variability == "art_improvement" ~ art_coverage_labels[4],
                                                                                    future_value == unique_future_values_art[5]  & future_variability == "art_improvement" ~ art_coverage_labels[5],
                                                                                    future_value == unique_future_values_art[6]  & future_variability == "art_improvement" ~ art_coverage_labels[6],
                                                                                    future_value == unique_future_values_art[7]  & future_variability == "art_improvement" ~ art_coverage_labels[7],
                                                                                    future_value == unique_future_values_art[8]  & future_variability == "art_improvement" ~ art_coverage_labels[8],
                                                                                    future_value == unique_future_values_art[9]  & future_variability == "art_improvement" ~ art_coverage_labels[9],
                                                                                    future_value == unique_future_values_art[10]  & future_variability == "art_improvement" ~ art_coverage_labels[10],
                                                                                    future_value == unique_future_values_art[11]  & future_variability == "art_improvement" ~ art_coverage_labels[11],
                                                                                    future_value == unique_future_values_art[12]  & future_variability == "art_improvement" ~ art_coverage_labels[12],
                                                                                    future_value == unique_future_values_art[13]  & future_variability == "art_improvement" ~ art_coverage_labels[13],
                                                                                    future_value == unique_future_values_art[14]  & future_variability == "art_improvement" ~ art_coverage_labels[14],
                                                                                    future_value == unique_future_values_art[15]  & future_variability == "art_improvement" ~ art_coverage_labels[15],
                                                                                    future_value == unique_future_values_art[16]  & future_variability == "art_improvement" ~ art_coverage_labels[16],
                                                                                    future_value == unique_future_values_art[17]  & future_variability == "art_improvement" ~ art_coverage_labels[17],
                                                                                    future_value == unique_future_values_art[18]  & future_variability == "art_improvement" ~ art_coverage_labels[18],
                                                                                    future_value == unique_future_values_art[19]  & future_variability == "art_improvement" ~ art_coverage_labels[19],
                                                                                    future_value == unique_future_values_art[20]  & future_variability == "art_improvement" ~ art_coverage_labels[20],
                                                                                    future_value == unique_future_values_art[21]  & future_variability == "art_improvement" ~ art_coverage_labels[21],
                                                                                    future_value == unique_future_values_art[22]  & future_variability == "art_improvement" ~ art_coverage_labels[22],
                                                                                    future_value == unique_future_values_art[23]  & future_variability == "art_improvement" ~ art_coverage_labels[23],
                                                                                    future_value == unique_future_values_art[24]  & future_variability == "art_improvement" ~ art_coverage_labels[24],
                                                                                    future_value == unique_future_values_art[25]  & future_variability == "art_improvement" ~ art_coverage_labels[25],
                                                                                    future_value == unique_future_values_art[26]  & future_variability == "art_improvement" ~ art_coverage_labels[26],
                                                                                    future_value == unique_future_values_art[27]  & future_variability == "art_improvement" ~ art_coverage_labels[27],
                                                                                    future_value == unique_future_values_art[28]  & future_variability == "art_improvement" ~ art_coverage_labels[28],
                                                                                    future_value == unique_future_values_art[29]  & future_variability == "art_improvement" ~ art_coverage_labels[29],
                                                                                    future_value == unique_future_values_art[1] & future_variability == "art_deterioration" ~ art_coverage_labels[30],
                                                                                    future_value == unique_future_values_art[2]  & future_variability == "art_deterioration" ~ art_coverage_labels[31],
                                                                                    future_value == unique_future_values_art[3]  & future_variability == "art_deterioration" ~ art_coverage_labels[32],
                                                                                    future_value == unique_future_values_art[4]  & future_variability == "art_deterioration" ~ art_coverage_labels[33],
                                                                                    future_value == unique_future_values_art[5]  & future_variability == "art_deterioration" ~ art_coverage_labels[34],
                                                                                    future_value == unique_future_values_art[6]  & future_variability == "art_deterioration" ~ art_coverage_labels[35],
                                                                                    future_value == unique_future_values_art[7]  & future_variability == "art_deterioration" ~ art_coverage_labels[36],
                                                                                    future_value == unique_future_values_art[8]  & future_variability == "art_deterioration" ~ art_coverage_labels[37],
                                                                                    future_value == unique_future_values_art[9]  & future_variability == "art_deterioration" ~ art_coverage_labels[38],
                                                                                    future_value == unique_future_values_art[10]  & future_variability == "art_deterioration" ~ art_coverage_labels[39],
                                                                                    future_value == unique_future_values_art[11]  & future_variability == "art_deterioration" ~ art_coverage_labels[40],
                                                                                    future_value == unique_future_values_art[12]  & future_variability == "art_deterioration" ~ art_coverage_labels[41],
                                                                                    future_value == unique_future_values_art[13]  & future_variability == "art_deterioration" ~ art_coverage_labels[42],
                                                                                    future_value == unique_future_values_art[14]  & future_variability == "art_deterioration" ~ art_coverage_labels[43],
                                                                                    future_value == unique_future_values_art[15]  & future_variability == "art_deterioration" ~ art_coverage_labels[44],
                                                                                    future_value == unique_future_values_art[16]  & future_variability == "art_deterioration" ~ art_coverage_labels[45],
                                                                                    future_value == unique_future_values_art[17]  & future_variability == "art_deterioration" ~ art_coverage_labels[46],
                                                                                    future_value == unique_future_values_art[18]  & future_variability == "art_deterioration" ~ art_coverage_labels[47],
                                                                                    future_value == unique_future_values_art[19]  & future_variability == "art_deterioration" ~ art_coverage_labels[48],
                                                                                    future_value == unique_future_values_art[20]  & future_variability == "art_deterioration" ~ art_coverage_labels[49],
                                                                                    future_value == unique_future_values_art[21]  & future_variability == "art_deterioration" ~ art_coverage_labels[50],
                                                                                    future_value == unique_future_values_art[22]  & future_variability == "art_deterioration" ~ art_coverage_labels[51],
                                                                                    future_value == unique_future_values_art[23]  & future_variability == "art_deterioration" ~ art_coverage_labels[52],
                                                                                    future_value == unique_future_values_art[24]  & future_variability == "art_deterioration" ~ art_coverage_labels[53],
                                                                                    future_value == unique_future_values_art[25]  & future_variability == "art_deterioration" ~ art_coverage_labels[54],
                                                                                    future_value == unique_future_values_art[26]  & future_variability == "art_deterioration" ~ art_coverage_labels[55],
                                                                                    future_value == unique_future_values_art[27]  & future_variability == "art_deterioration" ~ art_coverage_labels[56],
                                                                                    future_value == unique_future_values_art[28]  & future_variability == "art_deterioration" ~ art_coverage_labels[57],
                                                                                    future_value == unique_future_values_art[29]  & future_variability == "art_deterioration" ~ art_coverage_labels[58]))


cumulative_art_change <- cumulative_art_change %>% mutate(art_coverage_2035 = case_when(future_value == unique_future_values_art[1] & future_variability == "art_improvement" ~ art_coverage_labels[1],
                                                                                        future_value == unique_future_values_art[2]  & future_variability == "art_improvement" ~ art_coverage_labels[2],
                                                                                        future_value == unique_future_values_art[3]  & future_variability == "art_improvement" ~ art_coverage_labels[3],
                                                                                        future_value == unique_future_values_art[4]  & future_variability == "art_improvement" ~ art_coverage_labels[4],
                                                                                        future_value == unique_future_values_art[5]  & future_variability == "art_improvement" ~ art_coverage_labels[5],
                                                                                        future_value == unique_future_values_art[6]  & future_variability == "art_improvement" ~ art_coverage_labels[6],
                                                                                        future_value == unique_future_values_art[7]  & future_variability == "art_improvement" ~ art_coverage_labels[7],
                                                                                        future_value == unique_future_values_art[8]  & future_variability == "art_improvement" ~ art_coverage_labels[8],
                                                                                        future_value == unique_future_values_art[9]  & future_variability == "art_improvement" ~ art_coverage_labels[9],
                                                                                        future_value == unique_future_values_art[10]  & future_variability == "art_improvement" ~ art_coverage_labels[10],
                                                                                        future_value == unique_future_values_art[11]  & future_variability == "art_improvement" ~ art_coverage_labels[11],
                                                                                        future_value == unique_future_values_art[12]  & future_variability == "art_improvement" ~ art_coverage_labels[12],
                                                                                        future_value == unique_future_values_art[13]  & future_variability == "art_improvement" ~ art_coverage_labels[13],
                                                                                        future_value == unique_future_values_art[14]  & future_variability == "art_improvement" ~ art_coverage_labels[14],
                                                                                        future_value == unique_future_values_art[15]  & future_variability == "art_improvement" ~ art_coverage_labels[15],
                                                                                        future_value == unique_future_values_art[16]  & future_variability == "art_improvement" ~ art_coverage_labels[16],
                                                                                        future_value == unique_future_values_art[17]  & future_variability == "art_improvement" ~ art_coverage_labels[17],
                                                                                        future_value == unique_future_values_art[18]  & future_variability == "art_improvement" ~ art_coverage_labels[18],
                                                                                        future_value == unique_future_values_art[19]  & future_variability == "art_improvement" ~ art_coverage_labels[19],
                                                                                        future_value == unique_future_values_art[20]  & future_variability == "art_improvement" ~ art_coverage_labels[20],
                                                                                        future_value == unique_future_values_art[21]  & future_variability == "art_improvement" ~ art_coverage_labels[21],
                                                                                        future_value == unique_future_values_art[22]  & future_variability == "art_improvement" ~ art_coverage_labels[22],
                                                                                        future_value == unique_future_values_art[23]  & future_variability == "art_improvement" ~ art_coverage_labels[23],
                                                                                        future_value == unique_future_values_art[24]  & future_variability == "art_improvement" ~ art_coverage_labels[24],
                                                                                        future_value == unique_future_values_art[25]  & future_variability == "art_improvement" ~ art_coverage_labels[25],
                                                                                        future_value == unique_future_values_art[26]  & future_variability == "art_improvement" ~ art_coverage_labels[26],
                                                                                        future_value == unique_future_values_art[27]  & future_variability == "art_improvement" ~ art_coverage_labels[27],
                                                                                        future_value == unique_future_values_art[28]  & future_variability == "art_improvement" ~ art_coverage_labels[28],
                                                                                        future_value == unique_future_values_art[29]  & future_variability == "art_improvement" ~ art_coverage_labels[29],
                                                                                        future_value == unique_future_values_art[1] & future_variability == "art_deterioration" ~ art_coverage_labels[30],
                                                                                        future_value == unique_future_values_art[2]  & future_variability == "art_deterioration" ~ art_coverage_labels[31],
                                                                                        future_value == unique_future_values_art[3]  & future_variability == "art_deterioration" ~ art_coverage_labels[32],
                                                                                        future_value == unique_future_values_art[4]  & future_variability == "art_deterioration" ~ art_coverage_labels[33],
                                                                                        future_value == unique_future_values_art[5]  & future_variability == "art_deterioration" ~ art_coverage_labels[34],
                                                                                        future_value == unique_future_values_art[6]  & future_variability == "art_deterioration" ~ art_coverage_labels[35],
                                                                                        future_value == unique_future_values_art[7]  & future_variability == "art_deterioration" ~ art_coverage_labels[36],
                                                                                        future_value == unique_future_values_art[8]  & future_variability == "art_deterioration" ~ art_coverage_labels[37],
                                                                                        future_value == unique_future_values_art[9]  & future_variability == "art_deterioration" ~ art_coverage_labels[38],
                                                                                        future_value == unique_future_values_art[10]  & future_variability == "art_deterioration" ~ art_coverage_labels[39],
                                                                                        future_value == unique_future_values_art[11]  & future_variability == "art_deterioration" ~ art_coverage_labels[40],
                                                                                        future_value == unique_future_values_art[12]  & future_variability == "art_deterioration" ~ art_coverage_labels[41],
                                                                                        future_value == unique_future_values_art[13]  & future_variability == "art_deterioration" ~ art_coverage_labels[42],
                                                                                        future_value == unique_future_values_art[14]  & future_variability == "art_deterioration" ~ art_coverage_labels[43],
                                                                                        future_value == unique_future_values_art[15]  & future_variability == "art_deterioration" ~ art_coverage_labels[44],
                                                                                        future_value == unique_future_values_art[16]  & future_variability == "art_deterioration" ~ art_coverage_labels[45],
                                                                                        future_value == unique_future_values_art[17]  & future_variability == "art_deterioration" ~ art_coverage_labels[46],
                                                                                        future_value == unique_future_values_art[18]  & future_variability == "art_deterioration" ~ art_coverage_labels[47],
                                                                                        future_value == unique_future_values_art[19]  & future_variability == "art_deterioration" ~ art_coverage_labels[48],
                                                                                        future_value == unique_future_values_art[20]  & future_variability == "art_deterioration" ~ art_coverage_labels[49],
                                                                                        future_value == unique_future_values_art[21]  & future_variability == "art_deterioration" ~ art_coverage_labels[50],
                                                                                        future_value == unique_future_values_art[22]  & future_variability == "art_deterioration" ~ art_coverage_labels[51],
                                                                                        future_value == unique_future_values_art[23]  & future_variability == "art_deterioration" ~ art_coverage_labels[52],
                                                                                        future_value == unique_future_values_art[24]  & future_variability == "art_deterioration" ~ art_coverage_labels[53],
                                                                                        future_value == unique_future_values_art[25]  & future_variability == "art_deterioration" ~ art_coverage_labels[54],
                                                                                        future_value == unique_future_values_art[26]  & future_variability == "art_deterioration" ~ art_coverage_labels[55],
                                                                                        future_value == unique_future_values_art[27]  & future_variability == "art_deterioration" ~ art_coverage_labels[56],
                                                                                        future_value == unique_future_values_art[28]  & future_variability == "art_deterioration" ~ art_coverage_labels[57],
                                                                                        future_value == unique_future_values_art[29]  & future_variability == "art_deterioration" ~ art_coverage_labels[58]))



increase_art_retention_summary %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
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

#### changed art retention rate calculation ####
art_change_inc_elim <- art_change_inc_elim %>% mutate(art_int_rate = case_when(
  future_variability == "art_deterioration" ~ (0.1486*((1+(as.numeric(future_value)))**10)),
  future_variability == "art_improvement" ~ (0.1486*((1-(as.numeric(future_value)))**10))))

art_change_inc_elim <- art_change_inc_elim %>% mutate(art_ret_rate = 1 - art_int_rate)
write_csv(art_change_inc_elim, "results/art_change_inc_elim.csv")

art_change_inc_elim <- read_csv("results/art_change_inc_elim.csv")

#### add art_coverage_2035 as column ####

#### calculating ART coverage in ART retention rate changes ####
art_coverage_range <- art_change_summary %>% filter(indicator == "ARTcoverageAdult", 
                                                    test_reduction == 0, pitc_reduction_year == 2025,
                                                    year == 2035, scenario == "intervention") %>% select(mean)
art_coverage_labels <- round(art_coverage_range$mean)
unique_future_values_art <- unique(art_change_summary$future_value)
art_change_summary <- art_change_summary %>% mutate(art_coverage_2035 = case_when(future_value == unique_future_values_art[1] & future_variability == "art_improvement" ~ art_coverage_labels[1],
                                                                                  future_value == unique_future_values_art[2]  & future_variability == "art_improvement" ~ art_coverage_labels[2],
                                                                                  future_value == unique_future_values_art[3]  & future_variability == "art_improvement" ~ art_coverage_labels[3],
                                                                                  future_value == unique_future_values_art[4]  & future_variability == "art_improvement" ~ art_coverage_labels[4],
                                                                                  future_value == unique_future_values_art[5]  & future_variability == "art_improvement" ~ art_coverage_labels[5],
                                                                                  future_value == unique_future_values_art[6]  & future_variability == "art_improvement" ~ art_coverage_labels[6],
                                                                                  future_value == unique_future_values_art[7]  & future_variability == "art_improvement" ~ art_coverage_labels[7],
                                                                                  future_value == unique_future_values_art[8]  & future_variability == "art_improvement" ~ art_coverage_labels[8],
                                                                                  future_value == unique_future_values_art[9]  & future_variability == "art_improvement" ~ art_coverage_labels[9],
                                                                                  future_value == unique_future_values_art[10]  & future_variability == "art_improvement" ~ art_coverage_labels[10],
                                                                                  future_value == unique_future_values_art[11]  & future_variability == "art_improvement" ~ art_coverage_labels[11],
                                                                                  future_value == unique_future_values_art[12]  & future_variability == "art_improvement" ~ art_coverage_labels[12],
                                                                                  future_value == unique_future_values_art[13]  & future_variability == "art_improvement" ~ art_coverage_labels[13],
                                                                                  future_value == unique_future_values_art[14]  & future_variability == "art_improvement" ~ art_coverage_labels[14],
                                                                                  future_value == unique_future_values_art[15]  & future_variability == "art_improvement" ~ art_coverage_labels[15],
                                                                                  future_value == unique_future_values_art[16]  & future_variability == "art_improvement" ~ art_coverage_labels[16],
                                                                                  future_value == unique_future_values_art[17]  & future_variability == "art_improvement" ~ art_coverage_labels[17],
                                                                                  future_value == unique_future_values_art[18]  & future_variability == "art_improvement" ~ art_coverage_labels[18],
                                                                                  future_value == unique_future_values_art[19]  & future_variability == "art_improvement" ~ art_coverage_labels[19],
                                                                                  future_value == unique_future_values_art[20]  & future_variability == "art_improvement" ~ art_coverage_labels[20],
                                                                                  future_value == unique_future_values_art[21]  & future_variability == "art_improvement" ~ art_coverage_labels[21],
                                                                                  future_value == unique_future_values_art[22]  & future_variability == "art_improvement" ~ art_coverage_labels[22],
                                                                                  future_value == unique_future_values_art[23]  & future_variability == "art_improvement" ~ art_coverage_labels[23],
                                                                                  future_value == unique_future_values_art[24]  & future_variability == "art_improvement" ~ art_coverage_labels[24],
                                                                                  future_value == unique_future_values_art[25]  & future_variability == "art_improvement" ~ art_coverage_labels[25],
                                                                                  future_value == unique_future_values_art[26]  & future_variability == "art_improvement" ~ art_coverage_labels[26],
                                                                                  future_value == unique_future_values_art[27]  & future_variability == "art_improvement" ~ art_coverage_labels[27],
                                                                                  future_value == unique_future_values_art[28]  & future_variability == "art_improvement" ~ art_coverage_labels[28],
                                                                                  future_value == unique_future_values_art[29]  & future_variability == "art_improvement" ~ art_coverage_labels[29],
                                                                                  future_value == unique_future_values_art[1] & future_variability == "art_deterioration" ~ art_coverage_labels[30],
                                                                                  future_value == unique_future_values_art[2]  & future_variability == "art_deterioration" ~ art_coverage_labels[31],
                                                                                  future_value == unique_future_values_art[3]  & future_variability == "art_deterioration" ~ art_coverage_labels[32],
                                                                                  future_value == unique_future_values_art[4]  & future_variability == "art_deterioration" ~ art_coverage_labels[33],
                                                                                  future_value == unique_future_values_art[5]  & future_variability == "art_deterioration" ~ art_coverage_labels[34],
                                                                                  future_value == unique_future_values_art[6]  & future_variability == "art_deterioration" ~ art_coverage_labels[35],
                                                                                  future_value == unique_future_values_art[7]  & future_variability == "art_deterioration" ~ art_coverage_labels[36],
                                                                                  future_value == unique_future_values_art[8]  & future_variability == "art_deterioration" ~ art_coverage_labels[37],
                                                                                  future_value == unique_future_values_art[9]  & future_variability == "art_deterioration" ~ art_coverage_labels[38],
                                                                                  future_value == unique_future_values_art[10]  & future_variability == "art_deterioration" ~ art_coverage_labels[39],
                                                                                  future_value == unique_future_values_art[11]  & future_variability == "art_deterioration" ~ art_coverage_labels[40],
                                                                                  future_value == unique_future_values_art[12]  & future_variability == "art_deterioration" ~ art_coverage_labels[41],
                                                                                  future_value == unique_future_values_art[13]  & future_variability == "art_deterioration" ~ art_coverage_labels[42],
                                                                                  future_value == unique_future_values_art[14]  & future_variability == "art_deterioration" ~ art_coverage_labels[43],
                                                                                  future_value == unique_future_values_art[15]  & future_variability == "art_deterioration" ~ art_coverage_labels[44],
                                                                                  future_value == unique_future_values_art[16]  & future_variability == "art_deterioration" ~ art_coverage_labels[45],
                                                                                  future_value == unique_future_values_art[17]  & future_variability == "art_deterioration" ~ art_coverage_labels[46],
                                                                                  future_value == unique_future_values_art[18]  & future_variability == "art_deterioration" ~ art_coverage_labels[47],
                                                                                  future_value == unique_future_values_art[19]  & future_variability == "art_deterioration" ~ art_coverage_labels[48],
                                                                                  future_value == unique_future_values_art[20]  & future_variability == "art_deterioration" ~ art_coverage_labels[49],
                                                                                  future_value == unique_future_values_art[21]  & future_variability == "art_deterioration" ~ art_coverage_labels[50],
                                                                                  future_value == unique_future_values_art[22]  & future_variability == "art_deterioration" ~ art_coverage_labels[51],
                                                                                  future_value == unique_future_values_art[23]  & future_variability == "art_deterioration" ~ art_coverage_labels[52],
                                                                                  future_value == unique_future_values_art[24]  & future_variability == "art_deterioration" ~ art_coverage_labels[53],
                                                                                  future_value == unique_future_values_art[25]  & future_variability == "art_deterioration" ~ art_coverage_labels[54],
                                                                                  future_value == unique_future_values_art[26]  & future_variability == "art_deterioration" ~ art_coverage_labels[55],
                                                                                  future_value == unique_future_values_art[27]  & future_variability == "art_deterioration" ~ art_coverage_labels[56],
                                                                                  future_value == unique_future_values_art[28]  & future_variability == "art_deterioration" ~ art_coverage_labels[57],
                                                                                  future_value == unique_future_values_art[29]  & future_variability == "art_deterioration" ~ art_coverage_labels[58]))

art_change_inc_elim <- art_change_inc_elim %>% mutate(art_coverage_2035 = case_when(future_value == unique_future_values_art[1] & future_variability == "art_improvement" ~ art_coverage_labels[1],
                                                                                    future_value == unique_future_values_art[2]  & future_variability == "art_improvement" ~ art_coverage_labels[2],
                                                                                    future_value == unique_future_values_art[3]  & future_variability == "art_improvement" ~ art_coverage_labels[3],
                                                                                    future_value == unique_future_values_art[4]  & future_variability == "art_improvement" ~ art_coverage_labels[4],
                                                                                    future_value == unique_future_values_art[5]  & future_variability == "art_improvement" ~ art_coverage_labels[5],
                                                                                    future_value == unique_future_values_art[6]  & future_variability == "art_improvement" ~ art_coverage_labels[6],
                                                                                    future_value == unique_future_values_art[7]  & future_variability == "art_improvement" ~ art_coverage_labels[7],
                                                                                    future_value == unique_future_values_art[8]  & future_variability == "art_improvement" ~ art_coverage_labels[8],
                                                                                    future_value == unique_future_values_art[9]  & future_variability == "art_improvement" ~ art_coverage_labels[9],
                                                                                    future_value == unique_future_values_art[10]  & future_variability == "art_improvement" ~ art_coverage_labels[10],
                                                                                    future_value == unique_future_values_art[11]  & future_variability == "art_improvement" ~ art_coverage_labels[11],
                                                                                    future_value == unique_future_values_art[12]  & future_variability == "art_improvement" ~ art_coverage_labels[12],
                                                                                    future_value == unique_future_values_art[13]  & future_variability == "art_improvement" ~ art_coverage_labels[13],
                                                                                    future_value == unique_future_values_art[14]  & future_variability == "art_improvement" ~ art_coverage_labels[14],
                                                                                    future_value == unique_future_values_art[15]  & future_variability == "art_improvement" ~ art_coverage_labels[15],
                                                                                    future_value == unique_future_values_art[16]  & future_variability == "art_improvement" ~ art_coverage_labels[16],
                                                                                    future_value == unique_future_values_art[17]  & future_variability == "art_improvement" ~ art_coverage_labels[17],
                                                                                    future_value == unique_future_values_art[18]  & future_variability == "art_improvement" ~ art_coverage_labels[18],
                                                                                    future_value == unique_future_values_art[19]  & future_variability == "art_improvement" ~ art_coverage_labels[19],
                                                                                    future_value == unique_future_values_art[20]  & future_variability == "art_improvement" ~ art_coverage_labels[20],
                                                                                    future_value == unique_future_values_art[21]  & future_variability == "art_improvement" ~ art_coverage_labels[21],
                                                                                    future_value == unique_future_values_art[22]  & future_variability == "art_improvement" ~ art_coverage_labels[22],
                                                                                    future_value == unique_future_values_art[23]  & future_variability == "art_improvement" ~ art_coverage_labels[23],
                                                                                    future_value == unique_future_values_art[24]  & future_variability == "art_improvement" ~ art_coverage_labels[24],
                                                                                    future_value == unique_future_values_art[25]  & future_variability == "art_improvement" ~ art_coverage_labels[25],
                                                                                    future_value == unique_future_values_art[26]  & future_variability == "art_improvement" ~ art_coverage_labels[26],
                                                                                    future_value == unique_future_values_art[27]  & future_variability == "art_improvement" ~ art_coverage_labels[27],
                                                                                    future_value == unique_future_values_art[28]  & future_variability == "art_improvement" ~ art_coverage_labels[28],
                                                                                    future_value == unique_future_values_art[29]  & future_variability == "art_improvement" ~ art_coverage_labels[29],
                                                                                    future_value == unique_future_values_art[1] & future_variability == "art_deterioration" ~ art_coverage_labels[30],
                                                                                    future_value == unique_future_values_art[2]  & future_variability == "art_deterioration" ~ art_coverage_labels[31],
                                                                                    future_value == unique_future_values_art[3]  & future_variability == "art_deterioration" ~ art_coverage_labels[32],
                                                                                    future_value == unique_future_values_art[4]  & future_variability == "art_deterioration" ~ art_coverage_labels[33],
                                                                                    future_value == unique_future_values_art[5]  & future_variability == "art_deterioration" ~ art_coverage_labels[34],
                                                                                    future_value == unique_future_values_art[6]  & future_variability == "art_deterioration" ~ art_coverage_labels[35],
                                                                                    future_value == unique_future_values_art[7]  & future_variability == "art_deterioration" ~ art_coverage_labels[36],
                                                                                    future_value == unique_future_values_art[8]  & future_variability == "art_deterioration" ~ art_coverage_labels[37],
                                                                                    future_value == unique_future_values_art[9]  & future_variability == "art_deterioration" ~ art_coverage_labels[38],
                                                                                    future_value == unique_future_values_art[10]  & future_variability == "art_deterioration" ~ art_coverage_labels[39],
                                                                                    future_value == unique_future_values_art[11]  & future_variability == "art_deterioration" ~ art_coverage_labels[40],
                                                                                    future_value == unique_future_values_art[12]  & future_variability == "art_deterioration" ~ art_coverage_labels[41],
                                                                                    future_value == unique_future_values_art[13]  & future_variability == "art_deterioration" ~ art_coverage_labels[42],
                                                                                    future_value == unique_future_values_art[14]  & future_variability == "art_deterioration" ~ art_coverage_labels[43],
                                                                                    future_value == unique_future_values_art[15]  & future_variability == "art_deterioration" ~ art_coverage_labels[44],
                                                                                    future_value == unique_future_values_art[16]  & future_variability == "art_deterioration" ~ art_coverage_labels[45],
                                                                                    future_value == unique_future_values_art[17]  & future_variability == "art_deterioration" ~ art_coverage_labels[46],
                                                                                    future_value == unique_future_values_art[18]  & future_variability == "art_deterioration" ~ art_coverage_labels[47],
                                                                                    future_value == unique_future_values_art[19]  & future_variability == "art_deterioration" ~ art_coverage_labels[48],
                                                                                    future_value == unique_future_values_art[20]  & future_variability == "art_deterioration" ~ art_coverage_labels[49],
                                                                                    future_value == unique_future_values_art[21]  & future_variability == "art_deterioration" ~ art_coverage_labels[50],
                                                                                    future_value == unique_future_values_art[22]  & future_variability == "art_deterioration" ~ art_coverage_labels[51],
                                                                                    future_value == unique_future_values_art[23]  & future_variability == "art_deterioration" ~ art_coverage_labels[52],
                                                                                    future_value == unique_future_values_art[24]  & future_variability == "art_deterioration" ~ art_coverage_labels[53],
                                                                                    future_value == unique_future_values_art[25]  & future_variability == "art_deterioration" ~ art_coverage_labels[54],
                                                                                    future_value == unique_future_values_art[26]  & future_variability == "art_deterioration" ~ art_coverage_labels[55],
                                                                                    future_value == unique_future_values_art[27]  & future_variability == "art_deterioration" ~ art_coverage_labels[56],
                                                                                    future_value == unique_future_values_art[28]  & future_variability == "art_deterioration" ~ art_coverage_labels[57],
                                                                                    future_value == unique_future_values_art[29]  & future_variability == "art_deterioration" ~ art_coverage_labels[58]))

write_csv(art_change_inc_elim, "results/art_change_inc_elim.csv")

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
write_csv(condom_change_inc_elim, "results/condom_change_inc_elim.csv")
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


