library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(metR)
library(ggpubr)
library(ggfx)

#### figure 1 ####
setwd("/Users/stefan/Documents/HIV_EndGame_SA/")
test_reduction_only <- read_csv("THEMBISAV18/results/test_reduction_only_summary.csv")

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
  scale_y_continuous("HIV incidence per 1000 \n(15-49 years)", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
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
  scale_y_continuous("HIV prevalence \n(15-49 years)") +
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
  scale_y_continuous("ART coverage \n(over 15 years)", limits = c(0.5, 1)) +
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
  scale_y_continuous("HIV tests per 100 \n(over 15 years)", labels =(function(l) {round(l*1e2,1)})) +
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
# simple grid with common legend

ggarrange(fig1a, fig1b, fig1c, fig1d, ncol=2, nrow=2, common.legend = TRUE, legend = "right", labels = "AUTO")

#### figure 2 ####

test_reduction_cumulative <- read_csv("THEMBISAv18/results/cumulative_test_reduction_only_summary.csv")
test_reduction_only_inc_elim <- read_csv("results/test_reduction_only_inc_elim.csv")

#### fig 2 single plots ####

fig2a <- test_reduction_only_inc_elim %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         pitc_reduction_percentage = as.factor(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_percentage %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(pitc_reduction_percentage, mean_incidence_2100, group = pitc_reduction_year, fill = pitc_reduction_year)) + 
  geom_col(aes(color = pitc_reduction_year), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  xlab("Testing reduction (%)") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence in 2100 \nper 1000 (15-49 years)", labels = (function(l) {round(l*1e3,1)})) + 
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11)) + 
  scale_color_brewer("Testing \nreduction \nyear",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Testing \nreduction \nyear", palette = "Dark2",direction = -1)

fig2b <- test_reduction_cumulative %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif", 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(test_reduction, mean, group = pitc_reduction_year, fill = pitc_reduction_year)) +
  geom_col(aes(color = pitc_reduction_year), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("Additional HIV \ninfections (millions)", 
                     labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Testing \nreduction \nyear",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Testing \nreduction \nyear", palette = "Dark2",direction = -1) +
  xlab("Testing reduction (%)") + theme_classic() +
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11))

fig2c <- test_reduction_cumulative %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif", 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year), 
         test_reduction = as.factor(test_reduction)) %>% 
  ggplot(aes(test_reduction, mean, group = pitc_reduction_year, fill = pitc_reduction_year)) +
  geom_col(aes(color = pitc_reduction_year), position =  position_dodge(width = 0.85), width = 0.85, alpha = 0.75) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), show.legend = F, position =  position_dodge(width = 0.85), alpha = 0.5) +
  scale_y_continuous("Additional AIDS-related \ndeaths (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_color_brewer("Testing \nreduction \nyear",palette = "Dark2",direction = -1) +
  scale_fill_brewer("Testing \nreduction \nyear", palette = "Dark2",direction = -1) +
  xlab("Testing reduction (%)") + theme_classic() +
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11))

#### fig 2 panel ####

fig2 <-ggarrange(fig2a, fig2b, fig2c, nrow = 1, ncol = 3, common.legend = TRUE, legend = "right", labels = "AUTO")

#### figure 3 ####

art_change_inc_elim <- read_csv("results/art_change_inc_elim.csv")

fig3a <- art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                               pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_ret_rate)) + 
  with_blur(
    geom_raster(aes(fill = log(1000*mean_incidence_2100)), interpolate = TRUE),
    sigma = 6
  ) +
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", binwidth = 0.5) + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white") +
  geom_hline(aes(yintercept = 1-0.22), lty = "dotted") +
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  scale_y_continuous(name ="Female ART retention rate",
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.78, 0.8, 0.9),
                     labels = c("0.1", "0.2", "0.3", "0.4","0.5", "0.6", "0.7","", "0.8", "0.9"),
                     trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic() +
  annotate(geom = "text", x=15, y=0.78, label="Baseline", parse = TRUE) + 
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11))
fig3a

fig3b <- art_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                        pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = art_ret_rate, z = elimination_year)) + 
  with_blur(sigma = 6,
            geom_raster(aes(fill = elimination_year), interpolate = TRUE)
  ) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  geom_hline(aes(yintercept = 0.78), lty = "dotted") +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue", 
                       midpoint = 2055) +
  scale_y_continuous(name ="Female ART retention rate", breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.78, 0.8, 0.9),
                     labels = c("0.1", "0.2", "0.3", "0.4","0.5", "0.6", "0.7", "", "0.8", "0.9"),
                     trans = "logit") +
  xlab("Testing reduction (%)") + 
  annotate(geom = "text", x=90, y=0.78, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))
fig3b

fig3c <- condom_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         pitc_reduction_year = as.factor(pitc_reduction_year),
         overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage)) + 
  with_blur(
    sigma = 8, 
    geom_raster(aes(fill = log(1000*mean_incidence_2100)),interpolate = TRUE)) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black") + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white") +
  geom_hline(aes(yintercept = 33.0), lty = "dotted")+
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() +
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11)) +
  annotate(geom = "text", x = 15, y = 33, label = "Baseline")

fig3d <- condom_change_inc_elim %>% mutate(elimination_year = as.numeric(elimination_year), 
                                           pitc_reduction_year = as.factor(pitc_reduction_year),
                                           overall_condom_usage = as.numeric(overall_condom_usage)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage, z = elimination_year)) +
  with_blur(
  geom_raster(aes(fill = elimination_year), interpolate = TRUE),
  sigma = 10) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue", 
                       midpoint = 2055) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") +
  geom_hline(aes(yintercept = 33.0), lty = "dotted")+
  annotate(geom = "text", x=90, y=33, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))
fig3d


fig3ac <- ggarrange(fig3a, fig3c, nrow = 2, common.legend = TRUE, legend = "right", labels = c("A","C"))
fig3bd <- ggarrange(fig3b, fig3d, nrow = 2, common.legend = TRUE, legend = "right", labels = c("B","D"))

ggarrange(fig3ac, fig3bd, ncol = 2)


#### figure 4 ####
cumulative_art_change <- read_csv("THEMBISAv18/results/art_change_cumulative.csv")
condom_change_cumulative <- read_csv("results/condom_change_cumulative.csv")

#### fig 4 single plots ####

fig4a <- cumulative_art_change %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  with_blur(sigma = 8 ,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nHIV infections"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 1-0.22), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.78, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue", 
                       midpoint = 0) +
  ylab("Female ART retention rate") + 
  scale_y_continuous(name ="Female ART retention rate",
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.78, 0.8, 0.9),
                     labels = c("0.1", "0.2", "0.3", "0.4","0.5", "0.6", "0.7","", "0.8", "0.9"),
                     trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

fig4b <- cumulative_art_change %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  with_blur(sigma = 8 ,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nAIDS-related \ndeaths"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 1-0.22), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.78, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nAIDS-related \ndeaths \n(millions)",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue", 
                       midpoint = 0) +
  ylab("Female ART retention rate") + 
  scale_y_continuous(name ="Female ART retention rate",
                     breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.78, 0.8, 0.9),
                     labels = c("0.1", "0.2", "0.3", "0.4","0.5", "0.6", "0.7","", "0.8", "0.9"),
                     trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

fig4c <- condom_change_cumulative %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, overall_condom_usage, z = mean/10**6)) + 
  with_blur(sigma = 8 ,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nHIV infections"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 33), lty = "dotted") +
  annotate(geom = "text", x=50, y=33, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue", 
                       midpoint = 0) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

fig4d <- condom_change_cumulative %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, overall_condom_usage, z = mean/10**6)) + 
  with_blur(sigma = 8 ,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nAIDS-related \ndeaths"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 33), lty = "dotted") +
  annotate(geom = "text", x=50, y=33, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nAIDS-related \ndeaths \n(millions)",
                       low = "red",
                       mid = "lightyellow",
                       high = "blue", 
                       midpoint = 0) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

fig4a
fig4b
fig4c
fig4d

fig4ac <- ggarrange(fig4a, fig4c, nrow = 2, common.legend = TRUE, legend = "right", labels = c("A","C"))
fig4bd <- ggarrange(fig4b, fig4d, nrow = 2, common.legend = TRUE, legend = "right", labels = c("B","D"))

ggarrange(fig4ac, fig4bd, ncol = 2)
