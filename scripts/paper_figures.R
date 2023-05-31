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
test_reduction_only <- read_csv("results/test_reduction_only_summary.csv")

#### figure 1 single plots ####

fig1a <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per 1000 \n(15-49 years)\n", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) 

suppfig1a <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         indicator == "HIVinc15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
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
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced from 2025",
                                                                `2030` = "Testing reduced from 2030",
                                                                `2035` = "Testing reduced from 2035",
                                                                `2040` = "Testing reduced from 2040", 
                                                                `2045` = "Testing reduced from 2045", 
                                                                `2050` = "Testing reduced from 2050")))

fig1b <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (%) \n(15-49 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank())

suppfig1b <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (%) \n(15-49 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced from 2025",
                                                                `2030` = "Testing reduced from 2030",
                                                                `2035` = "Testing reduced from 2035",
                                                                `2040` = "Testing reduced from 2040", 
                                                                `2045` = "Testing reduced from 2045", 
                                                                `2050` = "Testing reduced from 2050")))

fig1c <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +  xlab("") +
  geom_line(aes(color = test_reduction)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (%) \n(15+ years)", labels =(function(l) {round(l*1e2,1)}), limits = c(0.5, 1)) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) 

suppfig1c <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (%) \n(15+ years)", labels =(function(l) {round(l*1e2,1)}), limits = c(0.5, 1)) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced from 2025",
                                                                `2030` = "Testing reduced from 2030",
                                                                `2035` = "Testing reduced from 2035",
                                                                `2040` = "Testing reduced from 2040", 
                                                                `2045` = "Testing reduced from 2045", 
                                                                `2050` = "Testing reduced from 2050")))


fig1d <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("Annual HIV tests per 100 \n(15+ years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) 

suppfig1d <- test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("Annual HIV tests per 100 (15+ years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_fill_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction",  "100% Testing reduction"),) + 
  scale_color_discrete(labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction")) + 
  theme(legend.title = element_blank()) +
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "Testing reduced from 2025",
                                                                `2030` = "Testing reduced from 2030",
                                                                `2035` = "Testing reduced from 2035",
                                                                `2040` = "Testing reduced from 2040", 
                                                                `2045` = "Testing reduced from 2045", 
                                                                `2050` = "Testing reduced from 2050")))

#### figure 1 panel ####
# simple grid with common legend

fig1 <- ggarrange(fig1a, fig1b, fig1c, fig1d, ncol=2, nrow=2, common.legend = TRUE, legend = "right", labels = "AUTO")

suppfig1a
suppfig1b
suppfig1c
suppfig1d

#### figure 2 ####

test_reduction_cumulative <- read_csv("results/test_reduction_only_cumulative.csv")
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

ggarrange(fig2a, fig2b, fig2c, nrow = 1, ncol = 3, common.legend = TRUE, legend = "right", labels = "AUTO")

#### figure 3 ####

art_change_inc_elim <- read_csv("THEMBISAv18/results/art_change_inc_elim.csv")
condom_change_inc_elim <- read_csv("results/condom_change_inc_elim.csv")
condom_usage_labels <- unique(condom_change_inc_elim$overall_condom_usage)
art_ret_values <- unique(art_change_inc_elim$art_ret_rate)


#### fig 3 single plots ####

# fig 3a

temp3a <- art_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         art_coverage_2035 = as.numeric(art_coverage_2035),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025)

for (i in seq(0, 100, 5)){
  art_coverage_2035 <- filter(temp3a, pitc_reduction_percentage == i)$art_coverage_2035
  mean_incidence_2100 <- filter(temp3a, pitc_reduction_percentage == i)$mean_incidence_2100
  interpolated <- data.frame(approx(x = art_coverage_2035, 
                                    y = mean_incidence_2100, 
                                    n = 100)) %>% 
    rename(art_coverage_2035 = x) %>% 
    rename(mean_incidence_2100 = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           pitc_reduction_percentage = i)
  temp3a <- bind_rows(temp3a, interpolated) 
}

temp3a <- temp3a[which(is.na(temp3a$future_value)),]

fig3a <- temp3a %>% 
  ggplot(aes(pitc_reduction_percentage, art_coverage_2035)) + 
  with_blur(
    geom_raster(aes(fill = log(1000*mean_incidence_2100)), interpolate = TRUE),
    sigma = 0
  ) +
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", binwidth = 0.5) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", breaks = 1, linewidth = 0.80) + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white") +
  geom_hline(aes(yintercept = 0.768), lty = "dotted") +
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  scale_y_continuous(name ="ART coverage (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.768, 0.90, 0.95, 1),
                     labels = c("10", "20", "30", "40", "45","50", "55", "60", "65", "70", "75", "80", "85", "", "90", "95", "100"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() +
  annotate(geom = "text", x=10, y=0.768, label="Baseline", parse = TRUE) + 
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11))

# fig 3b

temp3b <- art_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         art_coverage_2035 = as.numeric(art_coverage_2035),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025) %>% arrange(desc(art_coverage_2035)) 

for (i in seq(0, 100, 5)){
  art_coverage_2035 <- filter(temp3b, pitc_reduction_percentage == i)$art_coverage_2035
  elimination_year <- filter(temp3b, pitc_reduction_percentage == i)$elimination_year
  interpolated <- data.frame(approx(x = art_coverage_2035, 
                                    y = elimination_year, xout = seq(min(art_change_inc_elim$art_coverage_2035), max(art_change_inc_elim$art_coverage_2035),length.out = 100), 
                                    method = "linear", na.rm = FALSE)) %>% 
    rename(art_coverage_2035 = x) %>% 
    rename(elimination_year = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           pitc_reduction_percentage = i)
  temp3b <- bind_rows(temp3b, interpolated) 
}

temp3b <- temp3b[which(is.na(temp3b$future_value)),]

fig3b <- temp3b %>% 
  mutate(elimination_year = as.numeric(elimination_year), 
         pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = art_coverage_2035, z = elimination_year)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = elimination_year), interpolate = TRUE)
  ) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  geom_hline(aes(yintercept = 0.768), lty = "dotted") +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 2055) +
  scale_y_continuous(name ="ART coverage (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.768, 0.90, 0.95, 1),
                     labels = c("10", "20", "30", "40", "45","50", "55", "60", "65", "70", "75", "80", "85", "", "90", "95", "100"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + 
  annotate(geom = "text", x=90, y=0.768, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))

# fig 3c 
temp3c <- condom_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025)

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp3c, pitc_reduction_percentage == i)$overall_condom_usage
  mean_incidence_2100 <- filter(temp3c, pitc_reduction_percentage == i)$mean_incidence_2100
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean_incidence_2100, 
                                    xout = seq(min(condom_change_inc_elim$overall_condom_usage), max(condom_change_inc_elim$overall_condom_usage),length.out = 1000))) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean_incidence_2100 = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           pitc_reduction_percentage = i)
  
  
  temp3c <- bind_rows(temp3c, interpolated) 
}
temp3c <- temp3c[which(is.na(temp3c$future_value)),]

fig3c <- temp3c %>% 
  mutate(overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage), 
         log_incidence = log(1000*mean_incidence_2100)) %>% 
  filter(!overall_condom_usage %in% condom_usage_labels) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage)) + 
  with_blur(geom_raster(aes(fill = as.numeric(log_incidence)), interpolate = TRUE),sigma=0) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9)) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", breaks = 1, linewidth = 0.80) +
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white",breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  geom_hline(aes(yintercept = 33.9), lty = "dotted")+
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  ylab("Overall condom usage (%)") + 
  scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() +
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11)) +
  annotate(geom = "text", x = 15, y = 33, label = "Baseline")

# fig 3d

temp3d <- condom_change_cumulative %>% 
  mutate(overall_condom_usage = as.numeric(overall_condom_usage),
         test_reduction = as.numeric(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025) %>% arrange(desc(overall_condom_usage)) 

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp3d, test_reduction == i)$overall_condom_usage
  elimination_year <- filter(temp3d, test_reduction == i)$elimination_year
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = elimination_year, seq(min(condom_change_inc_elim$overall_condom_usage), max(condom_change_inc_elim$overall_condom_usage),length.out = 1000), 
                                    method = "linear", na.rm = FALSE)) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(elimination_year = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp3d <- bind_rows(temp3d, interpolated) 
}

temp3d <- temp3d[which(is.na(temp3d$future_value)),]

fig3d <- temp3d %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage)) +
  with_blur(geom_raster(aes(fill = elimination_year), interpolate = TRUE, na.rm = TRUE), sigma = 0) +
  geom_contour(aes(z = elimination_year),color = "black") + 
  geom_text_contour(aes(z = elimination_year), skip = 0, 
                    stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, 
                    min.size = 10, label.placer = label_placer_flattest()) +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 2055) +
  ylab("Overall condom usage (%)") + 
  scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") +
  geom_hline(aes(yintercept = 33.9), lty = "dotted")+
  annotate(geom = "text", x=90, y=33, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))


fig3ac <- ggarrange(fig3a, fig3c, nrow = 2, common.legend = TRUE, legend = "right", labels = c("A","C"))
fig3bd <- ggarrange(fig3b, fig3d, nrow = 2, common.legend = TRUE, legend = "right", labels = c("B","D"))

ggarrange(fig3ac, fig3bd, ncol = 2)


#### figure 4 ####
cumulative_art_change <- read_csv("THEMBISAv18/results/art_change_cumulative.csv")
condom_change_cumulative <- read_csv("results/condom_change_cumulative.csv")


#### fig 4 single plots ####

# fig 4a

temp4a <- cumulative_art_change %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  art_coverage_2035 <- filter(temp4a, test_reduction == i)$art_coverage_2035
  mean <- filter(temp4a, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = art_coverage_2035, 
                                    y = mean, 
                                    xout = seq(min(cumulative_art_change$art_coverage_2035), max(cumulative_art_change$art_coverage_2035),length.out = 100))) %>% 
    rename(art_coverage_2035 = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4a <- bind_rows(temp4a, interpolated) 
}

temp4a <- temp4a[which(is.na(temp4a$future_value)),]

fig4a <- temp4a %>% 
  ggplot(aes(test_reduction, art_coverage_2035, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nHIV infections"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 0.768), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.768, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  scale_y_continuous(name ="ART coverage (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.768, 0.90, 0.95, 1),
                     labels = c("10", "20", "30", "40", "45","50", "55", "60", "65", "70", "75", "80", "85", "", "90", "95", "100"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

# fig 4b

temp4b <- cumulative_art_change %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  art_coverage_2035 <- filter(temp4b, test_reduction == i)$art_coverage_2035
  mean <- filter(temp4b, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = art_coverage_2035, 
                                    y = mean, 
                                    xout = seq(min(cumulative_art_change$art_coverage_2035), max(cumulative_art_change$art_coverage_2035),length.out = 100))) %>% 
    rename(art_coverage_2035 = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4b <- bind_rows(temp4b, interpolated) 
}

temp4b <- temp4b[which(is.na(temp4b$future_value)),]

fig4b <- temp4b %>% 
  ggplot(aes(test_reduction, art_coverage_2035, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nAIDS-related \ndeaths"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 0.768), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.768, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nAIDS \nrelated \ndeaths \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  scale_y_continuous(name ="ART coverage (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.768, 0.90, 0.95, 1),
                     labels = c("10", "20", "30", "40", "45","50", "55", "60", "65", "70", "75", "80", "85", "", "90", "95", "100"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

# fig 4c 
temp4c <- condom_change_cumulative %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp4c, test_reduction == i)$overall_condom_usage
  mean <- filter(temp4c, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean, 
                                    xout = seq(min(condom_usage_labels), max(condom_usage_labels), length.out = 100))) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4c <- bind_rows(temp4c, interpolated) 
}

temp4c <- temp4c[which(is.na(temp4c$future_value)),]

fig4c <- temp4c %>% 
ggplot(aes(test_reduction, overall_condom_usage, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nHIV infections"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 33), lty = "dotted") +
  annotate(geom = "text", x=50, y=33, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

# fig 4d

temp4d <- condom_change_cumulative %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp4d, test_reduction == i)$overall_condom_usage
  mean <- filter(temp4d, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean, 
                                    xout = seq(min(condom_usage_labels), max(condom_usage_labels), length.out = 100))) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4d <- bind_rows(temp4d, interpolated) 
}

temp4d <- temp4d[which(is.na(temp4d$future_value)),]

fig4d <- temp4d %>% 
  ggplot(aes(test_reduction, overall_condom_usage, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nAIDS-related \ndeaths"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 33), lty = "dotted") +
  annotate(geom = "text", x=50, y=33, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nAIDS \nrelated \ndeaths \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")


fig4ac <- ggarrange(fig4a, fig4c, nrow = 2, common.legend = TRUE, legend = "right", labels = c("A","C"))
fig4bd <- ggarrange(fig4b, fig4d, nrow = 2, common.legend = TRUE, legend = "right", labels = c("B","D"))

ggarrange(fig4ac, fig4bd, ncol = 2)

#### new versions of figure 3 and 4 ####
ggarrange(fig3a, fig3b, fig4a, fig4b, ncol = 2, nrow = 2, legend = "right", labels = c("A", "B", "C", "D"))
ggarrange(fig3c, fig3d, fig4c, fig4d, ncol = 2, nrow = 2, legend = "right", labels = c("A", "B", "C", "D"))

#### additional figures ####
#### art increase ####
#### incidence ####
art_change_summary <- art_change_summary %>% mutate(test_reduction = factor(test_reduction, levels = as.character(seq(0,100,5))))
art_inc_incidence <- art_change_summary %>%  
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100), 
         future_value == 0.07, future_variability == "art_improvement") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "HIVinc15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_improvement"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "HIVinc15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_improvement"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per 1000 \n(15-49 years)\n", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### prevalence ####
art_inc_prev <- art_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 0.07, future_variability == "art_improvement") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "Prev15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_improvement"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "Prev15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_improvement"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (%) \n(15-49 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 


#### art coverage ####

art_inc_art_cov <- art_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 0.07, future_variability == "art_improvement") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +  xlab("") +
  geom_line(aes(color = test_reduction)) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "ARTcoverageAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_improvement"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "ARTcoverageAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_improvement"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (%) \n(15+ years)", labels =(function(l) {round(l*1e2,1)}), limits = c(0.5, 1)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### HIV test per adult ####

art_inc_test <- art_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 0.07, future_variability == "art_improvement") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "TestsPerAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_improvement"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "TestsPerAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_improvement"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV tests per 100 \n(over 15 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

ggarrange(art_inc_incidence, art_inc_prev, art_inc_art_cov, art_inc_test, ncol=2, nrow=2, common.legend = TRUE, legend = "right", labels = "AUTO") + ggtitle("ART retention rate in 2035 = 93%")


#### art decrease ####
#### incidence ####
art_dec_incidence <- art_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100), 
         future_value == 0.07, future_variability == "art_deterioration") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "HIVinc15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_deterioration"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "HIVinc15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_deterioration"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per 1000 \n(15-49 years)\n", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### prevalence ####
art_dec_prev <- art_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 0.07, future_variability == "art_deterioration") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "Prev15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_deterioration"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "Prev15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_deterioration"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (%) \n(15-49 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 


#### art coverage ####

art_dec_art_cov <- art_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 0.07, future_variability == "art_deterioration") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +  xlab("") +
  geom_line(aes(color = test_reduction)) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "ARTcoverageAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_deterioration"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "ARTcoverageAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_deterioration"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (%) \n(15+ years)", labels =(function(l) {round(l*1e2,1)}), limits = c(0.5, 1)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### HIV test per adult ####

art_dec_test <- art_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 0.07, future_variability == "art_deterioration") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(art_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "TestsPerAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 0.07 , future_variability == "art_deterioration"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(art_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "TestsPerAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 0.07 , future_variability == "art_deterioration"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV tests per 100 \n(over 15 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

ggarrange(art_dec_incidence, art_dec_prev, art_dec_art_cov, art_dec_test, ncol=2, nrow=2, common.legend = TRUE, legend = "right", labels = "AUTO") + ggtitle("ART retention rate in 2035 = 93%")


#### condom increase ####
#### incidence ####
con_inc_incidence <- condom_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100), 
         future_value == 6, future_variability == "condom_promotion") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "HIVinc15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "HIVinc15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_promotion"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per 1000 \n(15-49 years)\n", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### prevalence ####
con_inc_prev <- condom_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 6, future_variability == "condom_promotion") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "Prev15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "Prev15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_promotion"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (%) \n(15-49 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 


#### art coverage ####

con_inc_art_cov <- condom_change_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 6, future_variability == "condom_promotion") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +  xlab("") +
  geom_line(aes(color = test_reduction)) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "ARTcoverageAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "ARTcoverageAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_promotion"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (%) \n(15+ years)", labels =(function(l) {round(l*1e2,1)}), limits = c(0.5, 1)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### HIV test per adult ####

con_inc_test <- condom_change_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 6, future_variability == "condom_promotion") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "TestsPerAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "TestsPerAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_promotion"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV tests per 100 \n(over 15 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

ggarrange(con_inc_incidence, con_inc_prev, con_inc_art_cov, con_inc_test, ncol=2, nrow=2, common.legend = TRUE, legend = "right", labels = "AUTO") + ggtitle("Condom usage in in 2035 = 93%")

#### condom decrease ####
#### incidence ####
condom_change_summary <- condom_change_summary %>% mutate(test_reduction = factor(test_reduction, levels = as.character(seq(0,100,5))))
con_dec_incidence <- condom_change_summary %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100), 
         future_value == 6 , future_variability == "condom_reduction") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "HIVinc15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "HIVinc15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_reduction"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per 1000 \n(15-49 years)\n", labels =(function(l) {round(l*1e3,1)}), breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### prevalence ####
con_dec_prev <- condom_change_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 6, future_variability == "condom_reduction") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "Prev15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "Prev15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_reduction"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV prevalence (%) \n(15-49 years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 


#### art coverage ####

con_dec_art_cov <- condom_change_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 6, future_variability == "condom_reduction") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +  xlab("") +
  geom_line(aes(color = test_reduction)) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "ARTcoverageAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "ARTcoverageAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_reduction"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.10, show.legend = F) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART coverage (%) \n(15+ years)", labels =(function(l) {round(l*1e2,1)}), limits = c(0.5, 1)) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

#### HIV test per adult ####

con_dec_test <- condom_change_summary %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "TestsPerAdult",
         year > 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100),
         future_value == 6, future_variability == "condom_reduction") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "TestsPerAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 6 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "TestsPerAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 6 , future_variability == "condom_reduction"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.15, show.legend = F) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("Annual HIV tests per 100 \n(15+ years)", labels =(function(l) {round(l*1e2,1)})) +
  scale_color_manual(values = c("0" = "red", "25" = "yellow3", "50" = "green3", "75" = "dodgerblue2",  "100" = "magenta3", "baseline" = "grey"),
                     labels = c("No Testing reduction", "25% Testing reduction", "50% Testing reduction", "75% Testing reduction", "100% Testing reduction", "Status quo"),
                     aesthetics = c("colour", "fill"), breaks = c("0" ,"25", "50", "75", "100", "baseline")) + 
  theme(legend.title = element_blank()) 

ggarrange(con_dec_incidence, con_dec_prev, con_dec_art_cov, con_dec_test, ncol=2, nrow=2, common.legend = TRUE, legend = "right", labels = "AUTO") + ggtitle("Condom usage in in 2035 = 93%")

