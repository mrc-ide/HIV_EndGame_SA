#### library #####

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(metR)
library(ggpubr)
library(ggfx)

#### load data ####

condom_change_inc_elim <- read_csv("results/condom_change_inc_elim.csv")
art_change_inc_elim <- read_csv("results/art_change_inc_elim.csv")
cumulative_art_change <- read_csv("THEMBISAv18/results/art_change_cumulative.csv")
condom_change_cumulative <- read_csv("results/condom_change_cumulative.csv")

#### using approx function ####

#### condom change incidence heatmap ####
condom_usage_labels <- unique(condom_change_inc_elim$overall_condom_usage)

temp <- condom_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025)

temp %>%
  mutate(overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage), 
         log_incidence = log(1000*mean_incidence_2100)) %>% 
  filter(overall_condom_usage %in% condom_usage_labels) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage)) + 
  with_blur(geom_raster(aes(fill = as.numeric(log_incidence)), interpolate = TRUE),sigma=0) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9)) + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white",breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  geom_hline(aes(yintercept = 33.0), lty = "dotted")+
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() +
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11)) +
  annotate(geom = "text", x = 15, y = 33, label = "Baseline")
  
for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp, pitc_reduction_percentage == i)$overall_condom_usage
  mean_incidence_2100 <- filter(temp, pitc_reduction_percentage == i)$mean_incidence_2100
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean_incidence_2100, 
                                    n = n)) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean_incidence_2100 = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           pitc_reduction_percentage = i)
    
  
  temp <- bind_rows(temp, interpolated) 
}

temp %>%
  mutate(overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage), 
         log_incidence = log(1000*mean_incidence_2100)) %>% 
  filter(!overall_condom_usage %in% condom_usage_labels) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage)) + 
  with_blur(geom_raster(aes(fill = as.numeric(log_incidence)), interpolate = TRUE),sigma=0) + 
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9)) + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white",breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  geom_hline(aes(yintercept = 33.0), lty = "dotted")+
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() +
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11)) +
  annotate(geom = "text", x = 15, y = 33, label = "Baseline")


#### condom change elimination year ####

temp <- condom_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025)

temp %>% 
  mutate(elimination_year = as.numeric(elimination_year), 
         pitc_reduction_year = as.factor(pitc_reduction_year),
         overall_condom_usage = as.numeric(overall_condom_usage)) %>% 
  filter(overall_condom_usage %in% condom_usage_labels) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = overall_condom_usage, z = elimination_year)) +
  with_blur(
    geom_raster(aes(fill = elimination_year), interpolate = TRUE),
    sigma = 0) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 2055) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") +
  geom_hline(aes(yintercept = 33.0), lty = "dotted")+
  annotate(geom = "text", x=90, y=33, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))

temp <- condom_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         overall_condom_usage = as.numeric(overall_condom_usage),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025) %>% arrange(desc(overall_condom_usage)) 

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp, pitc_reduction_percentage == i)$overall_condom_usage
  elimination_year <- filter(temp, pitc_reduction_percentage == i)$elimination_year
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = elimination_year, xout = seq(13.5, 57.3,length.out = 100), 
                                    method = "linear", na.rm = FALSE)) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(elimination_year = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           pitc_reduction_percentage = i)
  temp <- bind_rows(temp, interpolated) 
}

temp <- temp %>% 
  mutate(elimination_year = as.numeric(elimination_year), 
            pitc_reduction_year = as.factor(pitc_reduction_year),
            overall_condom_usage = as.numeric(overall_condom_usage)) %>% 
  filter(!overall_condom_usage %in% condom_usage_labels) 

temp %>% 
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
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "identity", breaks = seq(0, 55,5)) +
  xlab("Testing reduction (%)") +
  geom_hline(aes(yintercept = 33.0), lty = "dotted")+
  annotate(geom = "text", x=90, y=33, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))



#### art change heatmap ####

#### HIV incidence ####
art_ret_values <- unique(art_change_inc_elim$art_ret_rate)
temp <- art_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         art_ret_rate = as.numeric(art_ret_rate),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025)

temp %>% 
  mutate(elimination_year = as.numeric(elimination_year), 
             pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(pitc_reduction_percentage, art_ret_rate)) + 
  with_blur(
    geom_raster(aes(fill = log(1000*mean_incidence_2100)), interpolate = TRUE),
    sigma = 0
  ) +
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", binwidth = 0.5) + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white") +
  geom_hline(aes(yintercept = 0.8514), lty = "dotted") +
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() +
  annotate(geom = "text", x=10, y=0.8514, label="Baseline", parse = TRUE) + 
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11))


temp <- art_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         art_ret_rate = as.numeric(art_ret_rate),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025)

for (i in seq(0, 100, 5)){
  art_ret_rate <- filter(temp, pitc_reduction_percentage == i)$art_ret_rate
  mean_incidence_2100 <- filter(temp, pitc_reduction_percentage == i)$mean_incidence_2100
  interpolated <- data.frame(approx(x = art_ret_rate, 
                                    y = mean_incidence_2100, 
                                    n = 100)) %>% 
    rename(art_ret_rate = x) %>% 
    rename(mean_incidence_2100 = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           pitc_reduction_percentage = i)
  temp <- bind_rows(temp, interpolated) 
}

temp <- temp[which(is.na(temp$future_value)),]

temp %>% 
  ggplot(aes(pitc_reduction_percentage, art_ret_rate)) + 
  with_blur(
    geom_raster(aes(fill = log(1000*mean_incidence_2100)), interpolate = TRUE),
    sigma = 0
  ) +
  geom_contour(aes(z = 1000*mean_incidence_2100), color = "black", binwidth = 0.5) + 
  geom_text_contour(aes(z = 1000*mean_incidence_2100),skip = 0, stroke = 0.1, stroke.colour = "white") +
  geom_hline(aes(yintercept = 0.8514), lty = "dotted") +
  scale_fill_gradient2("Mean HIV \nincidence \n(15-49y) \nper 1000 \nin 2100",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red",
                       midpoint = 0,
                       labels = (function(l) {round(exp(l),2)})
  ) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() +
  annotate(geom = "text", x=10, y=0.8514, label="Baseline", parse = TRUE) + 
  theme(axis.text = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11))

#### elimination year ####

temp <- art_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         art_ret_rate = as.numeric(art_ret_rate),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025)

temp %>% 
  mutate(elimination_year = as.numeric(elimination_year), 
         pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = art_ret_rate, z = elimination_year)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = elimination_year), interpolate = TRUE)
  ) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  geom_hline(aes(yintercept = 0.8514), lty = "dotted") +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 2055) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + 
  annotate(geom = "text", x=90, y=0.8514, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))

temp <- art_change_inc_elim %>% 
  mutate(elimination_year = as.numeric(elimination_year),
         art_ret_rate = as.numeric(art_ret_rate),
         pitc_reduction_percentage = as.numeric(pitc_reduction_percentage)) %>% 
  filter(pitc_reduction_year == 2025) %>% arrange(desc(art_ret_rate)) 

for (i in seq(0, 100, 5)){
  art_ret_rate <- filter(temp, pitc_reduction_percentage == i)$art_ret_rate
  elimination_year <- filter(temp, pitc_reduction_percentage == i)$elimination_year
  interpolated <- data.frame(approx(x = art_ret_rate, 
                                    y = elimination_year, xout = seq(min(art_ret_values), max(art_ret_values),length.out = 100), 
                                    method = "linear", na.rm = FALSE)) %>% 
    rename(art_ret_rate = x) %>% 
    rename(elimination_year = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           pitc_reduction_percentage = i)
  temp <- bind_rows(temp, interpolated) 
}

temp <- temp[which(is.na(temp$future_value)),]

temp %>% 
  mutate(elimination_year = as.numeric(elimination_year), 
         pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(x = pitc_reduction_percentage, y = art_ret_rate, z = elimination_year)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = elimination_year), interpolate = TRUE)
  ) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white",check_overlap = TRUE) +
  geom_hline(aes(yintercept = 0.8514), lty = "dotted") +
  scale_fill_gradient2("HIV \nelimination \nyear",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 2055) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + 
  annotate(geom = "text", x=90, y=0.8514, label="Baseline", parse = TRUE) + 
  theme_classic() + theme(axis.text = element_text(size = 11),
                          axis.title.y = element_text(size = 11),
                          axis.title.x = element_text(size = 11),
                          legend.text = element_text(size = 11))


#### additional infections art change ####
temp <- cumulative_art_change %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")
temp %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nHIV infections"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 0.8514000), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.8514000, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

temp <- cumulative_art_change %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  art_ret_rate <- filter(temp, test_reduction == i)$art_ret_rate
  mean <- filter(temp, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = art_ret_rate, 
                                    y = mean, 
                                    xout = seq(min(art_ret_values), max(art_ret_values), length.out = 100))) %>% 
    rename(art_ret_rate = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp <- bind_rows(temp, interpolated) 
}

temp <- temp[which(is.na(temp$future_value)),]

temp %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nHIV infections"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 0.8514000), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.8514000, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

#### additional deaths art change ####

temp <- cumulative_art_change %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

temp %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nAIDS-related \ndeaths"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 0.8514000), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.8514000, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nAIDS \nrelated \ndeaths \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

temp <- cumulative_art_change %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  art_ret_rate <- filter(temp, test_reduction == i)$art_ret_rate
  mean <- filter(temp, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = art_ret_rate, 
                                    y = mean, 
                                    xout = seq(min(art_ret_values), max(art_ret_values), length.out = 100))) %>% 
    rename(art_ret_rate = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp <- bind_rows(temp, interpolated) 
}

temp <- temp[which(is.na(temp$future_value)),]

temp %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  with_blur(sigma = 0,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "No additional \nAIDS-related \ndeaths"),breaks = 0,show.legend = FALSE) + 
  geom_text_contour(stroke = 0.1, stroke.colour = "white",check_overlap = TRUE, breaks = 0) +
  geom_hline(aes(yintercept = 0.8514000), lty = "dotted") +
  annotate(geom = "text", x=50, y=0.8514000, label="Baseline", parse = TRUE) + 
  scale_fill_gradient2("Additional \nAIDS \nrelated \ndeaths \n(millions)",
                       low = "blue",
                       mid = "lightyellow",
                       high = "red", 
                       midpoint = 0) +
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity") +
  xlab("Testing reduction (%)") + theme_classic() + theme(axis.text = element_text(size = 11),
                                                          axis.title.y = element_text(size = 11),
                                                          axis.title.x = element_text(size = 11),
                                                          legend.text = element_text(size = 11)) + 
  scale_color_discrete("",type = "black")

#### additional infections condom change ####

temp <- condom_change_cumulative %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")

temp %>% 
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

temp <- condom_change_cumulative %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp, test_reduction == i)$overall_condom_usage
  mean <- filter(temp, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean, 
                                    xout = seq(min(condom_usage_labels), max(condom_usage_labels), length.out = 100))) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp <- bind_rows(temp, interpolated) 
}

temp <- temp[which(is.na(temp$future_value)),]

temp %>% 
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

#### additional deaths condom change ####

temp <- condom_change_cumulative %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

temp %>% 
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

temp <- condom_change_cumulative %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp, test_reduction == i)$overall_condom_usage
  mean <- filter(temp, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean, 
                                    xout = seq(min(condom_usage_labels), max(condom_usage_labels), length.out = 100))) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp <- bind_rows(temp, interpolated) 
}

temp <- temp[which(is.na(temp$future_value)),]

temp %>% 
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
