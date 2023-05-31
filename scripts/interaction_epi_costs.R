#### cumulative heatmaps ####

art_change_values <- seq(0, 14, 0.5)
filepaths <- paste0("results/cumulative_decrease_art_retention_", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
cumulative_decrease_art_retention <- bind_rows(temp, .id = "art_int_reduction")

filepaths <- paste0("results/cumulative_increase_art_retention_", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
cumulative_increase_art_retention <- bind_rows(temp, .id = "art_int_improvement")

cumulative_art_change <- bind_rows(cumulative_decrease_art_retention, 
                                   cumulative_increase_art_retention)

cumulative_art_change <- cumulative_art_change %>% 
  # select(-c(art_int_improvement, art_int_reduction)) %>% 
  mutate(art_int_rate = case_when(
    future_variability == "art_deterioration" ~ (0.1486*((1+(as.numeric(future_value)))**10)),
    future_variability == "art_improvement" ~ (0.1486*((1-(as.numeric(future_value)))**10))))
cumulative_art_change <- cumulative_art_change %>% mutate(art_ret_rate = 1- art_int_rate)
write_csv(cumulative_art_change, "THEMBISAv18/results/art_change_cumulative.csv")
#### plotting ####

# additional HIV infections #

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif", 
         art_int_rate <= 0.53 |
         art_int_rate >= 0.9) %>% 
  ggplot(aes(test_reduction, mean, group = pitc_reduction_year, color = as.factor(art_int_rate))) +
  geom_point() + 
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI)) +
  facet_wrap(~pitc_reduction_year,labeller = as_labeller(c())) + 
  xlab("PITC reduction (%)") + 
  theme_classic()
  

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif",
         pitc_reduction_year == 2025,
         #test_reduction != 10, test_reduction !=30, test_reduction !=50, test_reduction !=70, test_reduction !=90
         ) %>% 
  ggplot(aes(as.factor(test_reduction), mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_crossbar(aes(ymin = lower_CI, ymax = upper_CI, color = as.factor(art_int_rate)), show.legend = T,width = 0.5, position = position_dodge(width = 1), alpha = 0.8) +
  geom_point(aes(color = as.factor(pitc_reduction_year)), shape = 18, size =1, position = position_dodge(width = 1), show.legend = F, color = "white") +
  xlab("Testing reduction (%)") + scale_y_continuous("Additional HIV infections (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete("Female ART \nretention rate") + 
  scale_color_discrete("Female ART \nretention rate") +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + geom_hline(aes(yintercept = 0), lty = "dotted")

#### Figure 4 ####

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif" ,
         pitc_reduction_year == 2025,
  ) %>% 
  ggplot(aes(test_reduction, mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), show.legend = T,alpha = 0.05) +
  geom_line(aes(color = as.factor(art_int_rate)),show.legend = T) +
  xlab("Testing reduction (%)") + scale_y_continuous("Additional HIV infections (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete("Female ART \nretention rate", labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  scale_color_discrete("Female ART \nretention rate",labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + geom_hline(aes(yintercept = 0), lty = "dotted")

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif",
         pitc_reduction_year == 2025,
  ) %>% 
  ggplot(aes(test_reduction, mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), show.legend = T,alpha = 0.05) +
  geom_line(aes(color = as.factor(art_int_rate)),show.legend = T) +
  xlab("Testing reduction (%)") + scale_y_continuous("Additional AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete("Female ART \nretention rate", labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  scale_color_discrete("Female ART \nretention rate",labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + geom_hline(aes(yintercept = 0), lty = "dotted")


#### cumulative infection and deaths 

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "intervention",
         pitc_reduction_year == 2025,
  ) %>% 
  ggplot(aes(test_reduction, mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), show.legend = T,alpha = 0.05) +
  geom_line(aes(color = as.factor(art_int_rate)),show.legend = T) +
  xlab("Testing reduction (%)") + scale_y_continuous("Cumulative HIV infections (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete("Female ART \nretention rate", labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  scale_color_discrete("Female ART \nretention rate",labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + geom_hline(aes(yintercept = 3.788054e+06), lty = "dotted")

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "intervention",
         pitc_reduction_year == 2025,
  ) %>% 
  ggplot(aes(test_reduction, mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), show.legend = T,alpha = 0.05) +
  geom_line(aes(color = as.factor(art_int_rate)),show.legend = T) +
  xlab("Testing reduction (%)") + scale_y_continuous("Cumulative AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete("Female ART \nretention rate", labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  scale_color_discrete("Female ART \nretention rate",labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + geom_hline(aes(yintercept = 1.745765e+06), lty = "dotted")


#### change from baseline ####

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "percent_change",
         pitc_reduction_year == 2025,
  ) %>% 
  ggplot(aes(test_reduction, mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), show.legend = T,alpha = 0.05) +
  geom_line(aes(color = as.factor(art_int_rate)),show.legend = T) +
  xlab("Testing reduction (%)") + scale_y_continuous("Change from baseline: HIV infections (%)") +
  scale_fill_discrete("Female ART \nretention rate", labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  scale_color_discrete("Female ART \nretention rate",labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + geom_hline(aes(yintercept = 0), lty = "dotted")

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "percent_change",
         pitc_reduction_year == 2025,
  ) %>% 
  ggplot(aes(test_reduction, mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), show.legend = T,alpha = 0.05) +
  geom_line(aes(color = as.factor(art_int_rate)),show.legend = T) +
  xlab("Testing reduction (%)") + scale_y_continuous("Additional AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete("Female ART \nretention rate", labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  scale_color_discrete("Female ART \nretention rate",labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + geom_hline(aes(yintercept = 0), lty = "dotted")



#### finding where absolute difference = 0 ####
library(metR)
cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  with_blur(sigma = 0 ,
            geom_raster(aes(fill = mean/10**6), interpolate = TRUE)) +
  geom_contour(aes(color = "0 additional \ninfections"),breaks = 0,show.legend = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "navyblue",
                       mid = "white",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Female ART retention rate") + scale_y_continuous(trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic() + scale_color_discrete("",type = "black")

#### plotting only the contour lines where additional HIV infections = 0 for different years ####

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = (1-art_int_rate)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_int_rate, z = mean/10**6, group = pitc_reduction_year, color = pitc_reduction_year)) + 
  geom_contour(breaks = 0,show.legend = TRUE) + 
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "white",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Female ART retention rate") + scale_y_continuous() +
  xlab("Testing reduction (%)") + theme_classic() + scale_color_discrete("")


#### deaths ####

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_ret_rate, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) +
  geom_contour(aes(color = "0 additional \ndeaths"),breaks = 0,show.legend = TRUE) + 
  scale_fill_gradient2("Additional \nAIDS-related \ndeaths \n(millions)",
                       low = "navyblue",
                       mid = "white",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Female ART retention rate") + scale_y_continuous(trans = "logit") +
  xlab("Testing reduction (%)") + theme_classic() + scale_color_discrete("",type = "black")

#### old stuff ####



cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif",
         pitc_reduction_year == 2025,
         art_int_rate == 0.9 | art_int_rate == 0.78 | art_int_rate == 0.53,  
         test_reduction != 10, test_reduction !=30, test_reduction !=50, test_reduction !=70, test_reduction !=90) %>% 
  ggplot(aes(as.factor(test_reduction), mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_crossbar(aes(ymin = lower_CI, ymax = upper_CI, color = as.factor(art_int_rate)), show.legend = T,width = 0.5, position = position_dodge(width = 1), alpha = 0.5) +
  geom_point(aes(color = as.factor(pitc_reduction_year)), shape = 18, size =3, position = position_dodge(width = 1), show.legend = F, color = "white") +
  xlab("PITC reduction (%)") + scale_y_continuous("Additional AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete(labels = c("Reduced ART retention", "Status quo ART retention", "Improved ART retention"),) + 
  scale_color_discrete(labels = c("Reduced ART retention", "Status quo ART retention", "Improved ART retention")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.title = element_blank())



cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                                 art_int_rate = (1-art_int_rate)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif", pitc_reduction_year == 2025) %>% 
  ggplot(aes(test_reduction, art_int_rate, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Female ART retention rate") + scale_y_continuous(trans = "logit") +
  xlab("PITC reduction (%)") + theme_classic()


# additional AIDS related deaths

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),art_int_rate = (1-art_int_rate)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_int_rate, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) + 
  geom_contour(color = "black", binwidth = 0.1) + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Additional \nAIDS-related \ndeaths \n(millions)",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Female ART retention rate") + scale_y_continuous(trans = "logit") +
  xlab("PITC reduction (%)") + theme_classic()


#### condom usage reductions ####

condom_reduction_cumulative <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_reduction_cumulative.csv")
condom_reduction_summary <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_reduction_summary.csv")

unique_future_values <- unique(condom_reduction_summary$future_value)
overall_condom_usage_range <- condom_reduction_summary %>% filter(indicator == "CondomUsage", 
                                                                  test_reduction == 0, pitc_reduction_year == 2025,
                                                                  year == 2035, scenario == "intervention") %>% select(mean)
condom_usage_reduction_labels <- round(overall_condom_usage_range$mean,1)
condom_reduction_cumulative <- condom_reduction_cumulative %>% mutate(overall_condom_usage = case_when(future_value == unique_future_values[1] ~ condom_usage_reduction_labels[1],
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
condom_promotion_cumulative <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_promotion_cumulative.csv")
condom_promotion_summary <- read_csv("/Users/stefan/Documents/HIV_EndGame_SA/results/condom_promotion_summary.csv")
unique_future_values <- unique(condom_promotion_summary$future_value)
overall_condom_usage_range <- condom_promotion_summary %>% filter(indicator == "CondomUsage", 
                                                                  test_reduction == 0, pitc_reduction_year == 2025,
                                                                  year == 2035, scenario == "intervention") %>% select(mean)


unique_future_values <- unique(condom_promotion_summary$future_value)
overall_condom_usage_range <- condom_promotion_summary %>% filter(indicator == "CondomUsage", 
                                                                  test_reduction == 0, pitc_reduction_year == 2025,
                                                                  year == 2035, scenario == "intervention") %>% select(mean)
condom_usage_promotion_labels <- round(overall_condom_usage_range$mean,1)
condom_promotion_cumulative <- condom_promotion_cumulative %>% mutate(overall_condom_usage = case_when(future_value == unique_future_values[1] ~ condom_usage_reduction_labels[1],
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
condom_change_cumulative <- bind_rows(condom_reduction_cumulative, condom_promotion_cumulative)
which(is.na(condom_change_cumulative$overall_condom_usage))
#### correcting incorrectly saved future value ####
unique_task_names <- unique(condom_change_summary$task_name)


for (i in 1:length(unique_task_names)){
  condom_usage_value <- unique(condom_change_summary[which(condom_change_summary$task_name == unique_task_names[i]),]$overall_condom_usage)
  condom_change_cumulative[which(condom_change_cumulative$task_name == unique_task_names[i]),]$overall_condom_usage <- condom_usage_value
}
unique(condom_change_cumulative$overall_condom_usage)
sum(is.na(condom_change_cumulative$overall_condom_usage))
write_csv(condom_change_cumulative, "results/condom_change_cumulative.csv")

condom_change_cumulative %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif",
         pitc_reduction_year == 2025,
         overall_condom_usage == 55 | overall_condom_usage == 33.0 | overall_condom_usage == 20.5,
         test_reduction != 10, test_reduction !=30, test_reduction !=50, test_reduction !=70, test_reduction !=90) %>% 
  ggplot(aes(as.factor(test_reduction), mean, group = as.factor(overall_condom_usage), fill = as.factor(overall_condom_usage))) +
  geom_crossbar(aes(ymin = lower_CI, ymax = upper_CI, color = as.factor(overall_condom_usage)), show.legend = T,width = 0.5, position = position_dodge(width = 1), alpha = 0.5) +
  geom_point(aes(color = as.factor(pitc_reduction_year)), shape = 18, size =3, position = position_dodge(width = 1), show.legend = F, color = "white") +
  xlab("PITC reduction (%)") + scale_y_continuous("Additional HIV infections (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete(labels = c("Reduced condom usage", "Status quo condom usage", "Improved condom usage"),) + 
  scale_color_discrete(labels = c("Reduced condom usage", "Status quo condom usage", "Improved condom usage")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.title = element_blank())

condom_change_cumulative %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif",
         pitc_reduction_year == 2025,
         overall_condom_usage == 55 | overall_condom_usage == 33.0 | overall_condom_usage == 20.5,
         test_reduction != 10, test_reduction !=30, test_reduction !=50, test_reduction !=70, test_reduction !=90) %>% 
  ggplot(aes(as.factor(test_reduction), mean, group = as.factor(overall_condom_usage), fill = as.factor(overall_condom_usage))) +
  geom_crossbar(aes(ymin = lower_CI, ymax = upper_CI, color = as.factor(overall_condom_usage)), show.legend = T,width = 0.5, position = position_dodge(width = 1), alpha = 0.5) +
  geom_point(aes(color = as.factor(pitc_reduction_year)), shape = 18, size =3, position = position_dodge(width = 1), show.legend = F, color = "white") +
  xlab("PITC reduction (%)") + scale_y_continuous("Additional AIDS-related deaths (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete(labels = c("Reduced condom usage", "Status quo condom usage", "Improved condom usage"),) + 
  scale_color_discrete(labels = c("Reduced condom usage", "Status quo condom usage", "Improved condom usage")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.title = element_blank())



condom_change_cumulative %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
         condom_usage_reduction = as.numeric(condom_usage_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewAdultHIV", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, condom_usage_reduction, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log") +
  xlab("PITC reduction (%)") + theme_classic()


cumulative_reduce_condom_usage_with_test_reduction %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
         condom_usage_reduction = as.numeric(condom_usage_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, condom_usage_reduction, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Additional \nAIDS-related \ndeaths \n(millions)",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Overall condom usage (%)") + scale_y_continuous(trans = "log") +
  xlab("PITC reduction (%)") + theme_classic()

#### deaths + condom ####

condom_change_cumulative %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, overall_condom_usage, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) +
  geom_contour(aes(color = "0 additional \ndeaths"),breaks = 0,show.legend = TRUE) + 
  scale_fill_gradient2("Additional \nHIV infection \n(millions)",
                       low = "navyblue",
                       mid = "white",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Overall condom usage") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "Baseline", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() + scale_color_discrete("",type = "black")

condom_change_cumulative %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, overall_condom_usage, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) +
  geom_contour(aes(color = "0 additional \ndeaths"),breaks = 0,show.legend = TRUE) + 
  scale_fill_gradient2("Additional \nHIV infection \n(millions)",
                       low = "navyblue",
                       mid = "white",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Overall condom usage") + scale_y_continuous(trans = "log", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), labels = c("10", "15", "20","25", "30", "Baseline", 35, "40", "45", "50", "55")) +
  xlab("Testing reduction (%)") + theme_classic() + scale_color_discrete("",type = "black")
