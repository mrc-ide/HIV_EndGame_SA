#### cumulative heatmaps ####

art_change_values <- seq(0, 8, 2)
filepaths <- paste0("results/cumulative_decrease_art_retention_with_test_reduction", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
cumulative_decrease_art_retention_with_test_reduction <- bind_rows(temp, .id = "art_int_reduction")

filepaths <- paste0("results/cumulative_increase_art_retention_with_test_reduction", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
cumulative_increase_art_retention_with_test_reduction <- bind_rows(temp, .id = "art_int_improvement")

cumulative_decrease_art_retention_with_test_reduction <- 
  cumulative_decrease_art_retention_with_test_reduction %>% 
  mutate(art_int_rate = (0.22*((1+(as.numeric(art_int_reduction)/100))**10)))

cumulative_increase_art_retention_with_test_reduction <- 
  cumulative_increase_art_retention_with_test_reduction %>% 
  mutate(art_int_rate = (0.22*((1-(as.numeric(art_int_improvement)/100))**10)))

cumulative_art_change <- bind_rows(cumulative_decrease_art_retention_with_test_reduction, 
                                   cumulative_increase_art_retention_with_test_reduction)

cumulative_art_change <- cumulative_art_change %>% 
  select(-c(art_int_improvement, art_int_reduction))

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
         art_int_rate == 0.9 | art_int_rate == 0.78 | art_int_rate == 0.53,  
         test_reduction != 10, test_reduction !=30, test_reduction !=50, test_reduction !=70, test_reduction !=90) %>% 
  ggplot(aes(as.factor(test_reduction), mean, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_crossbar(aes(ymin = lower_CI, ymax = upper_CI, color = as.factor(art_int_rate)), show.legend = T,width = 0.5, position = position_dodge(width = 1), alpha = 0.5) +
  geom_point(aes(color = as.factor(pitc_reduction_year)), shape = 18, size =3, position = position_dodge(width = 1), show.legend = F, color = "white") +
  xlab("PITC reduction (%)") + scale_y_continuous("Additional HIV infections (millions)", labels = (function(l) {round(l/1e6,1)})) +
  scale_fill_discrete(labels = c("Reduced ART retention", "Status quo ART retention", "Improved ART retention"),) + 
  scale_color_discrete(labels = c("Reduced ART retention", "Status quo ART retention", "Improved ART retention")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  theme(legend.title = element_blank())

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
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_int_rate, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Female ART interruption rate") + scale_y_continuous(trans = "logit") +
  xlab("PITC reduction (%)") + theme_classic()


# additional AIDS related deaths

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
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
  ylab("Female ART interruption rate") + scale_y_continuous(trans = "logit") +
  xlab("PITC reduction (%)") + theme_classic()


#### condom usage reductions ####

condom_reduction_cumulative <- read_csv("results/condom_reduction_cumulative.csv")

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
                                                                                                 future_value == unique_future_values[8] ~ condom_usage_reduction_labels[8]))
condom_promotion_cumulative <- read_csv("results/condom_promotion_cumulative.csv")
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
                                                                                                       future_value == unique_future_values[8] ~ condom_usage_reduction_labels[8]))
condom_change_cumulative <- bind_rows(condom_reduction_cumulative, condom_promotion_cumulative)
#### correcting incorrectly saved future value ####
unique_task_names <- unique(condom_change_summary$task_name)


for (i in 1:length(unique_task_names)){
  condom_usage_value <- unique(condom_change_summary[which(condom_change_summary$task_name == unique_task_names[i]),]$overall_condom_usage)
  condom_change_cumulative[which(condom_change_cumulative$task_name == unique_task_names[i]),]$overall_condom_usage <- condom_usage_value
}
unique(condom_change_cumulative$overall_condom_usage)

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



condom_reduction_cumulative %>% 
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


#### normalised y-axis ####
cumulative_art_change <- cumulative_art_change %>% mutate(art_int_rate_norm = art_int_rate - 0.22)
range(cumulative_art_change$art_int_rate_norm)

# additional HIV infections #
cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewAdultHIV", 
         scenario == "absolute_dif") %>% 
  ggplot(aes(test_reduction, art_int_rate_norm, z = mean/10**6)) + 
  geom_raster(aes(fill = mean/10**6), interpolate = TRUE) + 
  geom_contour(color = "black") + 
  geom_text_contour(skip = 0, stroke = 0.1, stroke.colour = "white") +
  scale_fill_gradient2("Additional \nHIV \ninfections \n(millions)",
                       low = "blue",
                       mid = "#FFFFCC",
                       high = "#FF0000", 
                       midpoint = 0) +
  ylab("Change in female ART interruption rate") + scale_y_continuous(trans = "exp") +
  xlab("PITC reduction (%)") + theme_classic()

