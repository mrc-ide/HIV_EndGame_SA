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
cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator == "NewAdultHIV", 
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

condom_reduction_values <- seq(0, 14, 2)

filepaths <- paste0("results/cumulative_reduce_condom_usage_with_test_reduction", condom_reduction_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- condom_usage_reduction_labels
cumulative_reduce_condom_usage_with_test_reduction <- bind_rows(temp, .id = "condom_usage_reduction")


cumulative_reduce_condom_usage_with_test_reduction %>% 
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

