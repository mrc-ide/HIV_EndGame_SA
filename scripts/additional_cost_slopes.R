#### calculate slope of no additional infections #####

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

cumulative_art_change <- read_csv("THEMBISAv18/results/art_change_cumulative.csv")
condom_change_cumulative <- read_csv("results/condom_change_cumulative.csv")

#### art change infections interpolated ####

temp4a <- cumulative_art_change %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  art_ret_rate <- filter(temp4a, test_reduction == i)$art_ret_rate
  mean <- filter(temp4a, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = art_ret_rate, 
                                    y = mean, 
                                      xout = seq(min(art_ret_values), max(art_ret_values), length.out = 100000))) %>% 
    rename(art_ret_rate = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4a <- bind_rows(temp4a, interpolated) 
}
temp4a <- temp4a[which(is.na(temp4a$future_value)),]

temp4a %>% 
  filter(mean < 100, mean > -100) %>% 
  ggplot(aes(test_reduction, art_ret_rate)) +
  stat_smooth(method = "lm") + geom_abline(slope = 0.001097307, intercept = 0.8514000)

lm_temp4a <- temp4a %>% 
  filter(mean < 100, mean > -100)

fig4a + 
  geom_abline(slope = 0.001097307, intercept = 0.8514000, color = "blue") + 
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity",limits = c(0.8, 0.9671146)) +
  annotate(geom = "text", label = "slope = 0.1097307", x = 75, y = 0.90)
  
#### art change interpolated deaths ####

temp4b <- cumulative_art_change %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  art_ret_rate <- filter(temp4b, test_reduction == i)$art_ret_rate
  mean <- filter(temp4b, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = art_ret_rate, 
                                    y = mean, 
                                    xout = seq(min(art_ret_values), max(art_ret_values), length.out = 100000))) %>% 
    rename(art_ret_rate = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4b <- bind_rows(temp4b, interpolated) 
}

temp4b <- temp4b[which(is.na(temp4b$future_value)),]

temp4b %>% 
  filter(mean < 100, mean > -100) %>% 
  ggplot(aes(test_reduction, art_ret_rate)) +
  stat_smooth(method = "lm") + geom_abline(slope = 0.001822887, intercept = 0.8541)



fig4b + 
  geom_abline(slope = 0.001822887, intercept = 0.8514000, colour = "blue") + 
  scale_y_continuous(name ="ART retention (%)",
                     breaks = c(0.1, 0.2, 0.3, 0.40, 0.50, 0.60, 0.70, 0.80, 0.8514000, 0.90),
                     labels = c("10", "20", "30", "40","50", "60", "70", "80", "", "90"),
                     trans = "identity",limits = c(0.8, 0.9671146)) + 
  annotate(geom = "text", label = "slope = 0.1822887", x = 60, y = 0.90)



temp4c <- condom_change_cumulative %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp4c, test_reduction == i)$overall_condom_usage
  mean <- filter(temp4c, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean, 
                                    xout = seq(min(condom_usage_labels), max(condom_usage_labels), length.out = 100000))) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4c <- bind_rows(temp4c, interpolated) 
}

temp4c <- temp4c[which(is.na(temp4c$future_value)),]

temp4c %>% 
  filter(mean < 100, mean > -100) %>% 
  ggplot(aes(test_reduction, overall_condom_usage)) +
  stat_smooth(method = "lm") + geom_abline(slope = 0.1365391, intercept = 33)

lm_temp4c <- temp4c %>% 
  filter(mean < 100, mean > -100)

fig4c + geom_abline(slope = 0.1365391, intercept = 33, , color = "blue") + 
  scale_y_continuous(trans = "identity", breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), 
                     labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55"), 
                     limits = c(30, 55)) + 
  annotate(geom = "text", label = "slope = 0.1365391", x = 60, y = 35)


temp4d <- condom_change_cumulative %>% 
  filter(indicator == "TotalAIDSdeathsadult", 
         scenario == "absolute_dif")

for (i in seq(0, 100, 5)){
  overall_condom_usage <- filter(temp4d, test_reduction == i)$overall_condom_usage
  mean <- filter(temp4d, test_reduction == i)$mean
  interpolated <- data.frame(approx(x = overall_condom_usage, 
                                    y = mean, 
                                    xout = seq(min(condom_usage_labels), max(condom_usage_labels), length.out = 100000))) %>% 
    rename(overall_condom_usage = x) %>% 
    rename(mean = y) %>% 
    mutate(pitc_reduction_year = 2025, 
           test_reduction = i)
  temp4d <- bind_rows(temp4d, interpolated) 
}

temp4d <- temp4d[which(is.na(temp4d$future_value)),]

temp4d %>% 
  filter(mean < 100, mean > -100) %>% 
  ggplot(aes(test_reduction, overall_condom_usage)) +
  stat_smooth(method = "lm") + geom_abline(slope = 0.396364, intercept = 33)                     

lm_temp4d <- temp4d %>% 
  filter(mean < 100, mean > -100)

fig4d + geom_abline(slope = 0.396364, intercept = 33, color = "blue") + 
  scale_y_continuous(trans = "identity", 
                     breaks = c(10, 15, 20, 25, 30, 33, 35, 40, 45, 50, 55), 
                     labels = c("10", "15", "20","25", "30", "", 35, "40", "45", "50", "55"),
                     limits =c(30, 57.3)) +
  annotate(geom = "text", label = "slope = 0.396364", x = 60, y = 40)

