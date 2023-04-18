#### number of years to accumalte HIV infections ####

years_to_accumulate <- test_reduction_only %>% filter(scenario == "baseline", 
                               pitc_reduction_year == 2025, 
                               test_reduction == 0, 
                               indicator == "NewAdultHIV", 
                               year <= 2025 & year >=1985) %>% 
  mutate(year_to_accumulate = year - 1984) %>% 
  select(-c(scenario, indicator, pitc_reduction_year, test_reduction,future_variability,future_value, lower_CI, upper_CI))

years_to_accumulate <- years_to_accumulate %>% mutate(cum_infections = cumsum(mean)) %>% 
  mutate(mean_infections = mean) %>% select(-mean)


years_to_accumulate %>% ggplot(aes(year_to_accumulate,cum_infections)) + geom_point() + 
  theme_classic() +
  scale_x_continuous("Years to accumulate") + scale_y_continuous("Cumulative HIV infections", labels = function(l) round(l/10**6,2))

years_to_accumulate %>% ggplot(aes(cum_infections,year_to_accumulate)) + geom_point() + 
  theme_classic() +
  scale_y_continuous("Years to accumulate") + scale_x_continuous("Cumulative HIV infections", labels = function(l) round(l/10**6,2))


fit1 <- lm(data = years_to_accumulate, formula = new_year ~ cum_infections)
summary(fit1)
plot(fit1)

years_to_accumulate %>% ggplot(aes(year_to_accumulate,cum_infections)) + geom_point() + 
  theme_classic() +
  scale_x_continuous("Years to accumulate") + 
  scale_y_continuous("Cumulative HIV infections", labels = function(l) round(l/10**6,2)) + 
  stat_function(fun = function(year_to_accumulate){-245943.66 - 6780.14*year_to_accumulate + 21596.17*(year_to_accumulate**2) - 356.25*(year_to_accumulate**3)})

(cum_infections + 245943.66)/-6780.14 = year_to_accumulate - 3.18521*(year_to_accumulate**2) + 0.05254316*(year_to_accumulate**3)

fit2 <- lm(data = years_to_accumulate, formula = cum_infections ~ year_to_accumulate + I(year_to_accumulate**2) + I(year_to_accumulate**3))
summary(fit2)
plot(fit2)

fit3 <- lm(data = years_to_accumulate, formula = year_to_accumulate ~ cum_infections + I(cum_infections**2) + I(cum_infections**3))
summary(fit3)

years_to_accumulate %>% ggplot(aes(cum_infections,year_to_accumulate)) + geom_point() + 
  theme_classic() +
  scale_y_continuous("Years to accumulate") + 
  scale_x_continuous("Cumulative HIV infections(millions)", labels = function(l) round(l/10**6,2)) +
  stat_function(fun = function(cum_infections){3.568 + 5.082e-06*cum_infections - 5.776e-13*(cum_infections**2) + 3.611e-20*(cum_infections**3)}) 

years_to_accumulate <- years_to_accumulate %>% mutate(predicted_years = 3.568 + 5.082e-06*cum_infections - 5.776e-13*(cum_infections**2) + 3.611e-20*(cum_infections**3))

cumulative_art_change %>% mutate(pitc_reduction_year = as.factor(pitc_reduction_year),
                            art_int_rate = round((1-art_int_rate),2)) %>% 
  filter(indicator == "NewAdultHIV", 
         scenario == "intervention",
         pitc_reduction_year == 2025,
  ) %>% 
  mutate(years_to_accumulate = 3.568 + 5.082e-06*mean - 5.776e-13*(mean**2) + 3.611e-20*(mean**3)) %>% 
  ggplot(aes(test_reduction, years_to_accumulate, group = as.factor(art_int_rate), fill = as.factor(art_int_rate))) +
  geom_line(aes(color = as.factor(art_int_rate)),show.legend = T) +
  xlab("Testing reduction (%)") + scale_y_continuous("Years from 1985 to accumulate same number of HIV infections") +
  scale_fill_discrete("Female ART \nretention rate", labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  scale_color_discrete("Female ART \nretention rate",labels = c("0.53", "0.61", "0.67", "0.73", "0.78 (Baseline)", "0.82", "0.85", "0.88", "0.90")) + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  geom_hline(aes(yintercept = 16.49350),lty = "dotted")


