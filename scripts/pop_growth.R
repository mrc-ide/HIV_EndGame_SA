baseline_1000 %>% 
  filter(scenario == "baseline", 
         pitc_reduction_year == 2025, 
         indicator %in% c("FemalesOver15","MalesOver15"),
         year >= 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = indicator, fill = indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = indicator), show.legend = T) +
  scale_x_continuous("",expand = c(0, 0)) + 
  theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Number of adults (millions)", labels =(function(l) {round(l/1e6,1)}), expand = c(0,0)) +
  scale_fill_brewer("Population", labels = c("Female (15+ years)", "Male (15+ years)"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("Population size")

ggsave(filename = "figures/pop_growth.png",  device = "png", width = 23, height = 20, units = "cm")
ggsave(filename = "figures/pop_growth.pdf", device = "pdf", width = 23, height = 10, units = "cm")
