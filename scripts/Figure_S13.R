library(egg)
baseline_1000 <- read_csv("~/Documents/clean_results/baseline_1000.csv")
prop_new_inf_kp <- baseline_1000 %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(pitc_reduction_year == 2025, 
         indicator %in% c("Prop_NewHIV_FSW", "Prop_NewHIV_client", "Prop_NewHIV_MSM"),
         year >= 2020, 
         test_reduction %in% c(0), 
         scenario == "baseline"
  ) %>%  
  ggplot(aes(year, mean, fill = indicator, color = indicator)) +
  geom_bar(aes(), stat = "identity") +
  scale_x_continuous("", expand = c(0, 1), breaks = seq(2020, 2100, 20)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "right") +
  scale_y_continuous("Proportion of total\nnew HIV infections (%)",expand = c(0, 0), labels =(function(l) {round(l*1e2,2)}), limits = c(0,0.31)) +
  scale_fill_manual("Key population", aesthetics = c("colour", "fill"), breaks = c("Prop_NewHIV_client", "Prop_NewHIV_FSW", "Prop_NewHIV_MSM"), values = c("Prop_NewHIV_client" = "#4daf4a", "Prop_NewHIV_FSW" = "#e41a1c", "Prop_NewHIV_MSM" = "#377eb8"), 
                    labels= c("Clients of FSW", "FSW", "MSM")) + 
  ggtitle("Proportion of new HIV infections\namongst key populations")

incidence_kp <- baseline_1000 %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator %in% c("HIVinc15to49","HIVincFSW","HIVincMSM"),
         year >= 2020, 
         test_reduction %in% c(0, 25, 50, 75, 100)) %>% 
  ggplot(aes(year, mean, group = indicator, fill = indicator)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = indicator), alpha = 0.10, show.legend = F) +
  geom_line(aes(colour = indicator), show.legend = T) +
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}),expand = c(0, 0)) +
  scale_fill_brewer("Population", labels = c("Adults (15-49 years)", "FSW", "MSM"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV incidence \n(per 1000; Key populations)")

prev_kp <- baseline_1000 %>% 
  filter(scenario == "baseline", 
         pitc_reduction_year == 2025, 
         indicator %in% c("Prev15to49","PrevFSW", "MSMprev18plus"),
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
  scale_y_continuous("HIV prevalence (%)", labels =(function(l) {round(l*1e2,1)}), expand = c(0,0)) +
  scale_fill_brewer("Population", labels = c("Adults (15-49 years)", "FSW", "MSM"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("HIV prevalence\n(Key populations)")

art_coverage_kp <- baseline_1000 %>% 
  mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator %in% c("ARTcoverageAdult","ARTcoverageMSM","ARTcoverageFSW"),
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
  scale_y_continuous("ART coverage (%) ", labels =(function(l) {round(l*1e2,1)}), limits = c(0,1), expand = c(0,0)) +
  scale_fill_brewer("Key population", labels = c("Adult (15-49 years)","FSW", "MSM"), aesthetics = c("colour", "fill"), palette = "Set1") + 
  ggtitle("ART coverage\n(Key populations)")

key_pop_fig <- ggarrange(incidence_kp, art_coverage_kp, prev_kp, prop_new_inf_kp, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), label.args = list(gp = grid::gpar(fontface = "bold", fontsize = 14)))

ggsave(filename = "figures/fig_S13.png", plot = key_pop_fig, device = "png", width = 23, height = 20, units = "cm")
ggsave(filename = "figures/fig_S13.pdf", plot = key_pop_fig, device = "pdf", width = 23, height = 10, units = "cm")

