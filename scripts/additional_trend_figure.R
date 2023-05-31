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
         future_value == 7, future_variability == "condom_promotion") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "HIVinc15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "HIVinc15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_promotion"),
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
         future_value == 7, future_variability == "condom_promotion") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "Prev15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "Prev15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_promotion"),
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
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +  xlab("") +
  geom_line(aes(color = test_reduction)) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "ARTcoverageAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "ARTcoverageAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_promotion"),
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
         future_value == 7, future_variability == "condom_promotion") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "TestsPerAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_promotion"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "TestsPerAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_promotion"),
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
         future_value == 7 , future_variability == "condom_reduction") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "HIVinc15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "HIVinc15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_reduction"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
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
         future_value == 7, future_variability == "condom_reduction") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "Prev15to49",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "Prev15to49",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_reduction"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
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
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +  xlab("") +
  geom_line(aes(color = test_reduction)) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "ARTcoverageAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "ARTcoverageAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_reduction"),
              aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
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
         future_value == 7, future_variability == "condom_reduction") %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_line(data = filter(condom_change_summary, 
                          scenario == "baseline",
                          pitc_reduction_year == 2025,
                          indicator == "TestsPerAdult",
                          year > 2020,
                          test_reduction %in% c(0, 25, 50, 75, 100),
                          future_value == 7 , future_variability == "condom_reduction"),
            aes(x=year, y=mean, colour = scenario)) +
  geom_ribbon(data = filter(condom_change_summary, 
                            scenario == "baseline",
                            pitc_reduction_year == 2025,
                            indicator == "TestsPerAdult",
                            year > 2020,
                            test_reduction %in% c(0, 25, 50, 75, 100),
                            future_value == 7 , future_variability == "condom_reduction"),
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

