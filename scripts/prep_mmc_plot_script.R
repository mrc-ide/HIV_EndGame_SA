library(ggh4x)

all_scenarios <- read_csv("results/prep_vmmc_scenarios.csv")
all_scenarios <- all_scenarios %>% 
  mutate(modeled_scenario = factor(modeled_scenario, 
                                   levels = c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                              "No PrEP + No VMMC + Condom usage 28%",
                                              "No PrEP + No VMMC + Condom usage 23%",
                                              "95-95-95", "95-95-95 + No PrEP + No VMMC")))


nms <- c("HIVinc15to49", "HIVincFSW", "HIVincMSM",
         "Prev15to49", "PrevFSW", "MSMprev18plus",
         "NewHIV15to49", "NewHIVinFSW", "NewHIVinMSM",
         "ARTcoverageAdult","ARTcoverageFSW", "ARTcoverageMSM",
         "PrEPcoverageAll", "PrEPcoverageFSW", "PrEPcoverageMSM",
         "CondomUsage", "FSWcondomUse", "CondomUse15to49MSM",
         "Circumcised15to49", "CircumcisedFSW", "CircumcisedMSM15to49")

ylabs <- c("HIV incidence per 1000", "HIV prevalence (%)",
           "New HIV infections (000s)", "ART coverage (%)", "PrEP coverage (%)",
           "Protected sex acts (%)", "Circumcision coverage (%)")
groups <- rep(c("All adults", "FSW", "MSM"), length(nms) / 3)
names(groups) <- nms
types <- rep(ylabs, each = 3)
names(types) <- nms

df <- all_scenarios %>% 
  dplyr::filter(indicator %in% nms) %>% 
  tidyr::pivot_longer(cols = c(mean, upper_CI, lower_CI)) %>% 
  tidyr::pivot_wider(names_from = indicator) %>% 
  mutate(CircumcisedFSW = 0) %>% 
  dplyr::mutate(across(contains("HIVinc"), ~ . * 1e3),
                across(contains("Prev"), ~ . * 100),
                across(contains("NewHIV"), ~. / 1000),
                across(contains("coverage"), ~ . * 100),
                across(contains("ondom"),  ~ . * 100),
                across(contains("Circumcised"),  ~ . * 100)
                ) %>% 
  tidyr::pivot_longer(cols = ARTcoverageAdult:CircumcisedFSW, names_to = "indicator") %>% 
  tidyr::pivot_wider() %>% 
  dplyr::mutate(type = types[indicator],
                group = groups[indicator])
# full grid 
df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
                                        )) %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", independent = "all") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", labels =(function(l) {if_else(l<=2,round(l,1),round(l,0))})) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1, 
        legend.title = element_blank(),
        plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "top",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.x = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_text(size = 13, face = "bold"),
        # strip.background = element_blank()
        ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

ggsave(filename = "unaids_figures/all_populations_facet.png", device = "png", units = "cm", height = 32, width = 24)


#### row by row ####

#### incidence ####

incidence <- df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "HIV incidence\nper 1000") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_text(size = 13, face = "bold"),
        strip.background.y = element_blank()
        # strip.background = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

incidence

#### Prevalence ####

prevalence <- df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "HIV prevalence\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

prevalence

#### New HIV infections ####

new_infections <- df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "New HIV\ninfections\n(thousands)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

new_infections

#### ART coverage ####

art_coverage <- df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "ART coverage\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", limits = c(50,100)) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

art_coverage

#### Condom usage ####

condom_use <- df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "Condom use\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", limits = c(0,100)) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

condom_use

#### PrEP coverage ####

prep_coverage <- df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "PrEP coverage\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", labels =(function(l) {if_else(l<=2,round(l,1),round(l,0))})) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        plot.margin = margin(l = 10),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

prep_coverage

#### Circumcision coverage ####

circumcision_coverage <- df %>% 
  filter(year >= 2020, 
         !modeled_scenario %in% c("95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "Circumcision\ncoverage\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", limits = c(0,100)) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1, 
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

circumcision_coverage

#### combined rows ####
library(gridExtra)
library(cowplot)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
inc_leg <- incidence + theme(legend.position = "top") 
legend <- get_legend(inc_leg)
trends_combined <- plot_grid(incidence, prevalence, new_infections, art_coverage, 
          condom_use, prep_coverage, circumcision_coverage, rel_widths = c(1,1,1,1,1,1,1), rel_heights = c(1.6,1.5,1.5,1.5,1.5,1.5,1.5),
          ncol = 1)
plot_grid(legend,trends_combined, ncol = 1, rel_widths = c(0.1, 2), rel_heights = c(0.1, 2))
ggsave(filename = "unaids_figures/trends_combined.png", device = "png", units = "cm", height = 28, width = 22)

#### 95-95-95 scenarios ####

#### incidence ####

incidence <- df %>% 
  filter(year >= 2020, 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "HIV incidence\nper 1000") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_text(size = 13, face = "bold"),
        strip.background.y = element_blank()
        # strip.background = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC"), palette = "Set1")

incidence

#### Prevalence ####

prevalence <- df %>% 
  filter(year >= 2020, 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "HIV prevalence\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

prevalence

#### New HIV infections ####

new_infections <- df %>% 
  filter(year >= 2020, 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "New HIV\ninfections\n(thousands)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("") +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

new_infections

#### ART coverage ####

art_coverage <- df %>% 
  filter(year >= 2020, 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "ART coverage\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", limits = c(50,100)) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

art_coverage

#### Condom usage ####

condom_use <- df %>% 
  filter(year >= 2020, 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "Condom use\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", limits = c(0,100)) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

condom_use

#### PrEP coverage ####

prep_coverage <- df %>% 
  filter(year >= 2020, 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "PrEP coverage\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", labels =(function(l) {if_else(l<=2,round(l,1),round(l,0))})) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, colour = "white"),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1,
        legend.title = element_blank(),
        plot.margin = margin(l = 10),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

prep_coverage

#### Circumcision coverage ####

circumcision_coverage <- df %>% 
  filter(year >= 2020, 
         modeled_scenario %in% c("Status quo", "95-95-95", "95-95-95 + No PrEP + No VMMC")) %>% 
  mutate(type = factor(type, levels = c("HIV incidence per 1000", "HIV prevalence (%)", 
                                        "New HIV infections (000s)", "ART coverage (%)",
                                        "Protected sex acts (%)", "PrEP coverage (%)",
                                        "Circumcision coverage (%)"),
                       labels = c("HIV incidence\nper 1000", "HIV prevalence\n(%)", 
                                  "New HIV\ninfections\n(thousands)", "ART coverage\n(%)",
                                  "Condom use\n(%)", "PrEP coverage\n(%)",
                                  "Circumcision\ncoverage\n(%)"
                       )
  )) %>% 
  filter(type == "Circumcision\ncoverage\n(%)") %>% 
  ggplot(aes(x = year, group = modeled_scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = modeled_scenario), alpha = 0.1) +
  geom_line(aes(y = mean, col = modeled_scenario)) +
  ggh4x::facet_grid2(vars(type), vars(group), scales = "free", space = "free_x", independent = "y") + theme_bw() +
  scale_x_continuous("", expand = c(0,0), breaks = seq(2025, 2100, 25)) +
  scale_y_continuous("", limits = c(0,100)) +
  theme_classic() + 
  expand_limits(x = 2020, y = 0) + 
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # aspect.ratio=1, 
        legend.title = element_blank(),
        # plot.margin = margin(r = 1.5, b = 2, unit = "cm"),
        legend.position = "none",
        # legend.position = c(0.58,-0.05), 
        legend.direction = "horizontal", 
        strip.placement = "outside",
        strip.text.y = element_text(size = 13, face = "bold", angle = 0),
        strip.text.x = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_blank()
  ) +
  scale_fill_brewer("",aesthetics = c("colour", "fill"), labels = c("Status quo", "No PrEP", "No PrEP + No VMMC", "No PrEP + No VMMC +\nCondom usage 28%","No PrEP + No VMMC +\nCondom usage 23%"), palette = "Set1")

circumcision_coverage

#### combined rows ####
library(gridExtra)
library(cowplot)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
inc_leg <- incidence + theme(legend.position = "top") 
legend <- get_legend(inc_leg)
trends_combined_art95 <- plot_grid(incidence, prevalence, new_infections, art_coverage, 
                             condom_use, prep_coverage, circumcision_coverage, rel_widths = c(1,1,1,1,1,1,1), rel_heights = c(1.6,1.5,1.5,1.5,1.5,1.5,1.5),
                             ncol = 1)
plot_grid(legend,trends_combined_art95, ncol = 1, rel_widths = c(0.1, 2), rel_heights = c(0.1, 2))
ggsave(filename = "unaids_figures/trends_combined_art95.png", device = "png", units = "cm", height = 28, width = 22)
