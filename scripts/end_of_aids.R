#### End of AIDS incidence and infections ####

# load libraries 
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

# run model
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")
system("./thembisa")

# all outputs included
output_names <- c("NewHIVinFSW", "HIVincFSW",
                  "NewHIVclients", "HIVincClients",
                  "NewHIVinMSM", "HIVincMSM",
                  "NewHIVU15", "HIVinc0to14",
                  "NewHIV15to49", "HIVinc15to49",
                  "NewHIV15to49M", "HIVinc15to49M",
                  "NewHIV15to49F", "HIVinc15to49F",
                  "NewHIV15to24", "HIVinc15to24",
                  "NewHIV15to24M", "HIVinc15to24M",
                  "NewHIV15to24F", "HIVinc15to24F",
                  "NewHIV25to49", "HIVinc25to49",
                  "NewHIV25to49M", "HIVinc25to49M",
                  "NewHIV25to49F", "HIVinc25to49F",
                  "NewHIV50", "HIVinc50",
                  "NewHIV50M", "HIVinc50M",
                  "NewHIV50F", "HIVinc50F", 
                  "TotalNewHIV")

# vector of the infection only output names 
infection_outputs <- c("NewHIVinFSW",
                       "NewHIVclients",
                       "NewHIVinMSM",
                       "NewHIVU15",
                       "NewHIV15to49",
                       "NewHIV15to49M",
                       "NewHIV15to49F",
                       "NewHIV15to24",
                       "NewHIV15to24M",
                       "NewHIV15to24F",
                       "NewHIV25to49",
                       "NewHIV25to49M",
                       "NewHIV25to49F",
                       "NewHIV50",
                       "NewHIV50M",
                       "NewHIV50F")

# vector of incidence output names 

incidence_outputs <- c("HIVincFSW",
                       "HIVincClients",
                       "HIVincMSM",
                       "HIVinc0to14",
                       "HIVinc15to49",
                       "HIVinc15to49M",
                       "HIVinc15to49F",
                       "HIVinc15to24",
                       "HIVinc15to24M",
                       "HIVinc15to24F",
                       "HIVinc25to49",
                       "HIVinc25to49M",
                       "HIVinc25to49F",
                       "HIVinc50",
                       "HIVinc50M",
                       "HIVinc50F")

# read raw results from outputs
baseline <- read_thembisa_scenario(output_names)
write_csv(baseline, "results/raw_baseline")

# calculate infections as proportion of total infections 

baseline <- baseline %>%
  pivot_wider(names_from = indicator) %>%
  mutate(Prop_Inf_FSW = (NewHIVinFSW/TotalNewHIV)) %>%
  mutate(Prop_Inf_MSM = (NewHIVinMSM/TotalNewHIV)) %>%
  mutate(Prop_Inf_Clients = (NewHIVclients/TotalNewHIV)) %>%
  mutate(Prop_Inf_0to14 = (NewHIVU15/TotalNewHIV)) %>%
  mutate(Prop_Inf_15to24 = (NewHIV15to24/TotalNewHIV)) %>%
  mutate(Prop_Inf_15to24M = (NewHIV15to24M/TotalNewHIV)) %>%
  mutate(Prop_Inf_15to24F = (NewHIV15to24F/TotalNewHIV)) %>%
  mutate(Prop_Inf_25to49 = (NewHIV25to49/TotalNewHIV)) %>%
  mutate(Prop_Inf_25to49M = (NewHIV25to49M/TotalNewHIV)) %>%
  mutate(Prop_Inf_25to49F = (NewHIV25to49F/TotalNewHIV)) %>%
  mutate(Prop_Inf_50 = (NewHIV50/TotalNewHIV)) %>%
  mutate(Prop_Inf_50M = (NewHIV50M/TotalNewHIV)) %>%
  mutate(Prop_Inf_50F = (NewHIV50F/TotalNewHIV)) %>%
  mutate(Prop_Inf_15to49 = (NewHIV15to49/TotalNewHIV)) %>%
  mutate(Prop_Inf_15to49M = (NewHIV15to49M/TotalNewHIV)) %>%
  mutate(Prop_Inf_15to49F = (NewHIV15to49F/TotalNewHIV)) %>%
  pivot_longer(-(year:parameter_set), names_to = "indicator")

# Distribution of infections outputs

proportion_total_infections <- 
  c("Prop_Inf_FSW",
  "Prop_Inf_MSM",
  "Prop_Inf_Clients",
  "Prop_Inf_0to14",
  "Prop_Inf_15to24",
  "Prop_Inf_15to24M",
  "Prop_Inf_15to24F",
  "Prop_Inf_25to49",
  "Prop_Inf_25to49M",
  "Prop_Inf_25to49F",
  "Prop_Inf_50",
  "Prop_Inf_50M",
  "Prop_Inf_50F",
  "Prop_Inf_15to49",
  "Prop_Inf_15to49M",
  "Prop_Inf_15to49F")

# calculate summary statistics
baseline_summary <- as.data.frame(baseline) %>% 
  dplyr::group_by(year, indicator) %>%
  dplyr::summarise(mean = mean(value), 
                   upper_CI = quantile(value, probs = 0.975, na.rm = TRUE),
                   lower_CI = quantile(value, probs = 0.025, na.rm = TRUE))

# save summary csv
write_csv(baseline_summary, "results/inc_inf_by_population.csv")

# function to plot incidences quickly 
plot_incidence <- function(output_name) {
  baseline_summary %>% 
    filter(indicator == output_name,
           year > 2020) %>% 
    ggplot(aes(year, mean)) +
    geom_ribbon(aes(ymin = lower_CI, 
                    ymax = upper_CI), 
                alpha = 0.25) +
    geom_line(aes()) +
    xlab("") +
    expand_limits(y=0) + theme_classic() + 
    theme(axis.text = element_text(size = 12), 
          axis.title.y = element_text(size = 12), 
          axis.title.x = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_y_continuous("HIV incidence per 1000", 
                       labels =(function(l) {round(l*1e3,1)})) +
    ggtitle(paste0("Incidence: ",
                   strsplit(output_name, split = "HIVinc", fixed = TRUE)[[1]][2], 
                   collapse = " "))
}

# function to plot the infections over time
plot_infections <- function(output_name) {
  baseline_summary %>% 
    filter(indicator == output_name,
           year > 2020) %>% 
    ggplot(aes(year, mean)) +
    geom_ribbon(aes(ymin = lower_CI, 
                    ymax = upper_CI), 
                alpha = 0.25) +
    geom_line(aes()) +
    xlab("") +
    expand_limits(y=0) + theme_classic() + 
    theme(axis.text = element_text(size = 12), 
          axis.title.y = element_text(size = 12), 
          axis.title.x = element_text(size = 12),
          legend.text = element_text(size = 12)) +
    scale_y_continuous("New HIV infections") +
    ggtitle(paste0("New infections:",
                   strsplit(output_name, split = "NewHIV", fixed = TRUE)[[1]][2],
                   collapse = " "))
}


# create plots for each individual incidence and infection outputs
for (i in 1:length(incidence_outputs)){
  print(plot_incidence(output_name = incidence_outputs[i]))
  Sys.sleep(2)
  print(plot_infections(output_name = infection_outputs[i]))
  Sys.sleep(2)
}

# facet_wrap of incidences 

baseline_summary %>% 
  filter(indicator %in% "TotalNewHIV",
         year > 2020) %>% 
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, 
                  ymax = upper_CI), 
              alpha = 0.25) +
  geom_line(aes()) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("New HIV infections") +
  facet_wrap(~indicator, scales = "free_y")

baseline_summary %>% 
  filter(indicator %in% incidence_outputs,
         year > 2020) %>% 
  ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, 
                  ymax = upper_CI), 
              alpha = 0.25) +
  geom_line(aes()) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per 1000", 
                     labels =(function(l) {round(l*1e3,1)})) +
  facet_wrap(~indicator, scales = "free_y")

baseline_summary%>% 
  filter(indicator %in% proportion_total_infections[1:3],
         year > 2020) %>% 
  ggplot(aes(year, mean, group = indicator, fill = indicator)) +
  geom_ribbon(aes(ymin = lower_CI, 
                  ymax = upper_CI), 
              alpha = 0.25) +
  geom_line(aes(color = indicator)) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("Proportion of total infections")

baseline_summary%>% 
  filter(indicator %in% proportion_total_infections[c(4,5,8,11)],
         year > 2020) %>% 
  ggplot(aes(year, mean, group = indicator, fill = indicator)) +
  geom_ribbon(aes(ymin = lower_CI, 
                  ymax = upper_CI), 
              alpha = 0.25) +
  geom_line(aes(color = indicator)) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("Proportion of total infections")
