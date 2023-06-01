#### End of AIDS incidence and infections ####

# load libraries 

library(dplyr)
library(tidyr)
library(ggplot2)

# read in csv 
baseline_summary <- read_csv("results/inc_inf_by_population.csv")

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
                  "NewHIV50F", "HIVinc50F")

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

# vector of the infection only output names 
infection_outputs <- output_names[!(output_names %in% incidence_outputs)]

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
  filter(indicator %in% infection_outputs,
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

