#### Single age outputs ####
library(tidyverse)
library(stringr)
library(ggplot2)
raw_data <- read.delim("OutputByAge.txt", header = FALSE) # read txt file

idx_blanks <- which(rowSums(raw_data, na.rm = TRUE) == 0) # remove separator rows
data <- as.matrix(raw_data)[-idx_blanks, -ncol(raw_data)] # remove final blank timepoint

nms <- c("Male_Pop", "Fem_Pop", "Male_Inc", "Fem_Inc", "Male_Prev",
         "Fem_Prev", "Male_Mort", "Fem_Mort", "Male_Diag", "Fem_Diag",
         "Male_ART", "Fem_ART", "Male_AIDSdeaths", "Fem_AIDSdeaths")
age_groups <- setNames(vector("list", length(nms)), nms)

age_groups[] <- list(seq(0, 90)) # default is ages 0-90
age_groups[grepl("Inc", nms)] <- list(seq(10, 90)) # incidence begins age 10
age_groups[grepl("deaths", nms)] <- list(seq(-1, 90)) # deaths includes stillbirth (coded -1)

stopifnot(nrow(data) == sum(lengths(age_groups)))

tidy_data <- data.frame(
  name = unlist(mapply(rep, x = nms, each = lengths(age_groups))),
  age = unlist(age_groups)) %>% 
  tidyr::expand_grid(year = seq(1985, 2100)) %>% 
  dplyr::mutate(value = c(t(data))) # this gets the data frame into a long format straight away

tidy_data[c('sex', 'indicator')] <- str_split_fixed(tidy_data$name,pattern = "_", 2)

age_specifc_art95 <- tidy_data

age_specifc_baseline <- age_specifc_baseline %>%
  mutate(modeled_scenario = "Status quo")

age_specifc_no_prep <- age_specifc_no_prep %>% 
  mutate(modeled_scenario = "No PrEP")

age_specifc_no_prep_no_vmmc <- age_specifc_no_prep_no_vmmc %>% 
  mutate(modeled_scenario = "No PrEP + No VMMC")

age_specifc_no_prep_no_vmmc_condom28 <- age_specifc_no_prep_no_vmmc_condom28 %>% 
  mutate(modeled_scenario = "No PrEP + No VMMC + Condom usage 28%")

age_specifc_no_prep_no_vmmc_condom23 <- age_specifc_no_prep_no_vmmc_condom23 %>% 
  mutate(modeled_scenario = "No PrEP + No VMMC + Condom usage 23%")

age_specifc_art95 <- age_specifc_art95 %>% 
  mutate(modeled_scenario = "95-95-95")

age_specifc_no_prep_no_vmmc_art95 <- age_specifc_no_prep_no_vmmc_art95 %>% 
  mutate(modeled_scenario = "95-95-95 + No PrEP + No VMMC")

age_specific_all <- bind_rows(age_specifc_baseline,
                             age_specifc_no_prep,
                             age_specifc_no_prep_no_vmmc,
                             age_specifc_no_prep_no_vmmc_condom28,
                             age_specifc_no_prep_no_vmmc_condom23,
                             age_specifc_art95,
                             age_specifc_no_prep_no_vmmc_art95)

age_specific_all <- age_specific_all %>% 
  mutate(sex = if_else(sex == "Fem", "Female", "Male")) %>% 
  mutate(modeled_scenario = factor(modeled_scenario, 
                                   levels = c("Status quo", "No PrEP", "No PrEP + No VMMC",
                                              "No PrEP + No VMMC + Condom usage 28%",
                                              "No PrEP + No VMMC + Condom usage 23%",
                                              "95-95-95", "95-95-95 + No PrEP + No VMMC")))

age_specific_all %>% 
  filter(year >= 2020,
         indicator == "Inc") %>% 
  ggplot(aes(x = year, group = age, y = value, col = age)) +
  geom_line() +
  facet_grid(vars(modeled_scenario), vars(sex), 
             scales = "free") + 
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        # aspect.ratio=1, 
        legend.title = element_text(size = 11)) + 
  scale_color_viridis_c(option = "D", direction = -1, aesthetics = c("colour", "fill")) + 
  scale_y_continuous("HIV incidence per 1000", labels =(function(l) {round(l*1e3,1)}))

ggsave("unaids_figures/single_age_incidences.png", device = "png", units = "cm", 
       height = 30, width = 20)

tidy_data %>% 
  filter(indicator %in% c("AIDSdeaths"), 
         year > 2010) %>% 
  ggplot(aes(year, value, group = age, col = age, fill = age)) +
  geom_area() +
  facet_wrap(~sex, scales = "free", labeller = as_labeller(c("Fem" = "Female", "Male" = "Male"))) + 
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("AIDS-deaths (thousands)", labels =(function(l) {round(l/10^3,1)}),expand = c(0, 0)) + 
  scale_color_viridis_c(option = "B", direction = 1, aesthetics = c("colour", "fill")) +
  ggtitle("Age distribution of AIDS-deaths") 

tidy_data %>% 
  filter(indicator %in% c("Diag"), 
         year > 2010) %>% 
  ggplot(aes(year, value, group = age, col = age, fill = age)) +
  geom_area() +
  facet_wrap(~sex, scales = "free_x", labeller = as_labeller(c("Fem" = "Female", "Male" = "Male"))) + 
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Diagnosed PLWH (thousands)", labels =(function(l) {round(l/10^3,1)}),expand = c(0, 0)) + 
  scale_color_viridis_c(option = "B", direction = 1, aesthetics = c("colour", "fill")) +
  ggtitle("Age distribution of PLWH diagnosed with HIV") 

tidy_data %>% 
  filter(indicator %in% c("Pop"), 
         year > 2010) %>% 
  ggplot(aes(year, value, group = age, col = age, fill = age)) +
  geom_area() +
  facet_wrap(~sex, scales = "free_x", labeller = as_labeller(c("Fem" = "Female", "Male" = "Male"))) + 
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("Population (millions)", labels =(function(l) {round(l/10^6,1)}),expand = c(0, 0)) + 
  scale_color_viridis_c(option = "B", direction = 1, aesthetics = c("colour", "fill")) +
  ggtitle("Age distribution of population") 

tidy_data %>% 
  filter(indicator %in% c("ART"), 
         year > 2010) %>% 
  ggplot(aes(year, value, group = age, col = age, fill = age)) +
  geom_area() +
  facet_wrap(~sex, scales = "free_x", labeller = as_labeller(c("Fem" = "Female", "Male" = "Male"))) + 
  scale_x_continuous("", expand = c(0, 0)) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 11), 
        axis.title.y = element_text(size = 11), 
        axis.title.x = element_text(size = 11),
        legend.text = element_text(size = 11), 
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        aspect.ratio=1, 
        legend.title = element_text(size = 11)) +
  scale_y_continuous("PLWH on ART (millions)", labels =(function(l) {round(l/10^6,1)}),expand = c(0, 0)) + 
  scale_color_viridis_c(option = "B", direction = 1, aesthetics = c("colour", "fill")) +
  ggtitle("Age distribution of PLWH on ART") 
