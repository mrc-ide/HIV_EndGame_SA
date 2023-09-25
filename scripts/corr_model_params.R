library(ggcorrplot)

inc_model_param <- read_csv("~/Documents/Work in progress/ModelParams.csv")

corr <- round(cor(inc_model_param[,c(48, 47, 49, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 34, 38, 45)]),2)

corr[,1]

p.mat <- cor_pmat(inc_model_param[,c(48, 47, 49, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 34, 38, 45)])

p.mat[,1]

ggcorrplot(corr, outline.col = "white", p.mat = p.mat,type = "lower", lab = TRUE,ggtheme = theme_classic())

ggsave("../unaids_figures/corrplot.png", units = "cm", height = 25, width = 25, device = "png")

# read HIVin15to49 output
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
HIVinc15to49 <- read_thembisa_output("HIVinc15to49")

# filter by year in 2100
HIVinc15to49_2100 <- HIVinc15to49 %>% filter(year == 2100)
HIVinc15to49_2100 <- rename(HIVinc15to49_2100, HIVinc2100 = value)

# filter by year in 2025
HIVinc15to49_2025 <- HIVinc15to49 %>% filter(year == 2025)
HIVinc15to49_2025<- rename(HIVinc15to49_2025, HIVinc2025 = value)

# filter by year in 2050
HIVinc15to49_2050 <- HIVinc15to49 %>% filter(year == 2050)
HIVinc15to49_2050<- rename(HIVinc15to49_2050, HIVinc2050 = value)

# add to df
inc_model_param$HIVinc2100 <- HIVinc15to49_2100$HIVinc2100
inc_model_param$HIVinc2025 <- HIVinc15to49_2025$HIVinc2025
inc_model_param$HIVinc2050 <- HIVinc15to49_2050$HIVinc2050

# scatterplots of model param 32 (male to male transmission)

inc_model_param %>% 
  ggplot(aes(ModelParameter32, HIVinc2100)) + 
  geom_point() +
  theme_classic() + 
  scale_y_continuous("HIV incidence in 2100 (per 1000)", labels =(function(l) {round(l*1e3,1)})) + 
  scale_x_continuous("Male-to-male transmission\nprobability per sex act")

inc_model_param %>% 
  ggplot(aes(ModelParameter32, HIVinc2025)) + 
  geom_point() +
  theme_classic() + 
  scale_y_continuous("HIV incidence in 2025 (per 1000)", labels =(function(l) {round(l*1e3,1)})) + 
  scale_x_continuous("Male-to-male transmission\nprobability per sex act")

inc_model_param %>% 
  ggplot(aes(ModelParameter43, HIVinc2100)) + 
  geom_point() +
  theme_classic() + 
  scale_y_continuous("HIV incidence in 2100 (per 1000)", labels =(function(l) {round(l*1e3,1)})) + 
  scale_x_continuous("Odds of 'true' viral suppression\nrelative to that in IeDEA-SA")
