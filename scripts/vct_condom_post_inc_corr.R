# Correlation between posterior values for effect of HIV diagnosis 
# on condom use and HIV incidence amnongst 1000 parameter sets #

# libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(ggtext)

# source functions
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")

# compile and run thembisa with baseline condtions
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")
system("./thembisa")

# read HIVin15to49 output
HIVinc15to49 <- read_thembisa_output("HIVinc15to49")

# read previously prepared csv of column 12 of model parameters
vctcondom_posterior <- read_csv("ModelParamCol12.csv",col_names = FALSE)
names(vctcondom_posterior) <- "vct_condom_posterior"

# filter by year in 2100
HIVinc15to49_2100 <- HIVinc15to49 %>% filter(year == 2100)
HIVinc15to49_2100 <- rename(HIVinc15to49_2100, HIVinc2100 = value)

# join dfs together
vctcondom_correlation <- cbind(HIVinc15to49_2100, vctcondom_posterior)

# graph show correlation

vctcondom_correlation %>% 
  ggplot(aes(x = vct_condom_posterior, y = HIVinc2100)) +
  geom_point() +
  theme_classic() +
  scale_x_continuous("Posterior: reduction in unprotected sex after HIV diagnosis") +
  scale_y_continuous("HIV incidence in 2100") + 
  geom_smooth(method = "lm", formula = y ~ x)


# linear regression
lm_vctcondom <- lm(data = vctcondom_correlation, formula = HIVinc2100 ~ vct_condom_posterior)
summary(lm_vctcondom)
confint(lm_vctcondom)

# predicted values
predict_vct_lm <- predict(lm_vctcondom, interval = "confidence")

vctcondom_corr_pred <- cbind(vctcondom_correlation, predict_vct_lm)

vctcondom_corr_pred %>% 
  ggplot(aes(x = vct_condom_posterior)) +
  geom_point(aes(y = HIVinc2100)) +
  theme_classic() +
  scale_x_continuous("Posterior: reduction in unprotected sex after HIV diagnosis") +
  scale_y_continuous("HIV incidence in 2100") + 
  geom_line(aes(y = fit), colour = "blue", size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  annotate(geom = "text", colour = "blue", x = 0.235, y = 0.0007, label = "paste(italic(R) ^ 2, \" = 0.0147\")", parse = TRUE) +
  annotate(geom = "text", colour = "blue", x = 0.22, y =0.0009, label = "y = -0.00074x + 0.00096")

# excluding outliers

lm_vctcondom_exl <- lm(data = filter(vctcondom_correlation, vct_condom_posterior != 0.266498), 
                       formula = HIVinc2100 ~ vct_condom_posterior)
summary(lm_vctcondom_exl)
confint(lm_vctcondom_exl)

predict_vct_lm_excl <- predict(lm_vctcondom_exl, interval = "confidence")

vctcondom_correlation_excl <- filter(vctcondom_correlation, vct_condom_posterior != 0.266498)

vctcondom_corr_pred_excl <- cbind(vctcondom_correlation_excl, predict_vct_lm_excl)

vctcondom_corr_pred_excl %>% 
  ggplot(aes(x = vct_condom_posterior)) +
  geom_point(aes(y = HIVinc2100)) +
  theme_classic() +
  scale_x_continuous("Posterior: reduction in unprotected sex after HIV diagnosis") +
  scale_y_continuous("HIV incidence in 2100") + 
  geom_line(aes(y = fit), colour = "blue", size = 1) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
  annotate(geom = "text", colour = "blue", x = 0.22, y = 0.00078, label = "paste(italic(R) ^ 2, \" = 0.0006\")", parse = TRUE) +
  annotate(geom = "text", colour = "blue", x = 0.20, y =0.000975, label = "y = 0.00017x + 0.00087")

# boxplots 

vctcondom_correlation %>% 
  ggplot(aes(x = vct_condom_posterior)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  scale_x_continuous("Posterior: reduction in unprotected sex after HIV diagnosis")

vctcondom_correlation %>% 
  ggplot(aes(x = vct_condom_posterior)) +
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density() +
  theme_classic() +
  scale_x_continuous("Posterior: reduction in unprotected sex after HIV diagnosis") + 

vctcondom_correlation %>% 
  ggplot(aes(x = HIVinc2100)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) + 
  scale_x_continuous("HIV incidence in 2100")

vctcondom_correlation %>% 
  ggplot(aes(x = HIVinc2100)) +  
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density() +
  theme_classic() +
  scale_x_continuous("HIV incidence in 2100")

