#### find 0 additional HIV infections ####
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(contoureR)
add_inf <- cumulative_art_change %>% 
  filter(scenario == "absolute_dif", indicator == "NewAdultHIV", pitc_reduction_year == 2025)
x <- 1:length(add_inf$test_reduction)
y <- 1:length(add_inf$art_int_rate)
z <- add_inf$mean


cl1 <- contourLinesR(x,y,z,nlevels = 2)

x
data(volcano)
head(volcano)
