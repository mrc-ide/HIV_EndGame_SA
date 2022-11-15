library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("R/support_modify_inputs.R"))
source(here("scripts/modify_rollout.R"))
source(here("R/read_and_run.R"))

extend_rollout <- function(fomatted_data, extended_year){
  n <- nrow(formatted_data$data)
  y <- extended_year - (1985 + (n-1))
  idx <- c(seq_len(n), rep(n ,y))
  idy <- as.integer(seq(1985, extended_year, 1))
  formatted_data$data <- formatted_data$data[idx,]
  formatted_data$year <- idy
  formatted_data
}
data <- readLines(here("THEMBISAv18/Rollout_Original.txt"))
formatted_data <- format_data(data, dictionary)
formatted_data <- extend_rollout(fomatted_data, 2100)
rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
write(rollout, "THEMBISAv18/Rollout_Original.txt")
write(rollout, "THEMBISAv18/Rollout.txt")
## compile and model
run_thembisa()
temp <- read_thembisa_scenario(output_names)

# checking demographic numbers
output_names <- c("TotalHIVtests", "NewAdultHIV",
                   "TotBirths", "TotPop", "FemalesOver15", "MalesOver15")

run_thembisa()
temp <- read_thembisa_scenario(output_names)

temp %>% filter(indicator == "TotPop") %>% 
  group_by(year, parameter_set) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, colour = parameter_set)) +
  geom_line() + expand_limits(y=0) + theme_bw() + ggtitle("Total Population")

temp %>% filter(indicator == "FemalesOver15") %>% 
  group_by(year, parameter_set) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, colour = parameter_set)) +
  geom_line() + expand_limits(y=0) + theme_bw() + ggtitle("Females aged > 15")

temp %>% filter(indicator == "MalesOver15") %>% 
  group_by(year, parameter_set) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, colour = parameter_set)) +
  geom_line() + expand_limits(y=0) + theme_bw() + ggtitle("Males aged > 15")

temp %>% filter(indicator == "TotBirths") %>% 
  group_by(year, parameter_set) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, colour = parameter_set)) +
  geom_line() + expand_limits(y=0) + theme_bw() + ggtitle("Total births")

temp %>% filter(indicator == "NewAdultHIV") %>% 
  group_by(year, parameter_set) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, colour = parameter_set)) +
  geom_line() + expand_limits(y=0) + theme_bw() + ggtitle("New HIV infections (adults)")

temp %>% filter(indicator == "TotalHIVtests") %>% 
  group_by(year, parameter_set) %>% 
  summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, mean, colour = parameter_set)) +
  geom_line() + expand_limits(y=0) + theme_bw() + ggtitle("Total HIV tests (adults)")
