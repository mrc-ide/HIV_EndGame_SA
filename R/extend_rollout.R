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
  formatted_data$data <- formatted_data$data[idx,]
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
