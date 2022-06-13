#### Load packages/functions ####

source("R/support_modify_inputs.R")
source("scripts/modify_rollout.R")
source("R/read_and_run.R")

#### Set working directory ####

setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA")

#### Make empty dataframe for outputs ####

### Make empty dataframe 
## Make years column
years <- formatted_data[['year']]
## Make dataframe with same number of rows as years
outputs_df <- data.frame(years)
## Add columns for all the outputs
outputs_df[2:7] <- rep(NA, 86)
## Empty list for each output name
col_names <- rep(NA, 7)
## Name each output
col_names[2] <- "TotSTestFixedPoint"
col_names[3] <- "TotSTestANC"
col_names[4] <- "TotSTestTaxi"
col_names[5] <- "TotSTestIndex"
col_names[6] <- "TotSTestWork1"
col_names[7] <- "TotSTestWork2"

for (i in 2:length(col_names)) {
  names(outputs_df)[i] <- col_names[i]
}

#### Outputs with baseline testing rates ####
## read in input parameter file
data <- readLines("THEMBISAv18/Rollout_Original.txt")
## write unedited input parameter file
formatted_data <- format_data(data, dictionary)
rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
write(rollout, "THEMBISAv18/Rollout.txt")
## compile and model
run_thembisa()
## add outputs to columns 
outputs_df$TotSTestFixedPoint <- read_output("THEMBISAv18/TotSTestFixedPoint")$Simulation_1
outputs_df$TotSTestANC <- read_output("THEMBISAv18/TotSTestANC")$Simulation_1
outputs_df$TotSTestTaxi <- read_output("THEMBISAv18/TotSTestTaxi")$Simulation_1
outputs_df$TotSTestIndex <- read_output("THEMBISAv18/TotSTestIndex")$Simulation_1
outputs_df$TotSTestWork1 <- read_output("THEMBISAv18/TotSTestWork1")$Simulation_1
outputs_df$TotSTestWork2 <- read_output("THEMBISAv18/TotSTestWork2")$Simulation_1

# sum of all self tests

outputs_df$total_self_test <- outputs_df$TotSTestFixedPoint + outputs_df$TotSTestANC +
  outputs_df$TotSTestTaxi + outputs_df$TotSTestIndex + outputs_df$TotSTestWork1 + 
  outputs_df$TotSTestWork2

total_self_test <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, total_self_test)) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("New cases of HIV") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0,3.5e+05) + 
  ggtitle("Total self tests")

