#### Set working directory ####

setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA")

#### Load packages/functions ####

source("R/support_modify_inputs.R")
source("scripts/modify_rollout.R")
source("R/read_and_run.R")

#### Make empty dataframe for outputs ####

### Make empty dataframe 
## Make years column
years <- formatted_data[['year']]
## Make dataframe with same number of rows as years
outputs_df <- data.frame(years)

# names of all the outputs of interest

output_names <- c("TotalNewHIV", "TotalHIV", "TotalHIVtests", "LYlostAIDS", 
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", "NewAdultHIV", 
                  "DiagnosedHIV_M", "DiagnosedHIV_F", "UndiagnosedHIV_M", 
                  "UndiagnosedHIV_F", "HIVtestsPos", "StartingARTtot",
                  "BirthsDiagHIV", "TotBirthsHIV", "FirstHIVtestsPos")

# calculate number of scenarios if there are 10 per output

n_outputs <- length(output_names)
n_scenarios <- n_outputs * 10 + 1

## Add columns for all the outputs
outputs_df[2:n_scenarios] <- rep(NA, 86)

## Empty list for each output name
col_names <- rep(NA, n_scenarios)

# make a vector of the scenarios including baseline
# reveresed so its in the right order in the next loop 
five_years <- c("2065", "2060", "2055", "2050", "2045", "2040", "2035", "2030",
                "2025", "baseline")
# makes a list of all the scenario namnes 
for (j in 1:length(five_years)){
  for (i in 1:n_outputs){
    col_names[((i+1)+(i*9)-j)+1] <- paste(output_names[i], five_years[j], sep = "_")
  }
}
# assigns scenario names to columns in dataframe
for (i in 2:n_scenarios){
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
## Read all baseline outputs to columns 
# this gives a warning but it works
for (i in 0:n_outputs){
  outputs_df[2+i*10] <- read_output(output_names[i+1])
}

#### Output with changed testing rates ####
## loop for each 5-year interval between 2025 and 2065
## make new output for each year intervention implemented
for (i in 1:9){
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  formatted_data <- format_data(data, dictionary)
  formatted_data <- edit_formatted_data("rate_first_test_neg_fem_under_25", 
                                        new_values = 0.0, starting_year = (2020+(i*5)))
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  run_thembisa()
  outputs_df[2+i] <- read_output("TotalNewHIV")
  outputs_df[12+i] <- read_output("TotalHIV")
  outputs_df[22+i] <- read_output("TotalHIVtests")
  outputs_df[32+i] <- read_output("LYlostAIDS")
  outputs_df[42+i] <- read_output("AIDSdeathsAdultM")
  outputs_df[52+i] <- read_output("AIDSdeathsAdultF")
  outputs_df[62+i] <- read_output("NewAdultHIV")
  outputs_df[72+i] <- read_output("DiagnosedHIV_M")
  outputs_df[82+i] <- read_output("DiagnosedHIV_F")
  outputs_df[92+i] <- read_output("UndiagnosedHIV_M")
  outputs_df[102+i] <- read_output("UndiagnosedHIV_F")
  outputs_df[112+i] <- read_output("HIVtestsPos")
  outputs_df[122+i] <- read_output("StartingARTtot")
  outputs_df[132+i] <- read_output("BirthsDiagHIV")
  outputs_df[142+i] <- read_output("TotBirthsHIV")
  outputs_df[152+i] <- read_output("FirstHIVtestsPos")
}

### write csv of outputs_df ####

write.csv(outputs_df, "outputs_df.csv", row.names = FALSE)
