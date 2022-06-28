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

## Calculate total AIDS related mortality in Adults 

outputs_df$TotalAIDSdeathsAdult_baseline <- outputs_df$AIDSdeathsAdultF_baseline + 
  outputs_df$AIDSdeathsAdultM_baseline
outputs_df$TotalAIDSdeathsAdult_2025 <- outputs_df$AIDSdeathsAdultF_2025 + 
  outputs_df$AIDSdeathsAdultM_2025
outputs_df$TotalAIDSdeathsAdult_2030 <- outputs_df$AIDSdeathsAdultF_2030 + 
  outputs_df$AIDSdeathsAdultM_2030
outputs_df$TotalAIDSdeathsAdult_2035 <- outputs_df$AIDSdeathsAdultF_2035 + 
  outputs_df$AIDSdeathsAdultM_2035
outputs_df$TotalAIDSdeathsAdult_2040 <- outputs_df$AIDSdeathsAdultF_2040 + 
  outputs_df$AIDSdeathsAdultM_2040
outputs_df$TotalAIDSdeathsAdult_2045 <- outputs_df$AIDSdeathsAdultF_2045 + 
  outputs_df$AIDSdeathsAdultM_2045
outputs_df$TotalAIDSdeathsAdult_2050 <- outputs_df$AIDSdeathsAdultF_2050 + 
  outputs_df$AIDSdeathsAdultM_2050
outputs_df$TotalAIDSdeathsAdult_2055 <- outputs_df$AIDSdeathsAdultF_2055 + 
  outputs_df$AIDSdeathsAdultM_2055
outputs_df$TotalAIDSdeathsAdult_2060 <- outputs_df$AIDSdeathsAdultF_2060 + 
  outputs_df$AIDSdeathsAdultM_2060
outputs_df$TotalAIDSdeathsAdult_2065 <- outputs_df$AIDSdeathsAdultF_2065 + 
  outputs_df$AIDSdeathsAdultM_2065


## Calculate total diagnosed HIV in Adults 

outputs_df$DiagnosedHIV_baseline <- outputs_df$DiagnosedHIV_F_baseline + 
  outputs_df$DiagnosedHIV_M_baseline
outputs_df$DiagnosedHIV_2025 <- outputs_df$DiagnosedHIV_F_2025 + 
  outputs_df$DiagnosedHIV_M_2025
outputs_df$DiagnosedHIV_2030 <- outputs_df$DiagnosedHIV_F_2030 + 
  outputs_df$DiagnosedHIV_M_2030
outputs_df$DiagnosedHIV_2035 <- outputs_df$DiagnosedHIV_F_2035 + 
  outputs_df$DiagnosedHIV_M_2035
outputs_df$DiagnosedHIV_2040 <- outputs_df$DiagnosedHIV_F_2040 + 
  outputs_df$DiagnosedHIV_M_2040
outputs_df$DiagnosedHIV_2045 <- outputs_df$DiagnosedHIV_F_2045 + 
  outputs_df$DiagnosedHIV_M_2045
outputs_df$DiagnosedHIV_2050 <- outputs_df$DiagnosedHIV_F_2050 + 
  outputs_df$DiagnosedHIV_M_2050
outputs_df$DiagnosedHIV_2055 <- outputs_df$DiagnosedHIV_F_2055 + 
  outputs_df$DiagnosedHIV_M_2055
outputs_df$DiagnosedHIV_2060 <- outputs_df$DiagnosedHIV_F_2060 + 
  outputs_df$DiagnosedHIV_M_2060
outputs_df$DiagnosedHIV_2065 <- outputs_df$DiagnosedHIV_F_2065 + 
  outputs_df$DiagnosedHIV_M_2065

## Calculate undiagnosed HIV in Adults 

outputs_df$UndiagnosedHIV_baseline <- outputs_df$UndiagnosedHIV_F_baseline + 
  outputs_df$UndiagnosedHIV_M_baseline
outputs_df$UndiagnosedHIV_2025 <- outputs_df$UndiagnosedHIV_F_2025 + 
  outputs_df$UndiagnosedHIV_M_2025
outputs_df$UndiagnosedHIV_2030 <- outputs_df$UndiagnosedHIV_F_2030 + 
  outputs_df$UndiagnosedHIV_M_2030
outputs_df$UndiagnosedHIV_2035 <- outputs_df$UndiagnosedHIV_F_2035 + 
  outputs_df$UndiagnosedHIV_M_2035
outputs_df$UndiagnosedHIV_2040 <- outputs_df$UndiagnosedHIV_F_2040 + 
  outputs_df$UndiagnosedHIV_M_2040
outputs_df$UndiagnosedHIV_2045 <- outputs_df$UndiagnosedHIV_F_2045 + 
  outputs_df$UndiagnosedHIV_M_2045
outputs_df$UndiagnosedHIV_2050 <- outputs_df$UndiagnosedHIV_F_2050 + 
  outputs_df$UndiagnosedHIV_M_2050
outputs_df$UndiagnosedHIV_2055 <- outputs_df$UndiagnosedHIV_F_2055 + 
  outputs_df$UndiagnosedHIV_M_2055
outputs_df$UndiagnosedHIV_2060 <- outputs_df$UndiagnosedHIV_F_2060 + 
  outputs_df$UndiagnosedHIV_M_2060
outputs_df$UndiagnosedHIV_2065 <- outputs_df$UndiagnosedHIV_F_2065 + 
  outputs_df$UndiagnosedHIV_M_2065


# Plot of baseline total new hiv 
total_new_hiv_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, TotalNewHIV_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("New cases of HIV") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0,2.5e+05) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
total_new_hiv_2025 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2025, colour = "Intervention")) +
  ggtitle("2025")
total_new_hiv_2030 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2030, colour = "Intervention")) + 
  ggtitle("2030")
total_new_hiv_2035 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2035, colour = "Intervention")) +
  ggtitle("2035")
total_new_hiv_2040 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2040, colour = "Intervention")) +
  ggtitle("2040")
total_new_hiv_2045 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2045, colour = "Intervention")) +
  ggtitle("2045")
total_new_hiv_2050 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2050, colour = "Intervention")) +
  ggtitle("2050")
total_new_hiv_2055 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2055, colour = "Intervention")) +
  ggtitle("2055")
total_new_hiv_2060 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2060, colour = "Intervention")) +
  ggtitle("2060")
total_new_hiv_2065 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2065, colour = "Intervention")) +
  ggtitle("2065")
total_new_hiv_2070 <- total_new_hiv_baseline + 
  geom_line(aes(years, TotalNewHIV_2070, colour = "Intervention")) +
  ggtitle("2070")

## all new hiv cases in  grid 
grid.arrange(total_new_hiv_2025, total_new_hiv_2030, total_new_hiv_2035, 
             total_new_hiv_2040, total_new_hiv_2045, total_new_hiv_2050, 
             total_new_hiv_2055, total_new_hiv_2060, total_new_hiv_2065, 
             top="New cases of HIV")

# Plot of baseline total / cumulative hiv 
cumulative_hiv_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, TotalHIV_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Cumulative cases of HIV") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(4e+06,9e+06) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
cumulative_hiv_2025 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2025, colour = "Intervention")) +
  ggtitle("2025")
cumulative_hiv_2030 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2030, colour = "Intervention")) + 
  ggtitle("2030")
cumulative_hiv_2035 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2035, colour = "Intervention")) +
  ggtitle("2035")
cumulative_hiv_2040 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2040, colour = "Intervention")) +
  ggtitle("2040")
cumulative_hiv_2045 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2045, colour = "Intervention")) +
  ggtitle("2045")
cumulative_hiv_2050 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2050, colour = "Intervention")) +
  ggtitle("2050")
cumulative_hiv_2055 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2055, colour = "Intervention")) +
  ggtitle("2055")
cumulative_hiv_2060 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2060, colour = "Intervention")) +
  ggtitle("2060")
cumulative_hiv_2065 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2065, colour = "Intervention")) +
  ggtitle("2065")
cumulative_hiv_2070 <- cumulative_hiv_baseline + 
  geom_line(aes(years, TotalHIV_2070, colour = "Intervention")) +
  ggtitle("2070")

## all cumulative hiv cases in  grid 
grid.arrange(cumulative_hiv_2025, cumulative_hiv_2030, cumulative_hiv_2035, 
             cumulative_hiv_2040, cumulative_hiv_2045, cumulative_hiv_2050, 
             cumulative_hiv_2055, cumulative_hiv_2060, cumulative_hiv_2065, 
             top="Cumulative cases of HIV")


# Plot of baseline total cumulative hiv tests
hiv_tests_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, TotalHIVtests_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("HIV tests") +
  xlab("Years") +
  xlim(2020, 2070) +
  #ylim(0,5e+06) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
hiv_tests_2025 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2025, colour = "Intervention")) +
  ggtitle("2025")
hiv_tests_2030 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2030, colour = "Intervention")) + 
  ggtitle("2030")
hiv_tests_2035 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2035, colour = "Intervention")) +
  ggtitle("2035")
hiv_tests_2040 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2040, colour = "Intervention")) +
  ggtitle("2040")
hiv_tests_2045 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2045, colour = "Intervention")) +
  ggtitle("2045")
hiv_tests_2050 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2050, colour = "Intervention")) +
  ggtitle("2050")
hiv_tests_2055 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2055, colour = "Intervention")) +
  ggtitle("2055")
hiv_tests_2060 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2060, colour = "Intervention")) +
  ggtitle("2060")
hiv_tests_2065 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2065, colour = "Intervention")) +
  ggtitle("2065")
hiv_tests_2070 <- hiv_tests_baseline + 
  geom_line(aes(years, TotalHIVtests_2070, colour = "Intervention")) +
  ggtitle("2070")

## all cumulative hiv test in  grid 
grid.arrange(hiv_tests_2025, hiv_tests_2030, hiv_tests_2035, 
             hiv_tests_2040, hiv_tests_2045, hiv_tests_2050, 
             hiv_tests_2055, hiv_tests_2060, hiv_tests_2065, 
             top="HIV tests per year")


# Plot of baseline life years lost 
ly_lost_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, LYlostAIDS_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Life years lost") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0, 2.5e+6) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
ly_lost_2025 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2025, colour = "Intervention")) +
  ggtitle("2025")
ly_lost_2030 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2030, colour = "Intervention")) + 
  ggtitle("2030")
ly_lost_2035 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2035, colour = "Intervention")) +
  ggtitle("2035")
ly_lost_2040 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2040, colour = "Intervention")) +
  ggtitle("2040")
ly_lost_2045 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2045, colour = "Intervention")) +
  ggtitle("2045")
ly_lost_2050 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2050, colour = "Intervention")) +
  ggtitle("2050")
ly_lost_2055 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2055, colour = "Intervention")) +
  ggtitle("2055")
ly_lost_2060 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2060, colour = "Intervention")) +
  ggtitle("2060")
ly_lost_2065 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2065, colour = "Intervention")) +
  ggtitle("2065")
ly_lost_2070 <- ly_lost_baseline + 
  geom_line(aes(years, LYlostAIDS_2070, colour = "Intervention")) +
  ggtitle("2070")
grid.arrange(ly_lost_2025, ly_lost_2030, ly_lost_2035, 
             ly_lost_2040, ly_lost_2045, ly_lost_2050, 
             ly_lost_2055, ly_lost_2060, ly_lost_2065,
             top = "Life years lost due to AIDS")

# Plot of baseline aids related mortality in adults
aids_mortality_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, TotalAIDSdeathsAdult_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("AIDS related deaths") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0, 60000) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
aids_mortality_2025 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2025, colour = "Intervention")) +
  ggtitle("2025")
aids_mortality_2030 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2030, colour = "Intervention")) + 
  ggtitle("2030")
aids_mortality_2035 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2035, colour = "Intervention")) +
  ggtitle("2035")
aids_mortality_2040 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2040, colour = "Intervention")) +
  ggtitle("2040")
aids_mortality_2045 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2045, colour = "Intervention")) +
  ggtitle("2045")
aids_mortality_2050 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2050, colour = "Intervention")) +
  ggtitle("2050")
aids_mortality_2055 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2055, colour = "Intervention")) +
  ggtitle("2055")
aids_mortality_2060 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2060, colour = "Intervention")) +
  ggtitle("2060")
aids_mortality_2065 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2065, colour = "Intervention")) +
  ggtitle("2065")
aids_mortality_2070 <- aids_mortality_baseline + 
  geom_line(aes(years, TotalAIDSdeathsAdult_2070, colour = "Intervention")) +
  ggtitle("2070")
grid.arrange(aids_mortality_2025, aids_mortality_2030, aids_mortality_2035, 
             aids_mortality_2040, aids_mortality_2045, aids_mortality_2050, 
             aids_mortality_2055, aids_mortality_2060, aids_mortality_2065,
             top = "AIDS related mortality in Adults")



# Plot of baseline adult new hiv 
new_adult_hiv_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, NewAdultHIV_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("New cases of adult HIV") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0,2.2e+05) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
new_adult_hiv_2025 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2025, colour = "Intervention")) +
  ggtitle("2025")
new_adult_hiv_2030 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2030, colour = "Intervention")) + 
  ggtitle("2030")
new_adult_hiv_2035 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2035, colour = "Intervention")) +
  ggtitle("2035")
new_adult_hiv_2040 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2040, colour = "Intervention")) +
  ggtitle("2040")
new_adult_hiv_2045 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2045, colour = "Intervention")) +
  ggtitle("2045")
new_adult_hiv_2050 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2050, colour = "Intervention")) +
  ggtitle("2050")
new_adult_hiv_2055 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2055, colour = "Intervention")) +
  ggtitle("2055")
new_adult_hiv_2060 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2060, colour = "Intervention")) +
  ggtitle("2060")
new_adult_hiv_2065 <- new_adult_hiv_baseline + 
  geom_line(aes(years, NewAdultHIV_2065, colour = "Intervention")) +
  ggtitle("2065")

## new adult hiv cases in  grid 
grid.arrange(new_adult_hiv_2025, new_adult_hiv_2030, new_adult_hiv_2035, 
             new_adult_hiv_2040, new_adult_hiv_2045, new_adult_hiv_2050, 
             new_adult_hiv_2055, new_adult_hiv_2060, new_adult_hiv_2065, 
             top="New cases of adult HIV")


# Plot of baseline diagnosed hiv 
diagnosed_hiv_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, DiagnosedHIV_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("HIV Diagnoses") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(4e+06,9e+06) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
diagnosed_hiv_2025 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2025, colour = "Intervention")) +
  ggtitle("2025")
diagnosed_hiv_2030 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2030, colour = "Intervention")) + 
  ggtitle("2030")
diagnosed_hiv_2035 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2035, colour = "Intervention")) +
  ggtitle("2035")
diagnosed_hiv_2040 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2040, colour = "Intervention")) +
  ggtitle("2040")
diagnosed_hiv_2045 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2045, colour = "Intervention")) +
  ggtitle("2045")
diagnosed_hiv_2050 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2050, colour = "Intervention")) +
  ggtitle("2050")
diagnosed_hiv_2055 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2055, colour = "Intervention")) +
  ggtitle("2055")
diagnosed_hiv_2060 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2060, colour = "Intervention")) +
  ggtitle("2060")
diagnosed_hiv_2065 <- diagnosed_hiv_baseline + 
  geom_line(aes(years, DiagnosedHIV_2065, colour = "Intervention")) +
  ggtitle("2065")

## new adult hiv cases in  grid 
grid.arrange(diagnosed_hiv_2025, diagnosed_hiv_2030, diagnosed_hiv_2035, 
             diagnosed_hiv_2040, diagnosed_hiv_2045, diagnosed_hiv_2050, 
             diagnosed_hiv_2055, diagnosed_hiv_2060, diagnosed_hiv_2065, 
             top="Diagnosed HIV")



# Plot of baseline undiagnosed hiv 
undiagnosed_hiv_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, UndiagnosedHIV_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Undiagnosed HIV") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0,8e+05) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
undiagnosed_hiv_2025 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2025, colour = "Intervention")) +
  ggtitle("2025")
undiagnosed_hiv_2030 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2030, colour = "Intervention")) + 
  ggtitle("2030")
undiagnosed_hiv_2035 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2035, colour = "Intervention")) +
  ggtitle("2035")
undiagnosed_hiv_2040 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2040, colour = "Intervention")) +
  ggtitle("2040")
undiagnosed_hiv_2045 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2045, colour = "Intervention")) +
  ggtitle("2045")
undiagnosed_hiv_2050 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2050, colour = "Intervention")) +
  ggtitle("2050")
undiagnosed_hiv_2055 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2055, colour = "Intervention")) +
  ggtitle("2055")
undiagnosed_hiv_2060 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2060, colour = "Intervention")) +
  ggtitle("2060")
undiagnosed_hiv_2065 <- undiagnosed_hiv_baseline + 
  geom_line(aes(years, UndiagnosedHIV_2065, colour = "Intervention")) +
  ggtitle("2065")

## new undiagnosed cases in  grid 
grid.arrange(undiagnosed_hiv_2025, undiagnosed_hiv_2030, undiagnosed_hiv_2035, 
             undiagnosed_hiv_2040, undiagnosed_hiv_2045, undiagnosed_hiv_2050, 
             undiagnosed_hiv_2055, undiagnosed_hiv_2060, undiagnosed_hiv_2065, 
             top="Undiagnosed HIV")


# Plot of positive hiv tests baseline 
pos_hiv_tests_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, HIVtestsPos_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Proportion of tests") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0,0.05) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
pos_hiv_tests_2025 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2025, colour = "Intervention")) +
  ggtitle("2025")
pos_hiv_tests_2030 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2030, colour = "Intervention")) + 
  ggtitle("2030")
pos_hiv_tests_2035 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2035, colour = "Intervention")) +
  ggtitle("2035")
pos_hiv_tests_2040 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2040, colour = "Intervention")) +
  ggtitle("2040")
pos_hiv_tests_2045 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2045, colour = "Intervention")) +
  ggtitle("2045")
pos_hiv_tests_2050 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2050, colour = "Intervention")) +
  ggtitle("2050")
pos_hiv_tests_2055 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2055, colour = "Intervention")) +
  ggtitle("2055")
pos_hiv_tests_2060 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2060, colour = "Intervention")) +
  ggtitle("2060")
pos_hiv_tests_2065 <- pos_hiv_tests_baseline + 
  geom_line(aes(years, HIVtestsPos_2065, colour = "Intervention")) +
  ggtitle("2065")

## positive hiv proportion in  grid 
grid.arrange(pos_hiv_tests_2025, pos_hiv_tests_2030, pos_hiv_tests_2035, 
             pos_hiv_tests_2040, pos_hiv_tests_2045, pos_hiv_tests_2050, 
             pos_hiv_tests_2055, pos_hiv_tests_2060, pos_hiv_tests_2065, 
             top="Positive proportion of HIV tests")


# Plot of numbers starting art baseline 
starting_art_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, StartingARTtot_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Starting ART") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0,3e+05) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
starting_art_2025 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2025, colour = "Intervention")) +
  ggtitle("2025")
starting_art_2030 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2030, colour = "Intervention")) + 
  ggtitle("2030")
starting_art_2035 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2035, colour = "Intervention")) +
  ggtitle("2035")
starting_art_2040 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2040, colour = "Intervention")) +
  ggtitle("2040")
starting_art_2045 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2045, colour = "Intervention")) +
  ggtitle("2045")
starting_art_2050 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2050, colour = "Intervention")) +
  ggtitle("2050")
starting_art_2055 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2055, colour = "Intervention")) +
  ggtitle("2055")
starting_art_2060 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2060, colour = "Intervention")) +
  ggtitle("2060")
starting_art_2065 <- starting_art_baseline + 
  geom_line(aes(years, StartingARTtot_2065, colour = "Intervention")) +
  ggtitle("2065")

## starting art in  grid 
grid.arrange(starting_art_2025, starting_art_2030, starting_art_2035, 
             starting_art_2040, starting_art_2045, starting_art_2050, 
             starting_art_2055, starting_art_2060, starting_art_2065, 
             top="Starting ART")


# Plot of proportion of infants diagnosed at birth baseline 
births_diagnosed_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, BirthsDiagHIV_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Proportion of births") +
  xlab("Years") +
  xlim(2020, 2070) +
  ylim(0.6,1) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
births_diagnosed_2025 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2025, colour = "Intervention")) +
  ggtitle("2025")
births_diagnosed_2030 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2030, colour = "Intervention")) + 
  ggtitle("2030")
births_diagnosed_2035 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2035, colour = "Intervention")) +
  ggtitle("2035")
births_diagnosed_2040 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2040, colour = "Intervention")) +
  ggtitle("2040")
births_diagnosed_2045 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2045, colour = "Intervention")) +
  ggtitle("2045")
births_diagnosed_2050 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2050, colour = "Intervention")) +
  ggtitle("2050")
births_diagnosed_2055 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2055, colour = "Intervention")) +
  ggtitle("2055")
births_diagnosed_2060 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2060, colour = "Intervention")) +
  ggtitle("2060")
births_diagnosed_2065 <- births_diagnosed_baseline + 
  geom_line(aes(years, BirthsDiagHIV_2065, colour = "Intervention")) +
  ggtitle("2065")

## diagnosed births in  grid 
grid.arrange(births_diagnosed_2025, births_diagnosed_2030, births_diagnosed_2035, 
             births_diagnosed_2040, births_diagnosed_2045, births_diagnosed_2050, 
             births_diagnosed_2055, births_diagnosed_2060, births_diagnosed_2065, 
             top="Proportion diagnosed at birth")



# Plot of total births with hiv baseline 
total_births_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, TotBirthsHIV_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Number of births") +
  xlab("Years") +
  xlim(2020, 2070) +
  #ylim(0.6,1) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
total_births_2025 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2025, colour = "Intervention")) +
  ggtitle("2025")
total_births_2030 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2030, colour = "Intervention")) + 
  ggtitle("2030")
total_births_2035 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2035, colour = "Intervention")) +
  ggtitle("2035")
total_births_2040 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2040, colour = "Intervention")) +
  ggtitle("2040")
total_births_2045 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2045, colour = "Intervention")) +
  ggtitle("2045")
total_births_2050 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2050, colour = "Intervention")) +
  ggtitle("2050")
total_births_2055 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2055, colour = "Intervention")) +
  ggtitle("2055")
total_births_2060 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2060, colour = "Intervention")) +
  ggtitle("2060")
total_births_2065 <- total_births_baseline + 
  geom_line(aes(years, TotBirthsHIV_2065, colour = "Intervention")) +
  ggtitle("2065")

## number of births with hivb in  grid 
grid.arrange(total_births_2025, total_births_2030, total_births_2035, 
             total_births_2040, total_births_2045, total_births_2050, 
             total_births_2055, total_births_2060, total_births_2065, 
             top="Births with HIV")



# Plot of first hiv test pos baseline 
first_test_pos_baseline <- 
  ggplot(outputs_df) + 
  geom_line(aes(years, FirstHIVtestsPos_baseline, colour = "Baseline")) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Proportion of tests") +
  xlab("Years") +
  xlim(2020, 2070) +
  #ylim(0.6,1) +
  scale_color_manual(name = "Intervention", 
                     values = c("Baseline" = "darkblue", 
                                "Intervention" = "red"))
# plot of reduction from each year vs baseline
first_test_pos_2025 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2025, colour = "Intervention")) +
  ggtitle("2025")
first_test_pos_2030 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2030, colour = "Intervention")) + 
  ggtitle("2030")
first_test_pos_2035 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2035, colour = "Intervention")) +
  ggtitle("2035")
first_test_pos_2040 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2040, colour = "Intervention")) +
  ggtitle("2040")
first_test_pos_2045 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2045, colour = "Intervention")) +
  ggtitle("2045")
first_test_pos_2050 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2050, colour = "Intervention")) +
  ggtitle("2050")
first_test_pos_2055 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2055, colour = "Intervention")) +
  ggtitle("2055")
first_test_pos_2060 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2060, colour = "Intervention")) +
  ggtitle("2060")
first_test_pos_2065 <- first_test_pos_baseline + 
  geom_line(aes(years, FirstHIVtestsPos_2065, colour = "Intervention")) +
  ggtitle("2065")

## number of births with hivb in  grid 
grid.arrange(first_test_pos_2025, first_test_pos_2030, first_test_pos_2035, 
             first_test_pos_2040, first_test_pos_2045, first_test_pos_2050, 
             first_test_pos_2055, first_test_pos_2060, first_test_pos_2065, 
             top="First HIV test positive")

#### Cumulative outputs ####

### Make a new data frame for each year that testing is stopped ###

cum_outputs <- data.frame(five_years,
                          Cum_NewHIV = rep(NA, 10),
                          Cum_TotHIV = rep(NA, 10), 
                          Cum_TotHIVtests = rep(NA, 10), 
                          Cum_lLYlostAIDS = rep(NA, 10),
                          Cum_AIDSdeathsAdult = rep(NA, 10), 
                          Cum_DiagnosedHIV = rep(NA, 10),
                          Cum_UndiagnosedHIV = rep(NA, 10), 
                          Cum_StartingART = rep(NA, 10),
                          Cum_BirthsHIV = rep(NA, 10))
cum_outputs$five_years <- rev(cum_outputs$five_years)
                          
### make df with only outputs from 2020 to 2070

truncated_outputs <- outputs_df[36:86,]

### calculate total over that time period 

sum_of_columns <- colSums(truncated_outputs)

### add to df
truncated_outputs[nrow(truncated_outputs) + 1, ] <- sum_of_columns

## drop years column 
truncated_outputs <- truncated_outputs %>% select(-years)

# reset the index numbers
row.names(truncated_outputs) <- NULL

# assign cumulative values to new cum_outputs df
for (i in 1:10){
  cum_outputs$Cum_NewHIV[i] <- truncated_outputs[52,i]
  cum_outputs$Cum_TotHIV[i] <- truncated_outputs[52,i+10]
  cum_outputs$Cum_TotHIVtests[i] <- truncated_outputs[52, i+20]
  cum_outputs$Cum_lLYlostAIDS[i] <- truncated_outputs[52, i+30]
  cum_outputs$Cum_AIDSdeathsAdult[i] <- truncated_outputs[52, i+160]
  cum_outputs$Cum_DiagnosedHIV[i] <- truncated_outputs[52, i+170]
  cum_outputs$Cum_UndiagnosedHIV[i] <- truncated_outputs[52, i +180]
  cum_outputs$Cum_StartingART[i] <- truncated_outputs[52, i+120]
  cum_outputs$Cum_BirthsHIV[i] <- truncated_outputs[52, i+140]
}

cum_outputs

cum_new_hiv <- ggplot(cum_outputs, aes(five_years, Cum_NewHIV)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("New HIV Cases") +
  ggtitle("Cumulative HIV incidence") +
  theme_bw() # + ylim(0, 8e+06)

cum_tothiv <- ggplot(cum_outputs, aes(five_years, Cum_TotHIV)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Total HIV cases") +
  ggtitle("Cumulative total HIV") +
  theme_bw() # + ylim(0, 4.1e+08)

cum_tothivtests <- ggplot(cum_outputs, aes(five_years, Cum_TotHIVtests)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("HIV tests") +
  ggtitle("Cumulative HIV tests") +
  theme_bw()

cum_lylost <- ggplot(cum_outputs, aes(five_years, Cum_lLYlostAIDS)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Life years lost") +
  ggtitle("Cumulative life years lost") +
  theme_bw() # + ylim(0, 9e+07)

cum_deaths <- ggplot(cum_outputs, aes(five_years, Cum_AIDSdeathsAdult)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("AIDS related deaths") +
  ggtitle("Cumulative AIDS related deaths") +
  theme_bw() # + ylim(0, 2800000)

cum_diagnosed <- ggplot(cum_outputs, aes(five_years, Cum_DiagnosedHIV)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("HIV diagnoses") +
  ggtitle("Cumulative HIV diagnoses") +
  theme_bw() # + ylim(0, 4e+08)

cum_undiagnosed <- ggplot(cum_outputs, aes(five_years, Cum_UndiagnosedHIV)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Undiagnosed HIV cases") +
  ggtitle("Cumulative undiagnosed HIV cases") +
  theme_bw() #+ ylim(0, 4e+07)

cum_starting_art <- ggplot(cum_outputs, aes(five_years, Cum_StartingART)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Number starting ART") +
  ggtitle("Cumulative number starting ART") +
  theme_bw() #+ ylim(0, 8e+06)

cum_births_hiv <- ggplot(cum_outputs, aes(five_years, Cum_BirthsHIV)) +
  geom_point() +
  xlab("Implementation Year") +
  ylab("Infants born with HIV") +
  ggtitle("Cumulative infants born with HIV") +
  theme_bw() # + ylim(0, 8e+06)

grid.arrange(cum_new_hiv, cum_deaths, cum_lylost, cum_tothiv,
            cum_births_hiv, cum_undiagnosed, cum_diagnosed, 
            cum_tothivtests,  cum_starting_art, 
            top = "Cumulative indicators")

#### Calculate fold change of cumulative indicators ####

fold_chg <- data.frame(years = c(2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065),
                          fc_newhiv = rep(NA, 9),
                          fc_tothiv = rep(NA, 9), 
                          fc_tothivtests = rep(NA, 9), 
                          fc_lylostaids = rep(NA, 9),
                          fc_aidsdeathsadult = rep(NA, 9), 
                          fc_diagnosed = rep(NA, 9),
                          fc_undiagnosed = rep(NA, 9), 
                          fc_starting_art = rep(NA, 9),
                          fc_births_hiv = rep(NA, 9))

for (i in 1:9){
  fold_chg$fc_newhiv[i] <- cum_outputs$Cum_NewHIV[i+1] / cum_outputs$Cum_NewHIV[1]
  fold_chg$fc_tothiv[i] <- cum_outputs$Cum_TotHIV[i+1] / cum_outputs$Cum_TotHIV[1]
  fold_chg$fc_tothivtests[i] <- cum_outputs$Cum_TotHIVtests[i+1] / cum_outputs$Cum_TotHIVtests[1]
  fold_chg$fc_lylostaids[i] <- cum_outputs$Cum_lLYlostAIDS[i+1] / cum_outputs$Cum_lLYlostAIDS[1]
  fold_chg$fc_aidsdeathsadult[i] <- cum_outputs$Cum_AIDSdeathsAdult[i+1] / cum_outputs$Cum_AIDSdeathsAdult[1]
  fold_chg$fc_diagnosed[i] <- cum_outputs$Cum_DiagnosedHIV[i+1] / cum_outputs$Cum_DiagnosedHIV[1]
  fold_chg$fc_undiagnosed[i] <- cum_outputs$Cum_UndiagnosedHIV[i+1] / cum_outputs$Cum_UndiagnosedHIV[1]
  fold_chg$fc_starting_art[i] <- cum_outputs$Cum_StartingART[i+1] / cum_outputs$Cum_StartingART[1]
  fold_chg$fc_births_hiv[i] <- cum_outputs$Cum_BirthsHIV[i+1] / cum_outputs$Cum_BirthsHIV[1]
}

fc_newhiv <- ggplot(fold_chg, aes(years, fc_newhiv)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in HIV incidence") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 1.5)

fc_tothiv <- ggplot(fold_chg, aes(years, fc_tothiv)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in total HIV") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 1.5)

fc_tothivtests <- ggplot(fold_chg, aes(years, fc_tothivtests)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in HIV tests") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0, 1.5)

fc_lylostaids <- ggplot(fold_chg, aes(years, fc_lylostaids)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in life years lost") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 1.5)

fc_aidsdeathsadult <- ggplot(fold_chg, aes(years, fc_aidsdeathsadult)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in AIDS related deaths") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 1.5)

fc_diagnosed <- ggplot(fold_chg, aes(years, fc_diagnosed)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in diagnosed HIV cases") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 1.5)

fc_undiagnosed <- ggplot(fold_chg, aes(years, fc_undiagnosed)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in undiagnosed HIV cases") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 3)

fc_starting_art <- ggplot(fold_chg, aes(years, fc_starting_art)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in people starting ART") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 1.5)

fc_births_hiv <- ggplot(fold_chg, aes(years, fc_births_hiv)) + 
  geom_point() +
  xlab("Implementation Year") +
  ylab("Fold change") +
  ggtitle("Fold change in infants born with HIV") +
  geom_line(aes(y=1), linetype = 3) +
  theme_bw() + ylim(0.5, 1.5)


grid.arrange(fc_newhiv, fc_tothiv, fc_aidsdeathsadult, 
             fc_lylostaids, fc_births_hiv, fc_starting_art,
             fc_diagnosed, fc_undiagnosed, fc_tothivtests, 
             top = "Fold change in cumulative indicators")
