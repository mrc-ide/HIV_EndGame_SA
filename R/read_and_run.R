library(dplyr)
library(ggplot2)
library(gridExtra)

# Compiles and runs the Thembisa model
run_thembisa <- function(){
  setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA/THEMBISAv18")
  system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")
  system("./thembisa")
  setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA")
}

# Read output from Thembisa and assign column headers
read_output <- function(output_name){
  # Import output txt
  output_txt <- paste(output_name, "txt", sep = ".")
  output_txt <- paste("THEMBISAv18", output_txt, sep = "/")
  output <- read.delim(output_txt, header=FALSE, row.names = 1)
  names(output)[2:87] <- seq(1985, 2070)
  # names(output)[1] <- "Simulation"
  output <- output %>% select(-V2) 
  t_output <- as.data.frame(t(output))
  output <- as.data.frame(as_tibble(t_output, rownames = "Year"))
  output$Year <- as.numeric(output$Year)
  names(output)[2] <- "Simulation_1"
  return(output$Simulation_1)
}

edit_formatted_data <- function(parameter_name, new_values, starting_year=1985, final_year=2070){
  # select parameter using dictionary
  parameter <- formatted_data$data[,which(dictionary$name == parameter_name)]
  # Edit value
  for (i in ((starting_year+1)-1985):((final_year+1)-1985)){
    parameter[i] <- new_values
  }
  # reassign to formatted data
  parameter -> formatted_data$data[,which(dictionary$name == parameter_name)]
  return(formatted_data)
}


## create outputs_df 
## output_names needs to be a vector/list of outputs as characters

create_outputs_df <- function(output_parameter_names){
  years <- formatted_data[['year']]
  ## Make dataframe with same number of rows as years
  outputs_df <- data.frame(years)
  # calculate number of scenarios if there are 10 per output
  
  n_outputs <- length(output_parameter_names)
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
      col_names[((i+1)+(i*9)-j)+1] <- paste(output_parameter_names[i], five_years[j], sep = "_")
    }
  }
  # assigns scenario names to columns in dataframe
  for (i in 2:n_scenarios){
    names(outputs_df)[i] <- col_names[i]
  }
  return(outputs_df)
}

save_baseline_outputs <- function(output_parameter_names = output_names, df = outputs_df){
  #### Outputs with baseline testing rates ####
  ## read in input parameter file
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  n_outputs <- length(output_names)
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  ## compile and model
  run_thembisa()
  ## Read all baseline outputs to columns 
  # this gives a warning but it works
  for (i in 1:n_outputs-1){
    outputs_df[2] <- read_output(output_names[1])
    outputs_df[2+i*10] <- read_output(output_names[i+1])
  }
  return(outputs_df)
}

save_baseline_medians <- function(output_parameter_names = output_names, df = outputs_df){
  #### Outputs with baseline testing rates ####
  ## read in input parameter file
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  n_outputs <- length(output_names)
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  ## compile and model
  run_thembisa()
  ## Read all baseline outputs to columns 
  # this gives a warning but it works
  for (i in 1:n_outputs-1){
    outputs_df[2] <- select_median(output_names[1])
    outputs_df[2+i*10] <- select_median(output_names[i+1])
  }
  return(outputs_df)
}

change_testing_rate <- function(output_parameter_names = output_names, df = outputs_df){
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
    n_outputs <- length(output_names)
    for (j in 1:n_outputs-1){
      outputs_df[2+i] <- select_median(output_names[1])
      outputs_df[2+j*10+i] <- select_median(output_names[i+1])
    }
  }
  return(outputs_df)
}  

make_long_df <- function(df_csv_name = "outputs_df.csv"){
  
  #### changing format of df to be used with facet_wrap ####
  
  df <- read_csv(df_csv_name)
  
  df_long <- df %>%
    pivot_longer(-years,
                 names_to = c("indicator", "intervention_year"),
                 names_pattern = "(.+)_(baseline|20[0-9]{2})")
  
  #' create a 'baseline' entry for each intervention year
  #' 
  
  df_long_facet <- df_long %>%
    pivot_wider(names_from = "intervention_year", values_from = "value") %>%
    pivot_longer(`2025`:last_col(), names_to = "intervention_year", values_to = "intervention") %>%
    pivot_longer(c(baseline, intervention), names_to = "scenario")
  
  return(df_long_facet)
}

# Plots the outputs over time 
plot_outputs <- function(output_name, title_of_plot = output_name, value=value, ylab) {
  df %>%
    filter(
      indicator == output_name,
      year >= 2020
    ) %>%
    ggplot(aes(year, value, color = scenario)) +
    geom_line() + ggtitle(title_of_plot) +
    xlab("Years") + ylab(ylab) +
    facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw() -> plot
  return(plot)
}

plot_pct_trend <- function(df=intervention_only, output_name, title_of_plot = output_name, 
                           percent_change = percent_change) {
  df %>%
    filter(
      indicator == output_name,
      year >= 2020
    ) %>%
    ggplot(aes(year, percent_change)) +
    geom_line() + ggtitle(title_of_plot) + ylab("Change from baseline (%)") +
    facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()
}

# adapted read_output to handle multiple parameter sets and add median to outputs_df

select_median <- function(output_name){
  output_txt <- paste(output_name, "txt", sep = ".")
  output_txt <- paste("THEMBISAv18", output_txt, sep = "/")
  output <- read.delim(output_txt, header=FALSE, row.names = 1)
  names(output)[2:87] <- seq(1985, 2070)
  output <- output %>% select(-V2)
  t_output <- t(output)
  output_new <- as.data.frame(as_tibble(t_output))
  output_new$median <- rep(NA, 86)
  for (i in 1:86){
    output_new$median[i] <- median(t_output[i,])
  }
  return(output_new$median)
}

read_thembisa_output <- function(output_name){
  output_txt <- paste(output_name, "txt", sep = ".")
  output_txt <- paste("THEMBISAv18", output_txt, sep = "/")
  output <- read.delim(output_txt, header=FALSE, row.names = 1)
  output <- output %>% select(-V2)
  t_output <- t(output)
  output_new <- as.data.frame(as_tibble(t_output))
  names(output_new) <- seq_along(output_new)
  output_new$year <- seq(1985, 2070)
  pivot_longer(output_new, -year, names_to = "parameter_set")
}


read_thembisa_scenario <- function(output_names){
  temp <- lapply(output_names, read_thembisa_output)
  names(temp) <- output_names
  bind_rows(temp, .id = "indicator")
}

run_thembisa_scenario <- function(intervention_year, output_names){
  ## read in input parameter file
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  ## write unedited input parameter file
  formatted_data <- format_data(data, dictionary)
  if (!is.na(intervention_year)){
    formatted_data <- edit_formatted_data("rate_first_test_neg_fem_under_25", 
                                          new_values = 0.0, 
                                          starting_year = intervention_year)
  }
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  ## compile and model
  run_thembisa()
  read_thembisa_scenario(output_names)
}

# dir.create("results", FALSE, TRUE)
# intervention_years <- seq(2025, 2050, 5)
# baseline <- run_thembisa_scenario(NA, output_names)
# write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for (intervention_year in intervention_years){
#   temp <- run_thembisa_scenario(intervention_year, output_names)
#   write.csv(temp, paste0("results/scenario_", intervention_year, ".csv"),
#             row.names = FALSE)
# }

read_thembisa_results <- function(intervention_years){
  filepaths <- paste0("results/scenario_", intervention_years, ".csv")
  temp <- lapply(filepaths, read.csv)
  names(temp) <- intervention_years
  baseline <- read.csv("results/baseline.csv")
  bind_rows(temp, .id = "intervention_year") %>% 
    dplyr::rename(intervention = value) %>% 
    dplyr::left_join(baseline) %>% 
    dplyr::rename(baseline = value) %>% 
    tidyr::pivot_longer(c(intervention, baseline), names_to = "scenario")
}

# df <- read_thembisa_results(intervention_years)

plot_outputs_with_uncertainty <- function(output_name){
  df %>% filter(
    scenario != "percent_change",
    indicator == output_name,
    year >= 2020) %>% 
    group_by(year, scenario, intervention_year) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(year, median, fill = scenario)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, group = scenario), alpha = 0.25) +
    geom_line(aes(color = scenario)) +
    ggtitle(output_name) +
    xlab("Years") +
    facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()
}

plot_pct_chg_uncertainty <- function(output_name){
  df %>% 
    filter(scenario == "percent_change",
           indicator == output_name, 
           year >= 2020) %>% 
    group_by(year, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(year, median)) +
    geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.25, fill = "#00BFC4") +
    geom_line(aes(), colour = "#00BFC4") +
    ggtitle(output_name) +
    xlab("Years") + ylab("Change from baseline (%)") +
    facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()
}

calc_cumulative <- function(year, follow_up_years){
  end_year <- year + follow_up_years
  df %>%
    filter(
      indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
      indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
      indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
      indicator != "TotalART15M", indicator != "TotalART15F",
      indicator != "TotalARTAdult", scenario != "percent_change", 
      indicator!= "TotalDiagnosedHIV",
      year >= year,
      year <= end_year,
      intervention_year == year
    )  %>% 
    group_by(indicator, intervention_year, scenario, parameter_set) %>% 
    summarise(cumulative = sum(value)) %>% 
    filter(intervention_year == year)
}

calc_all_cumulatives <- function(intervention_years, follow_up_years){
  cumulatives <- lapply(intervention_years, calc_cumulative, follow_up_years)
  names(cumulatives) <- intervention_years
  all_cumulatives <- bind_rows(cumulatives, .id = "intervention_year")
}


plot_cumulative_uncertainty <- function(){
  cumulative_values %>% filter(
    scenario != "percent_change",
    indicator != "LifeYrsSaved",
    indicator != "TestEfficiency", 
    indicator != "Pct1stHIVTestPos",
    indicator != "PctANCTestPos") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, fill = scenario)) +
    geom_point(aes(color = scenario), shape = 15) +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
    xlab("Years") + ylab("Median (IQR)") +
    facet_wrap(~indicator, scales = "free_y", 
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related mortality",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses"
                 ))) + 
    expand_limits(y=0) + theme_bw()
}
plot_cumulative_pct_chg <- function(){
  cumulative_values %>% filter(
    scenario == "percent_change",
    indicator != "LifeYrsSaved",
    indicator != "TestEfficiency", 
    indicator != "Pct1stHIVTestPos",
    indicator != "PctANCTestPos") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, fill = scenario)) +
    geom_point(aes(), shape = 15, color = , fill = "#00BFC4") +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "#00BFC4") +
    xlab("Years") + ylab("Median change from baseline (%)") +
    facet_wrap(~indicator, scales = "free_y",  
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related mortality",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses"))) +
    expand_limits(y=0) + theme_bw()
}

plot_cumulative_epi_uncertainty <- function(indicators){
  cumulative_values %>% filter(
    scenario != "percent_change", 
    indicator == "TotalNewHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult" | 
    indicator == "TotalHIVtests") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, fill = scenario)) +
    geom_point(aes(color = scenario), shape = 15) +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
    xlab("Years") + ylab("Median (IQR)") +
    facet_wrap(~indicator, scales = "free_y", 
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related mortality",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses"))) + 
    expand_limits(y=0) + theme_bw()
}

plot_cumulative_epi_pct_chg <- function(){
  cumulative_values %>% filter(
    scenario == "percent_change", 
    indicator == "TotalNewHIV" | 
      indicator == "LYlostAIDS" | 
      indicator =="TotalAIDSdeathsadult" | 
      indicator == "TotalHIVtests") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median)) +
    geom_point(shape = 15, color = "#00BFC4") +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "#00BFC4") +
    xlab("Years") + ylab("Median change from baseline (%)") +
    facet_wrap(~indicator, scales = "free_y",  
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related mortality",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses"))) +
    expand_limits(y=0) + theme_bw()
}

plot_cumulative_surv_uncertainty <- function(indicators){
  cumulative_values %>% filter(
    scenario != "percent_change", 
    indicator == "Number1stHIVtestsPos" | 
      indicator == "NewDiagnosesPregnancy" | 
      indicator =="StartingARTtot" | 
      indicator == "TotANCtests") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, fill = scenario)) +
    geom_point(aes(color = scenario), shape = 15) +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
    xlab("Years") + ylab("Median (IQR)") +
    facet_wrap(~indicator, scales = "free_y", 
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related mortality",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses"))) + 
    expand_limits(y=0) + theme_bw()
}


plot_cumulative_surv_pct_chg <- function(){
  cumulative_values %>% filter(
    scenario == "percent_change", 
    indicator == "Number1stHIVtestsPos" | 
      indicator == "NewDiagnosesPregnancy" | 
      indicator =="StartingARTtot" | 
      indicator == "TotANCtests") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median)) +
    geom_point(shape = 15, color = "#00BFC4") +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "#00BFC4") +
    xlab("Years") + ylab("Median change from baseline (%)") +
    facet_wrap(~indicator, scales = "free_y",  
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related mortality",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses"))) +
    expand_limits(y=0) + theme_bw()
}
