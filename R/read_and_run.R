library(dplyr)
library(ggplot2)
library(gridExtra)

# Compiles and runs the Thembisa model
run_thembisa <- function(){
  setwd("/Users/stefan/Documents/HIV_EndGame_SA/THEMBISAv18")
  system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")
  system("./thembisa")
  setwd("/Users/stefan/Documents/HIV_EndGame_SA")
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

edit_formatted_data_incremental <- function(parameter_name, new_values, starting_year=1985, final_year=2070){
  # select parameter using dictionary
  parameter <- formatted_data$data[,which(dictionary$name == parameter_name)]
  # Edit value
  for (i in ((starting_year+1)-1985):((starting_year+1)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.2)
  }
  for (i in ((starting_year+2)-1985):((starting_year+2)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.4)
  }
  for (i in ((starting_year+3)-1985):((starting_year+3)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.6)
  }
  for (i in ((starting_year+4)-1985):((starting_year+4)-1985)){
    parameter[i] <- 0.2877 - ((0.2877 - new_values) * 0.8)
  }
  for (i in ((starting_year+5)-1985):((final_year+1)-1985)){
    parameter[i] <- new_values
  }
  # reassign to formatted data
  parameter -> formatted_data$data[,which(dictionary$name == parameter_name)]
  return(formatted_data)
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
    formatted_data <- edit_formatted_data_incremental("rate_first_test_neg_fem_under_25", 
                                          new_values = 0.0, 
                                          starting_year = intervention_year)
  }
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  ## compile and model
  run_thembisa()
  read_thembisa_scenario(output_names)
}

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
    facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12))
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

calc_cumulative <- function(start_year, follow_up_years){
  end_year <- start_year + follow_up_years
  df %>%
    filter(
      indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
      indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
      indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
      indicator != "TotalART15M", indicator != "TotalART15F",
      indicator != "TotalARTAdult", scenario != "percent_change", 
      indicator != "TotalDiagnosedHIV", indicator!= "ARTinititationRatio", 
      indicator != "ARTInitPerNewInfection",
      year >= start_year,
      year <= end_year,
      intervention_year == start_year
    )  %>% 
    group_by(indicator, intervention_year, scenario, parameter_set) %>% 
    summarise(cumulative = sum(value))
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
    geom_point(aes(), shape = 15 , color = "#00BFC4") +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "#00BFC4") +
    xlab("Years") + ylab("Change from baseline (%)") +
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

plot_cumulative_epi_uncertainty <- function(df){
    df %>% filter(
    scenario != "percent_change", 
    indicator == "NewAdultHIV" | 
    indicator == "LYlostAIDS" | 
    indicator =="TotalAIDSdeathsadult") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, fill = scenario)) +
    geom_point(aes(color = scenario), shape = 15) +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
    xlab("Years") + ylab("Cumulative value") +
    facet_wrap(~indicator, scales = "free_y", 
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related deaths in adults",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses",
                 "NewAdultHIV" = "New HIV infections in adults")),nrow = 3) + 
    expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12))
}

plot_cumulative_epi_pct_chg <- function(df){
  df %>% filter(
    scenario == "percent_change", 
    indicator == "NewAdultHIV" | 
      indicator == "LYlostAIDS" | 
      indicator =="TotalAIDSdeathsadult") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, fill = scenario)) +
    geom_point(shape = 15, color = "#00BFC4") +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "#00BFC4") +
    xlab("Years") + ylab("Change from baseline (%)") +
    facet_wrap(~indicator, scales = "free_y",  
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "First-time diagnoses at ANC",
                 "Number1stHIVtestsPos" = "First-time diagnoses",
                 "RediagnosesPregnancy" = "Re-diagnoses at ANC",
                 "StartingARTtot" = "ART inititation",
                 "TotalAIDSdeathsadult" = "AIDS-related deaths in adults",
                 "TotalHIVtests" = "Total HIV tests",
                 "TotalNewHIV" = "New HIV infections",
                 "TotANCtests" = "HIV tests at ANC",
                 "ARTinititationRatio" = "ART initation : New diagnoses",
                 "ARTInitPerNewInfection" = "ART initation : New infections",
                 "NewANCDiagPerInfection" = "New ANC diagnosis : New infections",
                 "NewDiagPerInfection" = "New diagnosis : New infections",
                 "Pct1stHIVTestPos" = "Percentage first test positive",
                 "PctANCTestPos" = "Percentage first test positive at ANC",
                 "TotalARTratio" = "Total ART : Total HIV diagnoses",
                 "NewAdultHIV" = "New HIV infections in adults")),nrow = 3) +
    expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12))
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
    xlab("Years") + ylab("Change from baseline (%)") +
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

plot_cumulative_diag_uncertainty <- function(df){
    df %>% filter(
    scenario != "percent_change", 
    indicator == "Number1stHIVtestsPos" | 
    indicator == "NewDiagnosesPregnancy") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, fill = scenario)) +
    geom_point(aes(color = scenario), shape = 15) +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI, color = scenario), width = 0.1) +
    xlab("Years") + ylab("New HIV diagnoses") +
    facet_wrap(~indicator, scales = "free_y", 
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "New HIV diagnoses at ANC",
                 "Number1stHIVtestsPos" = "New HIV diagnoses in adults",
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
                 "TotalARTratio" = "Total ART : Total HIV diagnoses")), nrow = 2) + 
    expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12))
}


plot_cumulative_diag_pct_chg <- function(df){
    df %>% filter(
    scenario == "percent_change", 
    indicator == "Number1stHIVtestsPos" | 
      indicator == "NewDiagnosesPregnancy") %>% 
    group_by(indicator, intervention_year, scenario) %>% 
    summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
              lower_CI = quantile(value, probs = 0.025)) %>% 
    ggplot(aes(intervention_year, median, color = scenario)) +
    geom_point(shape = 15, color = "#00BFC4") +
    geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1, color = "#00BFC4") +
    xlab("Years") + ylab("Change from baseline (%)") +
    facet_wrap(~indicator, scales = "free_y",  
               labeller = labeller(indicator = c(
                 "LYlostAIDS" = "Life-years lost to AIDS", 
                 "NewDiagnosesPregnancy" = "New HIV diagnoses at ANC",
                 "Number1stHIVtestsPos" = "New HIV diagnoses in adults",
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
                 "TotalARTratio" = "Total ART : Total HIV diagnoses")), nrow =2) +
    expand_limits(y=0) + theme_bw() + theme(text = element_text(size = 12))
}
