#### Load packages/functions ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

source("modify_rollout_cluster.R")
source("read_and_run_cluster.R")


check_art_retention <- function(pitc_reduction_years, 
                           pitc_reduction_percentage, 
               condom_usage_reduction, 
               fsw_condom_usage_decrease, 
               st_condom_usage_decrease, 
               lt_condom_usage_decrease,
               condom_incr_start,
               art_coverage_increase,
               art_interrupt_rate_decrease, 
               art_incr_start,
               art_coverage_decrease,
               art_interrupt_rate_increase,
               art_decr_start,
               cumulative_years, summary_name = "summary"){

 
  #### Make empty dataframe for outputs ####
  
  # names of all the outputs of interest
  
  output_names <- c("TotalHIVtests", "NewAdultHIV",
                    "AIDSdeathsAdultM", "AIDSdeathsAdultF", "TotSexActs",
                    "SWsexActs", "TotProtSexActs", "SWsexActsProt", "HIVinc15to49", 
                    "ARTcoverageAdult", "AdultARTinterrupters", "AdultInterruptPropn", "TotalART15F", "TotalART15M", 
                    "ARTcoverageDiag", "DiagnosedHIV_M", "DiagnosedHIV_F")
  
  # create empty folder for results
  
  dir.create("results", FALSE, TRUE)
  
  # model input parameters 
  pitc_reduction_years <- pitc_reduction_years # establish years PITC changes occur
  pitc_reduction_percentage <- pitc_reduction_percentage # percentages of pitc that is reduced
  condom_usage_reduction <- condom_usage_reduction # switch for reducing condom usage
  fsw_condom_usage_decrease <- fsw_condom_usage_decrease # proportion of previous year's probability that fsw condom usage probability decreases by each year
  st_condom_usage_decrease <- st_condom_usage_decrease # proportion of previous year's probability that st condom usage probability decreases by each year
  lt_condom_usage_decrease <- lt_condom_usage_decrease # proportion of previous year's probability that lt condom usage probability decreases by each year
  condom_incr_years <- seq(condom_incr_start, condom_incr_start+10, 1) # years for which condom usage probabilities are decreased
  condom_maintenance_years <- seq(condom_incr_start+11, 2100, 1) # years reduced condom usage probabilities are maintained after reduction
  art_coverage_increase <- art_coverage_increase # switch for reducing art interruption rate, thereby increasing ART retention
  art_interrupt_rate_decrease <- art_interrupt_rate_decrease # proportion of previous year's rate that art interruption rate decreases by each year
  art_incr_years <- seq(art_incr_start, art_incr_start+10, 1) # years for which art interruption rate increases
  art_coverage_decrease <- art_coverage_decrease # switch for increasing art interruption rates, thereby reducing ART retention
  art_interrupt_rate_increase <- art_interrupt_rate_increase # proportion of previous year's rate that art interruption rate increases
  art_decr_years <- seq(art_decr_start, art_decr_start+10, 1) # years for which art interruption rate decreases
  cumulative_years <- cumulative_years # number of years over which cumulative values are calculated
  if (art_coverage_increase) {
    art_maintenance_years <- seq(2100, 2100, 1)
  }
  if (art_coverage_decrease) {
    art_maintenance_years <- seq(art_decr_start+11, 2100, 1)
  }
  # run baseline model
  baseline <- run_thembisa_scenario_prev_year(pitc_reduction_year = NA,
                                                    condom_usage_reduction = FALSE,
                                                     fsw_condom_usage_decrease = 0,
                                                     st_condom_usage_decrease = 0,
                                                     lt_condom_usage_decrease = 0,
                                                     condom_incr_years = NA,
                                                     condom_maintenance_years = condom_maintenance_years,
                                                    art_coverage_increase = FALSE,
                                                    art_coverage_decrease = FALSE,
                                                    art_interrupt_rate_decrease = NA,
                                                    art_incr_years = art_incr_years,
                                                    art_interrupt_rate_increase = art_interrupt_rate_increase,
                                                    art_decr_years = art_decr_years,
                                                    art_maintenance_years = art_maintenance_years,
                                                    output_names = output_names, 
                                                    base_rate_reduction = base_rate_reduction)
  
  # save baseline outputs
  assign("baseline", baseline)
  
  # create matrix for all pitc reduction years and reduction percentages
  name_matrix <- expand_grid(pitc_reduction_years, pitc_reduction_percentage)
  # combine each year with each percentage to create unique scenario names
  scenario_names <- purrr::pmap_chr(name_matrix, paste, sep = "_")
  # count scenario names
  n_scenarios <- length(scenario_names)
  # make a list with empty tibbles for each scenario
  scenarios <- replicate(n_scenarios, tibble())
  
  
  # for loop that changes testing rate at different years and saves outputs
  # predefined condom reduction and art interruption rate changes are used
  for (percentage_value in pitc_reduction_percentage){
    base_rate_reduction <- percentage_value/100
    for (year in pitc_reduction_years){
      pitc_reduction_year <- year
      one_scenario <- run_thembisa_scenario_prev_year(pitc_reduction_year = pitc_reduction_year,
                                                      condom_usage_reduction = condom_usage_reduction,
                                                      fsw_condom_usage_decrease = fsw_condom_usage_decrease,
                                                      st_condom_usage_decrease = st_condom_usage_decrease,
                                                      lt_condom_usage_decrease = lt_condom_usage_decrease,
                                                      condom_incr_years = condom_incr_years,
                                                      condom_maintenance_years = condom_maintenance_years,
                                                      art_interrupt_rate_decrease = art_interrupt_rate_decrease,
                                                      art_incr_years = art_incr_years,
                                                      art_coverage_increase = art_coverage_increase,
                                                      art_coverage_decrease = art_coverage_decrease,
                                                      art_interrupt_rate_increase = art_interrupt_rate_increase,
                                                      art_decr_years = art_decr_years,
                                                      art_maintenance_years = art_maintenance_years,
                                                      output_names = output_names,
                                                      base_rate_reduction = base_rate_reduction)
      name <- paste(year, percentage_value, sep = "_")
      scenarios[[which(scenario_names == name)]] <- one_scenario # save df for one scenario
    }
  }
  
  # make a new data frame joining all results
  df <- read_thembisa_results_cluster(pitc_reduction_years, pitc_reduction_percentage, 
                                      scenarios = scenarios, scenario_names = scenario_names, 
                                      baseline = baseline)

  # calculate total aids-related deaths

  df <- df %>%
    pivot_wider(names_from = indicator) %>%
    mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
    mutate(TotalART15 = TotalART15F + TotalART15M) %>% 
    mutate(TotalDiagnosedHIV = DiagnosedHIV_F + DiagnosedHIV_M) %>% 
    mutate(ARTCoverage_lise = TotalART15/TotalDiagnosedHIV) %>% 
    mutate(art_interrupt_rate = AdultARTinterrupters / TotalART15) %>% 
    pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")

  # calculate condom usage for total adults and fsw-client only
  df <- df %>%
    pivot_wider(names_from = indicator) %>%
    mutate(CondomUsage = ((TotProtSexActs/TotSexActs)*100)) %>%
    mutate(FSWCondomUsage = ((SWsexActsProt/SWsexActs)*100)) %>%
    pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")

  df$test_reduction <- 100 - as.integer(df$test_reduction)

  # make test_reduction a factor
  df$test_reduction <- as.factor(df$test_reduction)

  # saving a summary csv of all outputs
  summary <- as.data.frame(df) %>% 
    filter(year >= 2010) %>% 
    dplyr::group_by(year, scenario, indicator, pitc_reduction_year, test_reduction) %>%
    dplyr::summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
              lower_CI = quantile(value, probs = 0.025))

  write_csv(summary, paste0("results/", summary_name, ".csv"))

  # cumulative values
  cumulative_years <- cumulative_years
  cumulative_values <- calc_all_cumulatives(pitc_reduction_years, cumulative_years, df = df)

  # calculate cumulative percent change from baseline

  cumulative_values <- cumulative_values %>%
    pivot_wider(names_from = scenario, values_from = cumulative) %>%
    mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
    mutate(absolute_dif = intervention - baseline) %>%
    pivot_longer(-(indicator:test_reduction), names_to = "scenario")

  # Plotting cumulative HIV infections over 40 year
  cumulative_summary <- cumulative_values %>%
    group_by(indicator, pitc_reduction_year, test_reduction, scenario) %>%
    summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
              lower_CI = quantile(value, probs = 0.025))

  write_csv(cumulative_summary, paste0("results/cumulative_", summary_name, ".csv"))
}

system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa.exe -O2")

check_art_retention(pitc_reduction_years = c(2025), 
                    pitc_reduction_percentage = c(100),
                    condom_usage_reduction = FALSE, 
                    fsw_condom_usage_decrease = (0.07/3),
                    st_condom_usage_decrease = 0.07, 
                    lt_condom_usage_decrease = 0.07,
                    condom_incr_start = 2025,
                    art_coverage_increase = TRUE,
                    art_interrupt_rate_decrease = 0.8,
                    art_incr_start = 2020,
                    summary_name = "check_art_retention",
                    cumulative_years = 50,
                    art_coverage_decrease = FALSE,
                    art_interrupt_rate_increase = 2/100,
                    art_decr_start = 2025)

lise_art_retention <- read_csv("results/check_art_retention.csv")

lise_art_retention %>% filter(indicator == "art_interrupt_rate", scenario == "baseline") %>% ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.9, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  theme_classic() +
  expand_limits(y = 0) +
  geom_vline(aes(xintercept = 2020), lty = "dotted") + 
  scale_y_continuous("Proportion of adults on ART who interrupt ART")
  
lise_art_retention %>% filter(indicator == "ARTcoverageDiag", scenario == "baseline" , year < 2030) %>% ggplot(aes(year, mean)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.9, show.legend = F) +
  geom_line(aes()) +
  xlab("Years") +
  theme_classic() +
  expand_limits(y = 0) +
  geom_hline(aes(yintercept = 0.95), lty = "dotted") +
  geom_vline(aes(xintercept = 2025), lty = "dotted") + 
  scale_y_continuous("ART coverage")

  
  
  
  
  