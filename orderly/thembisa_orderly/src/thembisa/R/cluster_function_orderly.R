
run_on_cluster <- function(pitc_reduction_years, 
                           pitc_reduction_percentage,
                           condom_usage_reduction,
                           condom_usage_decrease,
                           condom_decr_start,
                           condom_usage_promotion,
                           condom_usage_increase,
                           condom_incr_start,
                           art_coverage_increase,
                           art_interrupt_rate_decrease,
                           art_incr_start,
                           art_coverage_decrease,
                           art_interrupt_rate_increase,
                           art_decr_start,
                           cumulative_years,
                           summary_name){

 
  #### Make empty dataframe for outputs ####
  
  # names of all the outputs of interest
  
  output_names <- c("MarriedF17to49", "MarriedM17to49", 
                    "MalesOver15", "FemalesOver15", "TotProtSexActs",
                    "TotSexActs", "SWsexActsProt", "SWsexActs", "HIVinc15to49", 
                    "NewAdultHIV")
  
  # create empty folder for results
  
  dir.create("results", FALSE, TRUE)
  
  # model input parameters 
  pitc_reduction_years <- pitc_reduction_years # establish years PITC changes occur
  pitc_reduction_percentage <- pitc_reduction_percentage # percentages of pitc that is reduced
  condom_usage_reduction <- condom_usage_reduction # switch for reducing condom usage
  fsw_condom_usage_decrease <- condom_usage_decrease/100 # proportion of previous year's probability that fsw condom usage probability decreases by each year
  st_condom_usage_decrease <- condom_usage_decrease/100 # proportion of previous year's probability that st condom usage probability decreases by each year
  lt_condom_usage_decrease <- condom_usage_decrease/100 # proportion of previous year's probability that lt condom usage probability decreases by each year
  condom_decr_years <- seq(condom_decr_start, condom_decr_start+10, 1) # years for which condom usage probabilities are decreased
  
  condom_usage_promotion <- condom_usage_promotion # switch for increasing condom usage
  fsw_condom_usage_increase <- condom_usage_increase/100 # proportion of previous year's probability that fsw condom usage probability increases by each year
  st_condom_usage_increase <- condom_usage_increase/100 # proportion of previous year's probability that st condom usage probability increases by each year
  lt_condom_usage_increase <- condom_usage_increase/100 # proportion of previous year's probability that lt condom usage probability increases by each year
  condom_incr_years <- seq(condom_incr_start, condom_incr_start+10, 1) # years for which condom usage probabilities are increased

  if (condom_usage_reduction) {
    condom_maintenance_years <- seq(condom_decr_start+11, 2100, 1)
  }
  if (condom_usage_promotion) {
    condom_maintenance_years <- seq(condom_incr_start+11, 2100, 1)
  } 
  
  art_coverage_increase <- art_coverage_increase # switch for reducing art interruption rate, thereby increasing ART retention
  art_interrupt_rate_decrease <- art_interrupt_rate_decrease # proportion of previous year's rate that art interruption rate decreases by each year
  art_incr_years <- seq(art_incr_start, art_incr_start+10, 1) # years for which art interruption rate increases
  art_coverage_decrease <- art_coverage_decrease # switch for increasing art interruption rates, thereby reducing ART retention
  art_interrupt_rate_increase <- art_interrupt_rate_increase # proportion of previous year's rate that art interruption rate increases
  art_decr_years <- seq(art_decr_start, art_decr_start+10, 1) # years for which art interruption rate decreases
  cumulative_years <- cumulative_years # number of years over which cumulative values are calculated
  if (art_coverage_increase) {
    art_maintenance_years <- seq(art_incr_start+11, 2100, 1)
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
                                              condom_decr_years = NA,
                                              condom_usage_promotion = FALSE,
                                              fsw_condom_usage_increase = 0,
                                              st_condom_usage_increase = 0,
                                              lt_condom_usage_increase = 0,
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
                                                      condom_decr_years = condom_decr_years,
                                                      condom_usage_promotion = condom_usage_promotion,
                                                      fsw_condom_usage_increase = fsw_condom_usage_increase,
                                                      st_condom_usage_increase = st_condom_usage_increase,
                                                      lt_condom_usage_increase = lt_condom_usage_increase,
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

  # df <- df %>%
  #   pivot_wider(names_from = indicator) %>%
  #   mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  #   mutate(TestsPerAdult = TotalHIVtests / (MalesOver15 + FemalesOver15))%>%
  #   mutate(AIDS_Mortality = TotalAIDSdeathsadult / (TotHIV15)) %>% 
  #   pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")
  
  

  # calculate condom usage for total adults and fsw-client only
  df <- df %>%
    pivot_wider(names_from = indicator) %>%
    mutate(CondomUsage = ((TotProtSexActs/TotSexActs)*100)) %>%
    mutate(FSWCondomUsage = ((SWsexActsProt/SWsexActs)*100)) %>%
    mutate(prop_married_m = MarriedM17to49/MalesOver15,
           prop_married_f = MarriedF17to49/FemalesOver15) %>% 
    pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")

  df$test_reduction <- 100 - as.integer(df$test_reduction)

  # make test_reduction a factor
  df$test_reduction <- as.factor(df$test_reduction)

  # saving a summary csv of all outputs
  summary <- as.data.frame(df) %>% 
    dplyr::group_by(year, scenario, indicator, pitc_reduction_year, test_reduction) %>%
    dplyr::summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975, na.rm = TRUE),
              lower_CI = quantile(value, probs = 0.025, na.rm = TRUE))  %>% mutate(future_variability = "test_reduction_only", future_value = 0)
  if (condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase & !art_coverage_decrease){
    summary$future_variability <- "condom_reduction"
    summary$future_value <- condom_usage_decrease
  }
  if (condom_usage_promotion & !condom_usage_reduction & !art_coverage_increase & !art_coverage_decrease){
    summary$future_variability <- "condom_promotion"
    summary$future_value <- condom_usage_increase
  }
  if (art_coverage_increase & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_decrease){
    summary$future_variability <-  "art_improvement"
    summary$future_value <- art_interrupt_rate_decrease
  }
  if (art_coverage_decrease & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase){
    summary$future_variability <- "art_deterioration"
    summary$future_value <- art_interrupt_rate_increase
  }
  

  write_csv(summary, paste0("results/", summary_name, ".csv"))

  # # cumulative values
  # cumulative_years <- cumulative_years
  # cumulative_values <- calc_all_cumulatives(pitc_reduction_years, cumulative_years, df = df)
  # 
  # # calculate cumulative percent change from baseline
  # 
  # cumulative_values <- cumulative_values %>%
  #   pivot_wider(names_from = scenario, values_from = cumulative) %>%
  #   mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
  #   mutate(absolute_dif = intervention - baseline) %>%
  #   pivot_longer(-(indicator:test_reduction), names_to = "scenario")
  # 
  # # Plotting cumulative HIV infections over 40 year
  # cumulative_summary <- cumulative_values %>%
  #   group_by(indicator, pitc_reduction_year, test_reduction, scenario) %>%
  #   summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975),
  #             lower_CI = quantile(value, probs = 0.025))
  # 
  # cumulative_summary <- cumulative_summary %>% mutate(future_variability = "test_reduction_only", future_value = 0)
  # if (condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase & !art_coverage_decrease){
  #   cumulative_summary$future_variability <- "condom_reduction"
  #   cumulative_summary$future_value <- condom_usage_decrease
  # }
  # if (condom_usage_promotion & !condom_usage_reduction & !art_coverage_increase & !art_coverage_decrease){
  #   cumulative_summary$future_variability <- "condom_promotion"
  #   cumulative_summary$future_value <- condom_usage_increase
  # }
  # if (art_coverage_increase & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_decrease){
  #   cumulative_summary$future_variability <-  "art_improvement"
  #   cumulative_summary$future_value <- art_interrupt_rate_decrease
  # }
  # if (art_coverage_decrease & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase){
  #   cumulative_summary$future_variability <- "art_deterioration"
  #   cumulative_summary$future_value <- art_interrupt_rate_increase
  # }
  # 
  # 
  # write_csv(cumulative_summary, paste0("results/cumulative_", summary_name, ".csv"))
}



