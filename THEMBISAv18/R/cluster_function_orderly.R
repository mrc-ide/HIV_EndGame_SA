source("R/modify_rollout_orderly.R")
source("R/read_and_run_orderly.R")
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
                           cumulative_years_list,
                           change_mmc,
                           mmc_rel_rate,
                           mmc_change_start,
                           change_prep,
                           prep_rel_rate, 
                           prep_change_start,
                           summary_name){

 
  #### Make empty dataframe for outputs ####
  
  # names of all the outputs of interest
  
  output_names <- c("TotalHIVtests", "NewAdultHIV",
                    "AIDSdeathsAdultM", "AIDSdeathsAdultF", "TotalART15F", "TotalART15M",
                    "TotalART15M2L", "TotalART15F2L", "AdultHIVtestsPos", "AdultHIVtestsNeg",
                    "TotANCtests", "NewDiagnosesPregnancy", "RediagnosesPregnancy",
                    "PreARTunder200M", "PreART200to349M", "PreART350to499M", "PreARTover500M",
                    "PreARTunder200F", "PreART200to349F", "PreART350to499F", "PreARTover500F",
                    "OnARTcurrUnder200", "OnARTcurr200to349", "OnARTcurr350to499", "OnARTcurrOver500",
                    "DiscARTover500", "DiscART350to499", "DiscART200to349", "DiscARTunder200", 
                    "StartingART0", "StartingART1","StartingART1to2", "StartingART3to5", 
                    "StartingART6to9", "StartingART10to14", "TotalART1to2", "TotalART3to5", 
                    "TotalART6to9", "TotalART10to14", "StartingART_M15", "StartingART_F15",
                    "AIDSdeaths0", "AIDSdeaths1to4", "AIDSdeaths5to9","AIDSdeaths10to14",
                    "PaedsHIVtestsPos", "PaedsHIVtestsNeg", "MalesOver15", "FemalesOver15", 
                    "Number1stHIVtestsPos", "TotProtSexActs", "TotSexActs", "SWsexActsProt", "SWsexActs", 
                    "ARTcoverageAdult")
  
  # create empty folder for results
  
  dir.create("results", FALSE, TRUE)
  
  # model input parameters 
  summary_name <- summary_name
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
  art_interrupt_rate_decrease <- art_interrupt_rate_decrease/100 # proportion of previous year's rate that art interruption rate decreases by each year
  art_incr_years <- seq(art_incr_start, art_incr_start+10, 1) # years for which art interruption rate increases
  art_coverage_decrease <- art_coverage_decrease # switch for increasing art interruption rates, thereby reducing ART retention
  art_interrupt_rate_increase <- art_interrupt_rate_increase/100 # proportion of previous year's rate that art interruption rate increases
  art_decr_years <- seq(art_decr_start, art_decr_start+10, 1) # years for which art interruption rate decreases
  cumulative_years_list <- cumulative_years_list # number of years over which cumulative values are calculated
  if (art_coverage_increase) {
    art_maintenance_years <- seq(art_incr_start+11, 2100, 1)
  }
  if (art_coverage_decrease) {
    art_maintenance_years <- seq(art_decr_start+11, 2100, 1)
  }
  change_mmc <- change_mmc
  mmc_rel_rate <- mmc_rel_rate
  mmc_change_years <- mmc_change_start
  change_prep <- change_prep
  prep_rel_rate <- prep_rel_rate 
  prep_change_years <- prep_change_start
  
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
                                              base_rate_reduction = base_rate_reduction,
                                              change_mmc = FALSE,
                                              mmc_rel_rate = 0,
                                              mmc_change_years = 2025,
                                              change_prep = FALSE,
                                              prep_rel_rate = 0,
                                              prep_change_years = 2025)
  
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
                                                      base_rate_reduction = base_rate_reduction,
                                                      change_mmc = change_mmc,
                                                      mmc_rel_rate = mmc_rel_rate,
                                                      mmc_change_years = mmc_change_years,
                                                      change_prep = change_prep,
                                                      prep_rel_rate = prep_rel_rate,
                                                      prep_change_years = prep_change_years)
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
    mutate(TotalAIDSdeaths = TotalAIDSdeathsadult + AIDSdeaths0 + 
             AIDSdeaths1to4 + AIDSdeaths5to9 + AIDSdeaths10to14) %>% 
    mutate(TestsPerAdult = TotalHIVtests / (MalesOver15 + FemalesOver15))%>%
    mutate(TotalAdultsOnART = TotalART15F + TotalART15M) %>% 
    mutate(StartingART_15 = StartingART_M15 + StartingART_F15) %>%
    mutate(Total2LAdultsOnART= TotalART15M2L + TotalART15F2L) %>% 
    mutate(Adult_1st_line_FU = TotalAdultsOnART - Total2LAdultsOnART - StartingART_15) %>% 
    mutate(ANCtestPos = RediagnosesPregnancy + NewDiagnosesPregnancy) %>% 
    mutate(ANCtestNeg = TotANCtests - ANCtestPos) %>% 
    mutate(FollowUpART1to2 = TotalART1to2 - StartingART1to2) %>% 
    mutate(FollowUpART3to5 = TotalART3to5 - StartingART3to5) %>% 
    mutate(FollowUpART6to9 = TotalART6to9 - StartingART6to9) %>% 
    mutate(FollowUpART10to14 = TotalART10to14 - StartingART10to14) %>% 
    select(-c(AIDSdeathsAdultF, AIDSdeathsAdultM, AIDSdeaths0,  
                AIDSdeaths1to4, AIDSdeaths5to9, AIDSdeaths10to14, 
              TotalART15F, TotalART15M,
              StartingART_M15, StartingART_F15, TotalART15M2L, TotalART15F2L, 
              TotANCtests, TotalART1to2,
              TotalART3to5, TotalART6to9, TotalART10to14)) %>% 
    pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")
  
  df <- df %>%
    pivot_wider(names_from = indicator) %>%
    mutate(cost_art_adults_1st_line_1st_yr = StartingART_15 * 272.032122700937) %>%
    mutate(cost_art_adults_1st_line_fu = Adult_1st_line_FU * 170.751611736494) %>%
    mutate(cost_art_adults_2nd_line = Total2LAdultsOnART * 299.8747935152) %>%
    mutate(cost_art_neonates = StartingART0 * 91.2391745089155) %>%
    mutate(cost_art_kids_under_1 = StartingART1 * 285.916170708839) %>%
    mutate(cost_art_kids_1to2 = StartingART1to2 * 381.539892940659) %>%
    mutate(cost_art_kids_1to2_fu = FollowUpART1to2 * 311.447129366554) %>%
    mutate(cost_art_kids_3to5 = StartingART3to5 * 441.97810423809) %>%
    mutate(cost_art_kids_3to5_fu = FollowUpART3to5 * 371.885340663985) %>%
    mutate(cost_art_kids_6to9 = StartingART6to9 * 327.476567703383) %>%
    mutate(cost_art_kids_6to9_fu = FollowUpART6to9 * 257.383804129278) %>%
    mutate(cost_art_kids_10to14 = StartingART10to14 * 263.072779455781) %>%
    mutate(cost_art_kids_10to14_fu = FollowUpART10to14 * 192.980015881676) %>%
    mutate(cost_general_hts_neg = (PaedsHIVtestsNeg + AdultHIVtestsNeg) * 3.62338764573665) %>%
    mutate(cost_general_hts_pos = (PaedsHIVtestsPos + AdultHIVtestsPos) * 5.19545019977912) %>%
    mutate(cost_anc_test_neg = ANCtestNeg * 3.16302881313226) %>%
    mutate(cost_anc_test_pos = ANCtestPos * 4.71561496557645) %>%
    mutate(cost_palliative_care = TotalAIDSdeaths * 65.4212024937027) %>%
    mutate(cost_inpatient_art_under200 = (OnARTcurrUnder200) * 401.760069448105) %>%
    mutate(cost_inpatient_art_200to349 = (OnARTcurr200to349) * 126.835600669276) %>%
    mutate(cost_inpatient_art_350to499 = (OnARTcurr350to499) * 37.3981315846365) %>%
    mutate(cost_inpatient_art_over500 = (OnARTcurrOver500) * 37.3981315846365) %>%
    mutate(cost_inpatient_pre_art_under200 = (PreARTunder200M + PreARTunder200F + DiscARTunder200) * 156.388938508806) %>%
    mutate(cost_inpatient_pre_art_200to349 = (PreART200to349M + PreART200to349F + DiscART200to349) * 83.870745576945) %>%
    mutate(cost_inpatient_pre_art_350to499 = (PreART350to499M + PreART350to499F + DiscART350to499) * 46.8195688818689) %>%
    mutate(cost_inpatient_pre_art_over500 = (PreARTover500M + PreARTover500F + DiscARTover500) * 46.8195688818689) %>%
    select(-c(Adult_1st_line_FU,Total2LAdultsOnART,StartingART0,StartingART1to2,
              StartingART3to5, FollowUpART3to5, StartingART6to9, FollowUpART6to9,
              StartingART10to14, FollowUpART10to14, AdultHIVtestsNeg, AdultHIVtestsPos,
              PaedsHIVtestsNeg, PaedsHIVtestsPos,
              ANCtestNeg, ANCtestPos, OnARTcurrUnder200, OnARTcurr200to349, OnARTcurr350to499, OnARTcurrOver500,
              PreARTunder200M, PreARTunder200F, DiscARTunder200, PreART200to349M, PreART200to349F,
              DiscART200to349, PreART350to499M, PreART350to499F, DiscART350to499, PreARTover500M,
              PreARTover500F, DiscARTover500)) %>%
    pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")

  df <- df %>%
    pivot_wider(names_from = indicator) %>%
    group_by(parameter_set) %>%
    mutate(cost_total_testing = cost_general_hts_neg +
                                   cost_general_hts_pos +
                                   cost_anc_test_neg +
                                   cost_anc_test_pos) %>%
    mutate(cost_total_treatment = cost_art_adults_1st_line_1st_yr +
                                     cost_art_adults_1st_line_fu +
                                     cost_art_adults_2nd_line +
                                     cost_art_neonates +
                                     cost_art_kids_under_1 +
                                     cost_art_kids_1to2 +
                                     cost_art_kids_1to2_fu +
                                     cost_art_kids_3to5 +
                                     cost_art_kids_3to5_fu +
                                     cost_art_kids_6to9 +
                                     cost_art_kids_6to9_fu +
                                     cost_art_kids_10to14 +
                                     cost_art_kids_10to14_fu) %>%
    mutate(cost_total_care = cost_palliative_care +
                                      cost_inpatient_art_under200 +
                                      cost_inpatient_art_200to349 +
                                      cost_inpatient_art_350to499 +
                                      cost_inpatient_art_over500 +
                                      cost_inpatient_pre_art_under200 +
                                      cost_inpatient_pre_art_200to349 +
                                      cost_inpatient_pre_art_350to499 +
                                      cost_inpatient_pre_art_over500) %>%
    mutate(cost_total = cost_total_testing + cost_total_treatment +
             cost_total_care) %>%
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

  # # add discounting
  # 
  df_3pct_disc <- df %>%
    pivot_wider(names_from = year)

  for (i in 46:121){
    df_3pct_disc[,i] <- df_3pct_disc[,i]*(1-0.03)^(as.numeric(colnames(df_3pct_disc[,i]))- 2025)
  }

  df_3pct_disc <- df_3pct_disc %>%
    pivot_longer(-(pitc_reduction_year:indicator), names_to = "year")

  df_3pct_disc <- df_3pct_disc %>%
    mutate(year = as.numeric(year)) %>%
    relocate(year, .before = parameter_set)

  df_6pct_disc <- df %>%
    pivot_wider(names_from = year)

  for (i in 46:121){
    df_6pct_disc[,i] <- df_6pct_disc[,i]*(1-0.06)^(as.numeric(colnames(df_6pct_disc[,i]))- 2025)
  }

  df_6pct_disc <- df_6pct_disc %>%
    pivot_longer(-(pitc_reduction_year:indicator), names_to = "year")

  df_6pct_disc <- df_6pct_disc %>%
    mutate(year = as.numeric(year)) %>%
    relocate(year, .before = parameter_set)

  df$discount <- "undiscounted"
  df_3pct_disc$discount <- "3%"
  df_6pct_disc$discount <- "6%"

  df <- bind_rows(df, df_3pct_disc, df_6pct_disc)

  df <- df %>%
    relocate(discount, .before =  value)

  df <- df %>%
    pivot_wider(names_from = scenario) %>%
    mutate(annual_percent_change = ((intervention - baseline)/baseline)*100) %>%
    mutate(annual_absolute_dif = intervention - baseline) %>%
    pivot_longer(-(pitc_reduction_year:discount), names_to = "scenario")
    
  # saving a summary csv of all outputs
  summary <- as.data.frame(df) %>% 
    dplyr::group_by(year, scenario, indicator, pitc_reduction_year, test_reduction, discount
                    ) %>%
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
  
  write.csv(summary, paste0("results/", summary_name,  "_summary.csv"),row.names = FALSE)


  
# cumulative values - comment out if cumulative values are not required

  cumulative_years <- cumulative_years_list[1]

  df_shortened <- df %>% filter(scenario %in% c("intervention", "baseline"),
                                indicator %in% c("cost_total", "cost_total_care",
                                                 "cost_total_testing", "cost_total_treatment",
                                                 "TestsPerAdult",
                                                 "NewAdultHIV", "TotalAIDSdeathsadult"))

  cumulative_values <- calc_all_cumulatives(as.numeric(pitc_reduction_years),
                                              cumulative_years, df = df_shortened)

    # calculate elimination years and add to cumulative values

  cumulative_values <- add_elimination_year_to_cumulative(cumulative_values, df_shortened)
  cumulative_values[is.na(cumulative_values$discount),]$discount <- "undiscounted"

    # add cumulative year column

  cumulative_values$cumulative_years <- cumulative_years

  cumulative_values <- cumulative_values %>%
      relocate(cumulative_years, .before = scenario)

    # calculate cumulative percent change from baseline

  cumulative_values <- cumulative_values %>%
    pivot_wider(names_from = scenario, values_from = cumulative) %>%
    mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
    mutate(absolute_dif = intervention - baseline) %>%
    pivot_longer(-(indicator:discount), names_to = "scenario")

    # calculate percent of total cost
  cumulative_values <- cumulative_values %>%
    pivot_wider(names_from = indicator) %>%
    mutate(percent_total_cost_testing = (cost_total_testing / cost_total)*100) %>%
    pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")

    # Calculating running cumulative totals
  cumulative_summary1 <- cumulative_values %>%
    group_by(indicator, pitc_reduction_year, test_reduction, scenario, discount) %>%
    summarise(mean = mean(value), median = median(value),
              upper_CI = quantile(value, probs = 0.975, na.rm = TRUE),
              lower_CI = quantile(value, probs = 0.025, na.rm = TRUE))

  cumulative_summary1 <- cumulative_summary1 %>% mutate(future_variability = "test_reduction_only", future_value = 0)
  if (condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase & !art_coverage_decrease){
    cumulative_summary1$future_variability <- "condom_reduction"
    cumulative_summary1$future_value <- condom_usage_decrease
  }
  if (condom_usage_promotion & !condom_usage_reduction & !art_coverage_increase & !art_coverage_decrease){
    cumulative_summary1$future_variability <- "condom_promotion"
    cumulative_summary1$future_value <- condom_usage_increase
  }
  if (art_coverage_increase & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_decrease){
    cumulative_summary1$future_variability <-  "art_improvement"
    cumulative_summary1$future_value <- art_interrupt_rate_decrease
  }
  if (art_coverage_decrease & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase){
    cumulative_summary1$future_variability <- "art_deterioration"
    cumulative_summary1$future_value <- art_interrupt_rate_increase
  }
  cumulative_summary1$cumulative_years <- cumulative_years

  # cumulative values
  if (length(cumulative_years_list) >1){
    cumulative_years <- cumulative_years_list[2]

  df_shortened <- df %>% filter(scenario %in% c("intervention", "baseline"),
                                indicator %in% c("cost_total", "cost_total_care",
                                                 "cost_total_testing", "cost_total_treatment",
                                                 "TestsPerAdult",
                                                 "NewAdultHIV", "TotalAIDSdeathsadult"))

  cumulative_values <- calc_all_cumulatives(as.numeric(pitc_reduction_years),
                                            cumulative_years, df = df_shortened)

  # calculate elimination years and add to cumulative values

  cumulative_values <- add_elimination_year_to_cumulative(cumulative_values, df_shortened)
  cumulative_values[is.na(cumulative_values$discount),]$discount <- "undiscounted"

  # add cumulative year column

  cumulative_values$cumulative_years <- cumulative_years

  cumulative_values <- cumulative_values %>%
    relocate(cumulative_years, .before = scenario)

  # calculate cumulative percent change from baseline

  cumulative_values <- cumulative_values %>%
    pivot_wider(names_from = scenario, values_from = cumulative) %>%
    mutate(percent_change = ((intervention - baseline)/baseline)*100) %>%
    mutate(absolute_dif = intervention - baseline) %>%
    pivot_longer(-(indicator:discount), names_to = "scenario")

  # calculate percent of total cost
  cumulative_values <- cumulative_values %>%
    pivot_wider(names_from = indicator) %>%
    mutate(percent_total_cost_testing = (cost_total_testing / cost_total)*100) %>%
    pivot_longer(-(pitc_reduction_year:scenario), names_to = "indicator")

  # Calculating running cumulative totals
  cumulative_summary2 <- cumulative_values %>%
    group_by(indicator, pitc_reduction_year, test_reduction, scenario, discount) %>%
    summarise(mean = mean(value), median = median(value), upper_CI = quantile(value, probs = 0.975, na.rm = TRUE),
              lower_CI = quantile(value, probs = 0.025, na.rm = TRUE))

  cumulative_summary2 <- cumulative_summary2 %>% mutate(future_variability = "test_reduction_only", future_value = 0)
  if (condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase & !art_coverage_decrease){
    cumulative_summary2$future_variability <- "condom_reduction"
    cumulative_summary2$future_value <- condom_usage_decrease
  }
  if (condom_usage_promotion & !condom_usage_reduction & !art_coverage_increase & !art_coverage_decrease){
    cumulative_summary2$future_variability <- "condom_promotion"
    cumulative_summary2$future_value <- condom_usage_increase
  }
  if (art_coverage_increase & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_decrease){
    cumulative_summary2$future_variability <-  "art_improvement"
    cumulative_summary2$future_value <- art_interrupt_rate_decrease
  }
  if (art_coverage_decrease & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase){
    cumulative_summary2$future_variability <- "art_deterioration"
    cumulative_summary2$future_value <- art_interrupt_rate_increase
  }
  cumulative_summary2$cumulative_years <- cumulative_years

  cumulative_summaries <- bind_rows(cumulative_summary1, cumulative_summary2)
  } else {
    cumulative_summaries <- cumulative_summary1
  }

  write_csv(cumulative_summaries, paste0("results/", summary_name, "_cumulative.csv"))
}
  



