small_df <- read_csv("small_df.csv")

calc_cumulative <- function(start_year, follow_up_years){
  end_year <- start_year + follow_up_years
  small_df %>%
    filter(
      indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
      indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
      indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
      indicator != "TotalART15M", indicator != "TotalART15F",
      indicator != "TotalARTAdult", scenario != "percent_change", 
      indicator != "TotalDiagnosedHIV", indicator!= "ARTinititationRatio", 
      indicator != "ARTInitPerNewInfection", indicator != "LifeYrsSaved",
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

cumulative_values <- calc_all_cumulatives(intervention_years, 20)

cumulative_values <- cumulative_values %>%
  pivot_wider(names_from = scenario, values_from = cumulative) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(indicator:parameter_set), names_to = "scenario") 

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

plot_cumulative_uncertainty()
