#### find elimination year ####
# these functions find the elimination year or record not attained and mean+95% CI of incidence at 2100

find_elimination_year <- function(df, pitc_reduction_year, pitc_reduction_percentage){
  inc <- df %>% 
  filter(scenario == "intervention", indicator == "HIVinc15to49",
         pitc_reduction_year == pitc_reduction_year, test_reduction == pitc_reduction_percentage, year > 2020) %>% 
  pivot_wider(names_from = indicator, values_from = "mean")
  # identify earliest year incidence <0.001 or record not attained
  if (!is.na(filter(inc, HIVinc15to49 < 0.001)$HIVinc15to49[1])) {
  HIV_elimination_year <- filter(inc, HIVinc15to49 < 0.001)$year[1]
  } else {
    HIV_elimination_year <- "Not attained"}
  HIV_elimination_year
}
find_elimination_years <- function(df){
  elimination_years <- data.frame(pitc_reduction_year = unique(df$pitc_reduction_year), 
                                  pitc_reduction_percentage = unique(df$test_reduction), 
                                  elimination_year = NA)
  for (i in 1:length(elimination_years[,1])){
    elimination_years$elimination_year[i] <- find_elimination_year(
      df = df, 
      pitc_reduction_year = elimination_years$pitc_reduction_year[i], 
      pitc_reduction_percentage = elimination_years$pitc_reduction_percentage[i])
  }
  elimination_years
}


find_incidences_at_2100 <- function(df){
  incidences_at_2100 <- data.frame(pitc_reduction_year = unique(df$pitc_reduction_year), 
                                pitc_reduction_percentage = unique(df$test_reduction), 
                                mean_incidence = NA,
                                lower_ci = NA,
                                upper_ci = NA)



  for (i in 1:length(incidences_at_2100[,1])){
  inc <- df %>% 
    filter(scenario == "intervention", indicator == "HIVinc15to49",
           pitc_reduction_year == incidences_at_2100$pitc_reduction_year[i], 
           test_reduction == incidences_at_2100$pitc_reduction_percentage[i], year > 2020) %>% 
    pivot_wider(names_from = indicator, values_from = "mean")
  incidences_at_2100$mean_incidence[i] <- tail(inc$HIVinc15to49, 1)
  incidences_at_2100$lower_ci[i] <- tail(inc$lower_CI, 1)
  incidences_at_2100$upper_ci[i] <- tail(inc$upper_CI, 1)
  }
  incidences_at_2100
}

find_inc_and_elimination <- function(df){
  incidences <- find_incidences_at_2100(df)
  years <- find_elimination_years(df) 
  inner_join(years, incidences, by = c("pitc_reduction_year", "pitc_reduction_percentage"))
}



