library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(metR)

test_reduction_only <- read_csv("results/test_reduction_only_summary.csv")

unique(test_reduction_only$indicator)

test_reduction_only %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "VLunsuppressed15total",
         year > 2020, 
         test_reduction == 0 |
           test_reduction == 20 | 
           test_reduction == 80 | 
           test_reduction == 100) %>% 
  ggplot(aes(year, mean, group = test_reduction, fill = test_reduction)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = test_reduction), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = test_reduction), show.legend = T) +
  geom_hline(aes(yintercept = 0.001), lty = "dotted") +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("Proportion of adults with HIV with unsuppressed viral load") +
  scale_fill_discrete(labels = c("No PITC reduction", "20% PITC reduction", "80% PITC reduction", "100% PITC reduction"),) + 
  scale_color_discrete(labels = c("No PITC reduction", "20% PITC reduction", "80% PITC reduction", "100% PITC reduction")) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(vars(pitc_reduction_year),labeller = as_labeller(c(`2025` = "PITC reduced in 2025",
                                                                `2030` = "PITC reduced in 2030",
                                                                `2035` = "PITC reduced in 2035",
                                                                `2040` = "PITC reduced in 2040", 
                                                                `2045` = "PITC reduced in 2045", 
                                                                `2050` = "PITC reduced in 2050")))


#### function : run on cluster ####

system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa.exe -O2")
system("./thembisa.exe")
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
  
  output_names <- c("TotalHIVtests", "NewAdultHIV",
                    "AIDSdeathsAdultM", "AIDSdeathsAdultF", "TotSexActs",
                    "SWsexActs", "TotProtSexActs", "SWsexActsProt", "HIVinc15to49", 
                    "Prev15to49", "ARTcoverageDiagM", "ARTcoverageDiagF", "MalesOver15", 
                    "FemalesOver15", "TotHIV15", "ARTcoverageAdult", 
                    "VLsuppressed15total", "VLunsuppressed15total")
  
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
  
  df <- df %>%
    pivot_wider(names_from = indicator) %>%
    mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
    mutate(TestsPerAdult = TotalHIVtests / (MalesOver15 + FemalesOver15))%>%
    mutate(AIDS_Mortality = TotalAIDSdeathsadult / (TotHIV15)) %>% 
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
    summarise(mean = mean(value), upper_CI = quantile(value, probs = 0.975, na.rm = TRUE),
              lower_CI = quantile(value, probs = 0.025, na.rm = TRUE))
  
  cumulative_summary <- cumulative_summary %>% mutate(future_variability = "test_reduction_only", future_value = 0)
  if (condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase & !art_coverage_decrease){
    cumulative_summary$future_variability <- "condom_reduction"
    cumulative_summary$future_value <- condom_usage_decrease
  }
  if (condom_usage_promotion & !condom_usage_reduction & !art_coverage_increase & !art_coverage_decrease){
    cumulative_summary$future_variability <- "condom_promotion"
    cumulative_summary$future_value <- condom_usage_increase
  }
  if (art_coverage_increase & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_decrease){
    cumulative_summary$future_variability <-  "art_improvement"
    cumulative_summary$future_value <- art_interrupt_rate_decrease
  }
  if (art_coverage_decrease & !condom_usage_reduction & !condom_usage_promotion & !art_coverage_increase){
    cumulative_summary$future_variability <- "art_deterioration"
    cumulative_summary$future_value <- art_interrupt_rate_increase
  }
  
  
  write_csv(cumulative_summary, paste0("results/cumulative_", summary_name, ".csv"))
}
#### run model ####

art_change_values <- seq(0, 14, 0.5)


for (i in art_change_values){
  run_on_cluster(pitc_reduction_years = 2025, 
                 pitc_reduction_percentage = seq(0,100,5),
                 condom_usage_reduction = FALSE, 
                 condom_usage_decrease = 0,
                 condom_decr_start = 2025,
                 condom_usage_promotion = FALSE,
                 condom_usage_increase = 0,
                 condom_incr_start = 2025,
                 art_coverage_increase = FALSE,
                 art_interrupt_rate_decrease = 0/100,
                 art_incr_start = 2025,
                 summary_name = paste0("decrease_art_retention_", i),
                 cumulative_years = 50,
                 art_coverage_decrease = TRUE,
                 art_interrupt_rate_increase = i/100,
                 art_decr_start = 2025
  )
}
#### read results increase ####
filepaths <- paste0("results/increase_art_retention_", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
increase_art_retention <- bind_rows(temp, .id = "art_int_reduction")

increase_art_retention <- increase_art_retention %>%  mutate(art_int_rate = (0.22*((1-(as.numeric(art_int_reduction)/100))**10))) %>% 
  mutate(art_ret_rate = 1 - art_int_rate)

unique(increase_art_retention$art_ret_rate)
unique(increase_art_retention$test_reduction)

write_csv(increase_art_retention, "results/increase_art_retention.csv")
increase_art_retention <- read_csv("results/increase_art_retention.csv")


#### read results decrease ####
filepaths <- paste0("results/decrease_art_retention_", art_change_values, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- art_change_values
decrease_art_retention <- bind_rows(temp, .id = "art_int_promotion")

decrease_art_retention <- decrease_art_retention %>%  mutate(art_int_rate = (0.22*((1+(as.numeric(art_int_promotion)/100))**10))) %>% 
  mutate(art_ret_rate = 1 - art_int_rate)

unique(decrease_art_retention$art_ret_rate)
unique(decrease_art_retention$test_reduction)

write_csv(decrease_art_retention, "results/decrease_art_retention.csv")
decrease_art_retention <- read_csv("results/decrease_art_retention.csv")


#### joining increase and decrease ####

art_change_summary <- bind_rows(increase_art_retention, decrease_art_retention)

#### plotting trends over time increase  ####

increase_art_retention %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "VLunsuppressed15total",
         year > 2020, 
         ) %>% mutate(art_ret_rate = as.factor(round(art_ret_rate,2))) %>% 
  ggplot(aes(year, mean, group = art_ret_rate, fill = art_ret_rate)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.01, show.legend = F) +
  geom_line(aes(colour = art_ret_rate), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("Proportion of adults with suppressed viral load") +
  scale_fill_discrete("ART \nretention\nrate in\n2035") + 
  scale_color_discrete("ART \nretention\nrate in\n2035")

increase_art_retention %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "HIVinc15to49",
         year > 2020, 
  ) %>% mutate(art_ret_rate = as.factor(round(art_ret_rate,2))) %>% 
  ggplot(aes(year, mean, group = art_ret_rate, fill = art_ret_rate)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.01, show.legend = F) +
  geom_line(aes(colour = art_ret_rate), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV incidence per 1000 (15-49 years)", labels = (function(l) {round(l*1e3,1)})) +
  scale_fill_discrete("ART \nretention\nrate in\n2035") + 
  scale_color_discrete("ART \nretention\nrate in\n2035")

increase_art_retention %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "Prev15to49",
         year > 2020, 
  ) %>% mutate(art_ret_rate = as.factor(round(art_ret_rate,2))) %>% 
  ggplot(aes(year, mean, group = art_ret_rate, fill = art_ret_rate)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.01, show.legend = F) +
  geom_line(aes(colour = art_ret_rate), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("HIV Prevalence (15-49 years)") +
  scale_fill_discrete("ART \nretention\nrate in\n2035") + 
  scale_color_discrete("ART \nretention\nrate in\n2035")

art_change_summary %>% mutate(test_reduction = as.factor(test_reduction)) %>% 
  filter(scenario == "intervention", 
         pitc_reduction_year == 2025, 
         indicator == "ARTcoverageAdult",
         year > 2020,
         test_reduction == 0, 
  ) %>% mutate(art_ret_rate = as.factor(round(art_ret_rate,2))) %>% 
  ggplot(aes(year, mean, group = art_ret_rate, fill = art_ret_rate)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.01, show.legend = F) +
  geom_line(aes(colour = art_ret_rate), show.legend = T) +
  xlab("") +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous("ART Coverage (older than 15 years)", limits = c(0,1)) +
  scale_fill_discrete("ART retention \nrate in 2035") + 
  scale_color_discrete("ART retention \nrate in 2035") + 
  guides(fill=guide_legend(ncol=2))

increased_art_inc_elim <- find_inc_and_elimination(increase_art_retention) 
increased_art_inc_elim <- increased_art_inc_elim %>% 
  mutate(art_int_rate = (0.22*((1-(as.numeric(future_value)))**10))) %>% 
  mutate(art_ret_rate = 1 - art_int_rate)

#### prop unsuppressed VL adults in 2100 ####
find_unsuppressed_vl <- function(simulation_df){
  future_variable <- unique(simulation_df$future_variability)
  future_values <- unique(simulation_df$future_value)
  pitc_reduction_years <- unique(simulation_df$pitc_reduction_year)
  pitc_reduction_percentages <- unique(simulation_df$test_reduction)
  VLunsuppressed_at_2100 <- data.frame(expand_grid(future_variability = future_variable, 
                                               future_value = future_values, 
                                               pitc_reduction_year = pitc_reduction_years,
                                               pitc_reduction_percentage = pitc_reduction_percentages,
                                               mean_incidence_2100 = NA,
                                               lower_ci = NA,
                                               upper_ci = NA))
  input_values <- expand_grid(future_variability = future_variable, 
                              future_value = future_values, 
                              pitc_reduction_year = pitc_reduction_years, 
                              pitc_reduction_percentage = pitc_reduction_percentages)
  
  
  for (i in 1:length(input_values$pitc_reduction_year)){
    inc <- simulation_df %>% 
      filter(scenario == "intervention", 
             indicator == "VLsuppressed15total",
             pitc_reduction_year == input_values$pitc_reduction_year[i],
             test_reduction == input_values$pitc_reduction_percentage[i],
             future_variability == input_values$future_variability[i], 
             future_value == input_values$future_value[i], 
             year == 2100)
    VLunsuppressed_at_2100$mean_incidence_2100[i] <- tail(inc$mean, 1)
    VLunsuppressed_at_2100$lower_ci[i] <- tail(inc$lower_CI, 1)
    VLunsuppressed_at_2100$upper_ci[i] <- tail(inc$upper_CI, 1)
  }
  VLunsuppressed_at_2100
}

VLunsuppressed_at_2100 <- find_unsuppressed_vl(increase_art_retention)

increased_art_inc_elim <- inner_join(increased_art_inc_elim, VLunsuppressed_at_2100, by = c("future_variability", "future_value", "pitc_reduction_year", "pitc_reduction_percentage"))

# hiv incidence at 2100
increased_art_inc_elim %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  #filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(art_ret_rate, mean_incidence_2100.x)) + 
  geom_line(aes()) +
  geom_ribbon(aes(ymin = lower_ci.x, ymax = upper_ci.x), alpha = 0.1, show.legend = F) +
  xlab("ART retention rate in 2035") + 
  theme_classic() + 
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,2)})) + 
  expand_limits(y = 0) +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10))



# vl suppresion
increased_art_inc_elim %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  #filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(art_ret_rate, mean_incidence_2100.y,)) + 
  geom_line(aes()) +
  geom_ribbon(aes(ymin = lower_ci.y, ymax = upper_ci.y), alpha = 0.1, show.legend = F) +
  xlab("ART retention rate in 2035") + 
  theme_classic() + 
  scale_y_continuous("Proportion unsuppressed VL adults in 2100") + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10))

# unsuppressed VL vs incidence in 2100
increased_art_inc_elim %>% 
  mutate(pitc_reduction_year = as.factor(pitc_reduction_year)) %>% 
  #filter(pitc_reduction_year == 2025) %>% 
  ggplot(aes(mean_incidence_2100.y, mean_incidence_2100.x)) + 
  geom_line(aes()) +
  scale_y_continuous("HIV incidence per 1000 (15-49 years) in 2100", labels = (function(l) {round(l*1e3,2)})) + 
  theme_classic() + 
  scale_x_continuous("Proportion suppressed VL adults in 2100") + 
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.text = element_text(size = 10))


vl_suppression <- increase_art_retention %>% 
  filter(indicator %in% c("Prev15to49","ARTcoverageAdult", "HIVinc15to49", "VLsuppression15total"),
         scenario == "intervention") %>% 
  select(-c(art_int_reduction, scenario, future_value, art_int_rate, future_variability, pitc_reduction_year, test_reduction)) %>% 
  select(c(year, indicator, art_ret_rate, mean, lower_CI, upper_CI))

write_csv(vl_suppression, "results/vl_suppression_100.csv")
