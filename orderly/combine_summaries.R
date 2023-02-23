#### running multiple scenarios on orderly ####

library(orderly) 
library(dplyr)
library(purrr)

# parameters to run

pitc_reduction_years <- seq(2025, 2050, 5)
pitc_reduction_percentage <- seq(0, 100, 10)
condom_usage_reduction <- TRUE
condom_usage_decrease <- seq(0,14,2)
condom_incr_start <- seq(2025, 2040, 5)
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0
art_incr_start <- 2025
art_coverage_decrease <- FALSE
art_interrupt_rate_increase <- 0
art_decr_start <- 2025
cumulative_years <- 50
summary_name <- "summary"

# combine parameters 

pars <- crossing(pitc_reduction_years,
                    pitc_reduction_percentage,
                    condom_usage_reduction,
                    condom_usage_decrease,
                    condom_incr_start,
                    art_coverage_increase,
                    art_interrupt_rate_decrease,
                    art_incr_start,
                    art_coverage_decrease,
                    art_interrupt_rate_increase,
                    art_decr_start,
                    cumulative_years,
                    summary_name)
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
for (i in seq_len(nrow(pars))){
  orderly::orderly_run(parameters = list(pitc_reduction_years = pars$pitc_reduction_years[i],
                                         pitc_reduction_percentage = pars$pitc_reduction_percentage[i],
                                         condom_usage_reduction = pars$condom_usage_reduction[i],
                                         condom_usage_decrease = pars$condom_usage_decrease[i],
                                         condom_incr_start = pars$condom_incr_start[i],
                                         art_coverage_increase = pars$art_coverage_increase[i],
                                         art_interrupt_rate_decrease = pars$art_interrupt_rate_decrease[i],
                                         art_incr_start = pars$art_incr_start[i],
                                         art_coverage_decrease = pars$art_coverage_decrease[i],
                                         art_interrupt_rate_increase = pars$art_interrupt_rate_increase[i],
                                         art_decr_start = pars$art_decr_start[i],
                                         cumulative_years = pars$cumulative_years[i],
                                         summary_name = pars$summary_name[i]))
}

# this will find all of the names of the tasks that meet the criteria in query in drafts

draft_task_names <- orderly::orderly_list_drafts(include_failed = FALSE)$id
lapply(X = draft_task_names, FUN = function(x){
  orderly::orderly_commit(x)
})

orderly_list_archive()$id -> archive_task_list

archive_task_names <- unlist(lapply(seq_len(nrow(pars)), function(i) {
  orderly::orderly_search(name = "thembisa", 
                          query = "latest(parameter:pitc_reduction_percentage == pitc_reduction_percentage && 
                          parameter:pitc_reduction_percentage == pitc_reduction_percentage &&  
                          parameter:condom_usage_reduction == condom_usage_reduction &&
                          parameter:condom_usage_decrease == condom_usage_decrease &&
                          parameter:condom_incr_start == condom_incr_start &&
                          parameter:art_coverage_increase == art_coverage_increase &&
                          parameter:art_interrupt_rate_decrease == art_interrupt_rate_decrease &&
                          parameter:art_incr_start == art_incr_start &&
                          parameter:art_coverage_decrease == art_coverage_decrease &&
                          parameter:art_interrupt_rate_increase == art_interrupt_rate_increase &&
                          parameter:art_decr_start == art_decr_start &&
                          parameter:cumulative_years == cumulative_years &&
                          parameter:summary_name == summary_name)",
                          parameters = list(pitc_reduction_years = pars$pitc_reduction_years[i],
                                            pitc_reduction_percentage = pars$pitc_reduction_percentage[i],
                                            condom_usage_reduction = pars$condom_usage_reduction[i],
                                            condom_usage_decrease = pars$condom_usage_decrease[i],
                                            condom_incr_start = pars$condom_incr_start[i],
                                            art_coverage_increase = pars$art_coverage_increase[i],
                                            art_interrupt_rate_decrease = pars$art_interrupt_rate_decrease[i],
                                            art_incr_start = pars$art_incr_start[i],
                                            art_coverage_decrease = pars$art_coverage_decrease[i],
                                            art_interrupt_rate_increase = pars$art_interrupt_rate_increase[i],
                                            art_decr_start = pars$art_decr_start[i],
                                            cumulative_years = pars$cumulative_years[i],
                                            summary_name = pars$summary_name[i])
  )}))



filepaths <- paste0("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa/", archive_task_names, "/results/summary.csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- task_names  
combined_summary <- bind_rows(temp, .id = "task_name")

inc_and_elim <- find_inc_and_elimination(combined_summary)
reduction_year <- 2025
pitc_reduction <- 100
combined_summary %>% 
  filter(scenario == "intervention", indicator == "HIVinc15to49",
         pitc_reduction_year == reduction_year, test_reduction == pitc_reduction, year > 2020) %>% 
  pivot_wider(names_from = indicator, values_from = "mean")
# identify earliest year incidence <0.001 or record not attained
if (!is.na(filter(inc, HIVinc15to49 < 0.001)$HIVinc15to49[1])) {
  HIV_elimination_year <- filter(inc, HIVinc15to49 < 0.001)$year[1]
} else {
  HIV_elimination_year <- NA}
HIV_elimination_year

unique(combined_summary$pitc_reduction_year)


