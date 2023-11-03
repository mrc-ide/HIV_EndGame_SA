#### Single age outputs ####

#### run baseline scenario ####

#### baseline ####
setwd("~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa")
source("R/cluster_function_orderly.R")
setwd("~/Documents/HIV_EndGame_SA/THEMBISAv18")
system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")

run_on_cluster(pitc_reduction_years = 2025, 
               pitc_reduction_percentage = c(100),
               condom_usage_reduction = FALSE,
               condom_usage_decrease = 0,
               condom_decr_start = 2025,
               condom_usage_promotion = FALSE,
               condom_usage_increase = 0,
               condom_incr_start = 2025,
               art_coverage_increase = FALSE,
               art_interrupt_rate_decrease = 0,
               art_incr_start = 2025,
               art_coverage_decrease = FALSE,
               art_interrupt_rate_increase = 0,
               art_decr_start = 2025,
               cumulative_years_list = 50,
               change_mmc = FALSE,
               mmc_rel_rate = 0,
               mmc_change_start = 2025,
               change_prep = FALSE,
               prep_rel_rate = 0, 
               prep_change_start = 2025, 
               summary_name = "age_specific_run" 
)

age_outputs <- read.delim("OutputByAge.txt", header=FALSE) # read txt file
t_age_outputs <- t(age_outputs) # transpose
age_outputs_new <- as.data.frame(as_tibble(t_age_outputs)) # covert to dataframe
age_outputs_new <- age_outputs_new[-117,] # remove last empty row
age_outputs_new$year <- seq(1985,2100,1) # add year column

indicators <- c("MalePop", "FemPop", "MaleInc", "FemInc", "MalePrev",
                "FemPrev", "MaleMort", "FemMort", "MaleDiag", "FemDiag",
                "MaleART", "FemART")
indicator_list <- rep(NA, 91*length(indicators))
for (indicator in indicators){  
  indicator_name <- rep("MalePopAS", 91)
  for (i in 1:length(indicator_name)){
    indicator_name[i] <- paste0(indicator_name, "_", i-1)
  }
}


names(output_new) <- seq_along(output_new)
  pivot_longer(output_new, -year, names_to = "parameter_set")
