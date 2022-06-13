setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA")
#### Changing general testing from 2025 onwards to 0 in 5-year increments

# Baseline with no changes
data <- readLines("THEMBISAv18/Rollout_Original.txt")
formatted_data <- format_data(data, dictionary)
rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
write(rollout, "THEMBISAv18/Rollout.txt")
run_thembisa()
TotalNewHIV_baseline <- read_output("THEMBISAv18/TotalNewHIV")
LifeYrsLost_baseline <- read_output("THEMBISAv18/LYlostAIDS")
AIDSdeathsAdultMale_baseline <- read_output("THEMBISAv18/AIDSdeathsAdultM")
AIDSdeathsAdultFemale_baseline <- read_output("THEMBISAv18/AIDSdeathsAdultF")

# loop for each 5-year intervenal between 2025 and 2070
# make new output for each year intervention implemented

five_years <- seq(2025, 2070, 5)
for (year in five_years){
  data <- readLines("THEMBISAv18/Rollout_Original.txt")
  formatted_data <- format_data(data, dictionary)
  formatted_data <- edit_formatted_data("rate_first_test_neg_fem_under_25", 
                                      new_values = 0.0, starting_year = year)
  rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)
  write(rollout, "THEMBISAv18/Rollout.txt")
  run_thembisa()
  assign(paste("TotalNewHIV_", as.character(year), sep = ""), 
         read_output("THEMBISAv18/TotalNewHIV"))
  assign(paste("AIDSdeathsAdultMale_", as.character(year), sep = ""), 
         read_output("THEMBISAv18/AIDSdeathsAdultM"))
  assign(paste("AIDSdeathsAdultFemale_", as.character(year), sep = ""), 
         read_output("THEMBISAv18/AIDSdeathsAdultF"))
  assign(paste("LifeYrsLost_", as.character(year), sep = ""), 
         read_output("THEMBISAv18/LYlostAIDS"))
}
# make new plots 
# new hiv cases
plot_outputs(TotalNewHIV_baseline, "New Cases of HIV", "Annual new HIV cases - no testing reduction") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2025, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2025") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2030, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2030") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2035, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2035") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2040, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2040") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2045, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2045") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2050, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2050") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2055, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2055") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2060, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2060") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2065, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2065") + xlim(2020, 2070)
plot_outputs(TotalNewHIV_2070, "New Cases of HIV", "Annual new HIV cases - reducing testing in 2070") + xlim(2020, 2070)

# life years lost
plot_outputs(LifeYrsLost_baseline, "Life years lost", "Life years lost - no testing reduction") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2025, "Life years lost", "Life years lost - reducing testing in 2025") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2030, "Life years lost", "Life years lost - reducing testing in 2030") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2035, "Life years lost", "Life years lost - reducing testing in 2035") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2040, "Life years lost", "Life years lost - reducing testing in 2040") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2045, "Life years lost", "Life years lost - reducing testing in 2045") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2050, "Life years lost", "Life years lost - reducing testing in 2050") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2055, "Life years lost", "Life years lost - reducing testing in 2055") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2060, "Life years lost", "Life years lost - reducing testing in 2060") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2065, "Life years lost", "Life years lost - reducing testing in 2065") + xlim(2020, 2070) + ylim(0, 2.5e+6)
plot_outputs(LifeYrsLost_2070, "Life years lost", "Life years lost - reducing testing in 2070") + xlim(2020, 2070) + ylim(0, 2.5e+6)

# AIDS related deaths in Adult Males 
plot_outputs(AIDSdeathsAdultMale_baseline, "AIDS related deaths", "AIDS related deaths in Adult Males - no testing reduction") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2025, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2025") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2030, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2030") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2035, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2035") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2040, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2040") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2045, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2045") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2050, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2050") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2055, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2055") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2060, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2060") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2065, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2065") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultMale_2070, "AIDS related deaths", "AIDS related deaths in Adult Males reducing testing in 2070") + xlim(2020, 2070) + ylim(0, 30000)

# AIDS related deaths in Adult Females 
plot_outputs(AIDSdeathsAdultFemale_baseline, "AIDS related deaths", "AIDS related deaths in Adult Females - no testing reduction") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2025, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2025") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2030, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2030") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2035, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2035") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2040, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2040") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2045, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2045") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2050, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2050") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2055, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2055") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2060, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2060") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2065, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2065") + xlim(2020, 2070) + ylim(0, 30000)
plot_outputs(AIDSdeathsAdultFemale_2070, "AIDS related deaths", "AIDS related deaths in Adult Females reducing testing in 2070") + xlim(2020, 2070) + ylim(0, 30000)

