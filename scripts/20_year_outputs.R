#### Set working directory ####

setwd("/Users/stefanrautenbach/Documents/Imperial/Research_project/HIV_EndGame_SA")

#### Load packages/functions ####

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

source("R/support_modify_inputs.R")
source("scripts/modify_rollout.R")
source("R/read_and_run.R")

#### Make empty dataframe for outputs ####

# names of all the outputs of interest

output_names <- c("TotalHIVtests", "TotalNewHIV", "LYlostAIDS", 
                  "AIDSdeathsAdultM", "AIDSdeathsAdultF", 
                  "DiagnosedHIV_M", "DiagnosedHIV_F", 
                  "Number1stHIVtestsPos", "StartingARTtot", "Prop1stHIVtestsPos", 
                  "NewDiagnosesPregnancy", "TotalART15F", "TotalART15M", 
                  "RediagnosesPregnancy", "TotANCtests")

# create empty folder for results

dir.create("results", FALSE, TRUE)

# establish intervention years
intervention_years <- seq(2025, 2050, 5)

# run baseline model
t1 <- Sys.time()
baseline <- run_thembisa_scenario(NA, output_names)

# save baseline outputs
write.csv(baseline, "results/baseline.csv", row.names = FALSE)

# for loop that changes testing rate at different years and saves outputs
for (intervention_year in intervention_years){
  one_scenario <- run_thembisa_scenario(intervention_year, output_names)
  write.csv(one_scenario, paste0("results/scenario_", intervention_year, ".csv"),
            row.names = FALSE)
}

# make a new data frame joining all results 
df <- read_thembisa_results(intervention_years)
write_csv(df, "results/all_scenarios.csv")
df <- read_csv("results/all_scenarios.csv")

t2 <- Sys.time()
time_elapsed <- t2 - t1

## Calculate total AIDS related mortality in Adults 
## Calculate total ART in Adults 
## Calculate total number of diagnosed adults

df <- df %>%
  pivot_wider(names_from = indicator) %>%
  mutate(TotalAIDSdeathsadult = AIDSdeathsAdultF + AIDSdeathsAdultM) %>%
  mutate(TotalARTAdult = TotalART15F + TotalART15M) %>% 
  mutate(TotalDiagnosedHIV = DiagnosedHIV_F + DiagnosedHIV_M) %>% 
  pivot_longer(-(intervention_year:scenario), names_to = "indicator")


### write csv of outputs_df ####

write.csv(df, "results/df.csv", row.names = FALSE)
df <- read.csv("results/df.csv")

### plot yearly outputs #### 
# for some reason I can't loop over these

df %>% filter(
  scenario != "percent_change",
  indicator == output_names[1],
  year >= 2020) %>% 
  group_by(year, scenario, intervention_year) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, median, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, group = scenario), alpha = 0.5) +
  geom_line(aes(color = scenario)) +
  geom_vline(intervention_year) +
  ggtitle(output_names[1]) +
  xlab("Years") +
  facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()



df %>% filter(indicator == "TotalHIVtests" & parameter_set == 695, year < 2025) %>% 
  group_by(year, scenario) %>% summarise(mean(value),sd(value))

plot_outputs_with_uncertainty(output_names[1]) + ggtitle("Annual number of HIV tests performed") + ylab("Number of HIV tests")
plot_outputs_with_uncertainty(output_names[2]) + ggtitle ("Annual number of new HIV infections") + ylab ("New HIV infections") 
plot_outputs_with_uncertainty(output_names[3]) + ggtitle ("Annual life-years lost to AIDS") + ylab ("Life-years lost")
plot_outputs_with_uncertainty(output_names[4]) + ggtitle ("Annual adult male AIDS-related mortality") + ylab ("AIDS-related deaths")
plot_outputs_with_uncertainty(output_names[5]) + ggtitle ("Annual adult female AIDS-related mortality") + ylab ("AIDS-related deaths")
plot_outputs_with_uncertainty(output_names[6]) + ggtitle ("Total number of adult males with diagnosed HIV") + ylab ("HIV diagnosed adults")
plot_outputs_with_uncertainty(output_names[7]) + ggtitle ("Total number of adult females with diagnosed HIV") + ylab ("HIV diagnosed adults")
plot_outputs_with_uncertainty(output_names[8]) + ggtitle ("Annual number of adults with newly diagnosed HIV") + ylab ("New HIV diagnoses")
plot_outputs_with_uncertainty(output_names[9]) + ggtitle ("Annual number of adults initiating ART") + ylab ("Adults initating ART")
plot_outputs_with_uncertainty(output_names[10]) + ggtitle ("Proportion of HIV tests that are first-time positive") + ylab ("Proportion")
plot_outputs_with_uncertainty(output_names[11]) + ggtitle ("Annual number of first-time diagnoses at ANC") + ylab ("New HIV diagnoses")
plot_outputs_with_uncertainty(output_names[12]) + ggtitle ("Total number of female adults on ART") + ylab ("Adults on ART")
plot_outputs_with_uncertainty(output_names[13]) + ggtitle ("Total number of male adults on ART") + ylab ("Adults on ART")
plot_outputs_with_uncertainty(output_names[14]) + ggtitle ("Annual number of rediagnoses at ANC") + ylab ("HIV re-diagnoses")
plot_outputs_with_uncertainty(output_names[15]) + ggtitle ("Annual number of HIV tests performed at ANC") + ylab ("HIV tests")
plot_outputs_with_uncertainty("TotalAIDSdeathsadult") + ggtitle ("Annual adult AIDS-related mortality") + ylab ("AIDS-related deaths")
plot_outputs_with_uncertainty("TotalARTAdult") + ggtitle ("Total number of adults on ART") + ylab ("Adults on ART")
plot_outputs_with_uncertainty("TotalDiagnosedHIV") + ggtitle ("Total number of adults with diagnosed HIV") + ylab ("HIV diagnosed adults")



# calculate percentage from baseline for trends

df <- df %>%
  pivot_wider(names_from = scenario) %>% 
  mutate(percent_change = ((intervention - baseline)/baseline)*100) %>% 
  pivot_longer(-(intervention_year:indicator), names_to = "scenario") 

df %>% 
  filter(scenario == "percent_change",
         indicator == "TotalNewHIV", 
         year >= 2020) %>% 
  group_by(year, intervention_year) %>% 
  summarise(median = median(value), upper_CI = quantile(value, probs = 0.975), 
            lower_CI = quantile(value, probs = 0.025)) %>% 
  ggplot(aes(year, median)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), alpha = 0.5) +
  geom_line() +
  ggtitle("TotalNewHIV") +
  xlab("Years") + ylab("Change from baseline (%)") +
  facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()


# baseline_only <- df %>% 
#   filter(scenario == "baseline")
# intervention_only <- df %>%
#   filter(scenario == "intervention")
# intervention_only$percent_change <- 
#   ((intervention_only$value - baseline_only$value) / baseline_only$value) * 100

# plot percentage change from baseline 

intervention_only %>% filter(
  indicator == output_names[4],
  year >= 2020) %>% 
  group_by(year, scenario, intervention_year) %>% 
  summarise(median = median(percent_change), upper_CI = quantile(percent_change, probs = 0.975), 
            lower_CI = quantile(percent_change, probs = 0.025)) %>% 
  ggplot(aes(year, median, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, group = scenario), alpha = 0.5) +
  geom_line(aes(color = scenario)) +
  ggtitle(output_names[1]) +
  xlab("Years") + ylab("Change from baseline (%)") +
  facet_wrap(~intervention_year) + expand_limits(y=0) + theme_bw()

plot_pct_chg_uncertainty(output_names[1]) + ggtitle("Percentage change of annual HIV tests performed")
plot_pct_chg_uncertainty(output_names[2]) + ggtitle ("Percentage change of annual new HIV infections")
plot_pct_chg_uncertainty(output_names[3])
plot_pct_chg_uncertainty(output_names[4])
plot_pct_chg_uncertainty(output_names[5])
plot_pct_chg_uncertainty(output_names[6])
plot_pct_chg_uncertainty(output_names[7])
plot_pct_chg_uncertainty(output_names[8])
plot_pct_chg_uncertainty(output_names[9])
plot_pct_chg_uncertainty(output_names[11])
plot_pct_chg_uncertainty(output_names[12])
plot_pct_chg_uncertainty(output_names[13])
plot_pct_chg_uncertainty(output_names[14])
plot_pct_chg_uncertainty(output_names[15])

## cumulative outputs

# make a dataframe of the cumulative results 
# start with the baseline results from 2025 to 2045 

df %>%
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2025,
    year <= 2025+20, 
    scenario == "baseline",
    intervention_year == 2025
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs

# then baseline results from 2030 to 2050 

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2030,
    year <= 2030+20, 
    scenario == "baseline",
    intervention_year == 2030
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2030

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2030))

# baseline results from 2035 to 2055

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2035,
    year <= 2035+20, 
    scenario == "baseline",
    intervention_year == 2035
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2035

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2035))

# baseline results from 2040 to 2060

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2040,
    year <= 2040+20, 
    scenario == "baseline",
    intervention_year == 2040
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2040

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2040))


# baseline results from 2045 to 2065

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2045,
    year <= 2045+20, 
    scenario == "baseline",
    intervention_year == 2045
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2045

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2045))


# baseline results from 2050 to 2070

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2050,
    year <= 2050+20, 
    scenario == "baseline",
    intervention_year == 2050
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_baseline_2050

# add that to the bottom of the cumulative outputs
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_baseline_2050))

# make a new a new dataframe of the cumulative results of the 2025 intervention

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2025,
    year <= 2025+20, 
    scenario == "intervention",
    intervention_year == 2025
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2025
# add that to the bottom of the baseline 
cumulative_outputs <- add_row(as.data.frame(cumulative_outputs), 
                              as.data.frame(cumulative_outputs_2025))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2030,
    year <= 2030+20, 
    scenario == "intervention",
    intervention_year == 2030
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2030

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2030))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2035,
    year <= 2035+20, 
    scenario == "intervention",
    intervention_year == 2035
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2035

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2035))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2040,
    year <= 2040+20, 
    scenario == "intervention",
    intervention_year == 2040,
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2040

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2040))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2045,
    year <= 2045+20, 
    scenario == "intervention",
    intervention_year == 2045
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2045

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2045))

df %>%
  filter(intervention_year <= 2050) %>% 
  group_by(indicator, intervention_year, scenario) %>% 
  filter(
    year >= 2050,
    year <= 2050+20, 
    scenario == "intervention",
    intervention_year == 2050
  ) %>% 
  summarise(cumulative = sum(value)) -> cumulative_outputs_2050

cumulative_outputs <- add_row(cumulative_outputs, 
                              as.data.frame(cumulative_outputs_2050))

# plot all in one grid

cumulative_outputs %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult") %>% 
  ggplot(aes(intervention_year, cumulative, color = scenario)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Cumulative Value")

# calculate cumulative percent change from baseline 

cumulative_baseline_only <- cumulative_outputs %>% 
  filter(scenario == "baseline")
cumulative_intervention_only <- cumulative_outputs %>%
  filter(scenario == "intervention")
cumulative_intervention_only$percent_change <- 
  ((cumulative_intervention_only$cumulative - cumulative_baseline_only$cumulative)
   / cumulative_baseline_only$cumulative) * 100

# plot percentage change  in one grid

cumulative_intervention_only %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult") %>% 
  ggplot(aes(intervention_year, percent_change)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Percentage of baseline (%)")

# epidemiologic outcomes only 

cumulative_intervention_only %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult", indicator != "NewDiagnosesPregnancy", 
         indicator != "Number1stHIVtestsPos", indicator != "RediagnosesPregnancy",
         indicator!= "StartingARTtot", indicator != "TotalHIVtests", 
         indicator != "TotANCtests") %>% 
  ggplot(aes(intervention_year, percent_change)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Change from baseline (%)")


# epidemiologic outcomes only 

cumulative_intervention_only %>%
  filter(indicator != "Prop1stHIVtestsPos", indicator != "DiagnosedHIV_M",
         indicator !="DiagnosedHIV_F", indicator != "TotalHIV", 
         indicator != "AIDSdeathsAdultF", indicator != "AIDSdeathsAdultM", 
         indicator != "TotalART15M", indicator != "TotalART15F",
         indicator != "TotalARTAdult", indicator != "TotalNewHIV", 
         indicator != "LYlostAIDS", indicator != "TotalHIVtests", 
         indicator != "TotalAIDSdeathsAdult") %>% 
  ggplot(aes(intervention_year, percent_change)) +
  geom_point() +
  facet_wrap(~indicator,scales = "free_y") + theme_bw() + xlab("Intervention Year") +
  ylab("Change from baseline (%)")



# plot each individually

# cumulative_outputs %>% 
#   filter(
#     indicator == "TotalNewHIV",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("TotalNewHIV") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "TotalHIV",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("TotalHIV") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "TotalHIVtests",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("TotalHIVtests")
# 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "LYlostAIDS",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("LYlostAIDS") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "AIDSdeathsAdultF",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("AIDSdeathsAdultF") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "AIDSdeathsAdultM",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("AIDSdeathsAdultM") 
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "DiagnosedHIV_F",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("DiagnosedHIV_F")
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "DiagnosedHIV_M",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("DiagnosedHIV_M")
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "UndiagnosedHIV_F",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("UndiagnosedHIV_F")
# 
# cumulative_outputs %>% 
#   filter(
#     indicator == "UndiagnosedHIV_M",
#   ) %>%
#   ggplot(aes(intervention_year, cumulative, color = scenario)) +
#   geom_point() + geom_line() + ggtitle("UndiagnosedHIV_M")



