workdir <- "Q:/Git/HIV_EndGame_SA/THEMBISAv18" 
setwd(workdir)
library(didehpc)
getwd()
system("compile.bat")
system("./thembisa.exe")
mrc_config <- didehpc::didehpc_config(credentials = "spr21", workdir=workdir)
packages = c("tidyr", "dplyr","ggplot2", "readr", "purrr")
sources <- c("modify_rollout_cluster.R", "read_and_run_cluster.R", "support_modify_inputs_cluster.R", "cluster_function_thembisav18.R")
ctx <- context::context_save("contexts", packages = packages, sources = sources)
mrcq <- didehpc::queue_didehpc(ctx, config=mrc_config)
h <- mrcq$enqueue(packageVersion("ggplot2"))
h$wait(10)
# j <- mrcq$enqueue(system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa"))
# j$status()
# j$log()
# 
k <- mrcq$enqueue(system("./thembisa.exe"))
k$status()
k$log()


# 
x<- mrcq$enqueue(run_thembisa())
x$status()
x$log()

pitc_reduction_years<- c(2025)
pitc_reduction_percentage <- c(0)
condom_usage_reduction <- FALSE
fsw_condom_usage_decrease <- 0
st_condom_usage_decrease <- 0
lt_condom_usage_decrease <- 0
condom_incr_start <- 2025
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0 
art_incr_start <- 2025
art_coverage_decrease <- FALSE
art_interrupt_rate_increase <- 0
art_decr_start <- 2025
cumulative_years <- 50
summary_name <- "summary_concept"

summary_test <- read.csv("results/summary_test.csv")

pars <- expand_grid(pitc_reduction_years, 
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
                    cumulative_years,
                    summary_name)

for (i in 1:length(pars$summary_name)){
  name <- paste0("summary_name", i)
  pars$summary_name[i] <- name
} 

x <- mrcq$enqueue_bulk(pars, run_on_cluster) 
x$status()

filepaths <- paste0("results/", pars$summary_name, ".csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- seq(1:length(pars$summary_name))
combined_summary <- bind_rows(temp, .id = "scenario_number")

combined_summary %>% mutate(intervention_year = as.factor(pitc_reduction_year)) %>% 
  filter(pitc_reduction_year == 2025, indicator == "TotalHIVtests", year >= 2020) %>% 
  ggplot(aes(year, mean, group = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI, fill = scenario), alpha = 0.25, show.legend = F) +
  geom_line(aes(colour = scenario), show.legend = T) +
  xlab("") +
  scale_y_continuous("HIV tests (millions)", labels = (function(l) {round(l/1e6,1)})) +
  expand_limits(y=0) + theme_classic() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14)) +
  facet_wrap(vars(test_reduction))

h <- mrcq$enqueue(packageVersion("ggplot2"))
h$wait(10)