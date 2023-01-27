library(didehpc)
workdir <- "Q:/Git/HIV_EndGame_SA" 

mrc_config <- didehpc::didehpc_config(credentials = "spr21", workdir=workdir)
packages = c("tidyr", "dplyr", "here", "ggplot2", "readr", "purrr")
sources <- c("scripts/modify_rollout.R", "R/read_and_run.R", "R/support_modify_inputs.R", "scripts/cluster_function.R")
ctx <- context::context_save("contexts", packages = packages, sources = sources)
mrcq <- didehpc::queue_didehpc(ctx, config=mrc_config)
j <- mrcq$enqueue(here())
j$wait(10)

pitc_reduction_years <- c(2025, 2030, 2035, 2040, 2045, 2050, 2099)
pitc_reduction_percentage <- seq(0, 100, 10) 
condom_usage_reduction <- FALSE 
fsw_condom_usage_decrease <- 0.14
st_condom_usage_decrease <- 0.14
lt_condom_usage_decrease <- 0.0475
condom_incr_start <- 2035
art_coverage_increase = FALSE
art_interrupt_rate_decrease = 0
art_incr_start = 2025
cumulative_years <- 50
summary <- "summary"

pars <- expand_grid(pitc_reduction_years, pitc_reduction_percentage, 
                    condom_usage_reduction, 
                    fsw_condom_usage_decrease, st_condom_usage_decrease, lt_condom_usage_decrease,
                    condom_incr_start, 
                    art_coverage_increase,
                    art_interrupt_rate_decrease,art_incr_start,
                    cumulative_years, summary)

for (i in 1:length(pars$summary)){
  name <- paste0("summary_", i)
  pars$summary[i] <- name
} 


x <- mrcq$enqueue_bulk(pars, run_on_cluster) 
x$log()
x$wait(120)
x$result()
x$status()

s <- mrcq$enqueue(sessionInfo()) 
s$wait(10)
h <- mrcq$enqueue(packageVersion("ggplot2"))
h$wait(10)