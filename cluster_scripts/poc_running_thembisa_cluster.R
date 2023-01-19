library(didehpc)
workdir <- "Q:/Git/HIV_EndGame_SA" 

mrc_config <- didehpc::didehpc_config(credentials = "spr21", workdir=workdir)
packages = c("tidyr", "dplyr", "here", "ggplot2", "readr", "purrr")
sources <- c("scripts/modify_rollout.R", "R/read_and_run.R", "R/support_modify_inputs.R", "scripts/cluster_function.R")
ctx <- context::context_save("contexts", packages = packages, sources = sources)
mrcq <- didehpc::queue_didehpc(ctx, config=mrc_config)
j <- mrcq$enqueue(here())
j$wait(10)
x <- mrcq$enqueue(run_on_cluster(pitc_reduction_years = c(2025, 2099), pitc_reduction_percentage = c(0,100), 
                                   condom_usage_reduction = FALSE, 
                                   fsw_condom_usage_decrease = 0, st_condom_usage_decrease = 0, lt_condom_usage_decrease = 0,
                                   condom_incr_years = seq(2035, 2045, 1), condom_maintenance_years = seq(2046, 2100, 1),
                                   art_coverage_increase = FALSE,
                                   art_interrupt_rate_decrease = 0, art_incr_years = seq(2025, 2100, 1),
                                   cumulative_years = 50))
x$log()
x$wait(120)
x$result()
x$status()

s <- mrcq$enqueue(sessionInfo()) 
s$wait(10)
h <- mrcq$enqueue(packageVersion("ggplot2"))
h$wait(10)