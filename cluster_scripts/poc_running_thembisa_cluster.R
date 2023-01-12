
workdir <- "Q:/Git/HIV_EndGame_SA" 

mrc_config <- didehpc::didehpc_config(credentials = "spr21", workdir=workdir, cluster="mrc")
packages = c("tidyr", "dplyr", "here", "ggplot2", "gridExtra", "readr")
sources <- c("scripts/modify_rollout.R", "R/read_and_run.R", "R/support_modify_inputs.R")
ctx <- context::context_save("contexts", packages = packages, sources = sources)
mrcq <- didehpc::queue_didehpc(ctx, config=mrc_config)
s <- mrcq$enqueue(sessionInfo()) 
s$log()
s$wait(10)
h <- mrcq$enqueue(packageVersion("ggplot2"))
h
h$wait(10)
j <- mrcq$enqueue(here())
j
j$log()
j$wait(10)
t <- mrcq$enqueue(run_thembisa())
t$log()
t$wait(60)


