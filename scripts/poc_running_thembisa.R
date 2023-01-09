
library(here)
## setwd(here("THEMBISAv18")) 
system("g++ -std=c++14 THEMBISAv18/THEMBISA.cpp THEMBISAv18/StatFunctions.cpp THEMBISAv18/mersenne.cpp -o THEMBISAv18/thembisa -O2") 
system("./THEMBISAv18/thembisa")
setwd(here())
# homedir <- "/Volumes/jwe08" 
workdir <- "Q:/Git/HIV_EndGame_SA" 
# dir.create(workdir)
# file.copy(".", workdir, recursive = TRUE, overwrite = TRUE) 
mrc_config <- didehpc::didehpc_config(credentials = "spr21", workdir=workdir, cluster="mrc")
ctx <- context::context_save(path = workdir, packages = c("dplyr", "here", "ggplot2", "gridExtra"), sources = "R/read_and_run.R")
mrcq <- didehpc::queue_didehpc(ctx, config=mrc_config)
s <- mrcq$enqueue(sessionInfo()) 
h <- mrcq$enqueue(here::here()) 
t <- mrcq$enqueue(run_thembisa()) 
d <- mrcq$enqueue(system("g++ -std=c++14 THEMBISA.cpp StatFunctions.cpp mersenne.cpp -o thembisa -O2")) 
d$wait(10)
