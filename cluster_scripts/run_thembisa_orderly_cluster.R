library(orderly)
library(dplyr)
library(tidyr)
library(purrr)

# check that thembise.exe is up to date
# setwd("Q:/Git/HIV_EndGame_SA/THEMBISAv18")
# system("compile.bat")
# system("./thembisa.exe")

# copy this version of thembisa.exe into the orderly src

# move into orderly directory and check
workdir <- "Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa"
setwd(workdir)
getwd()

# check that orderly works where you are
orderly::orderly_run(parameters = list(pitc_reduction_years = 2025,
                                       pitc_reduction_percentage = 0,
                                       condom_usage_reduction = FALSE,
                                       condom_usage_decrease = 0,
                                       condom_decr_start = 2020,
                                       condom_usage_promotion = TRUE,
                                       condom_incr_start = 2025,
                                       condom_usage_increase = 1,
                                       art_coverage_increase = FALSE,
                                       art_interrupt_rate_decrease = 0,
                                       art_incr_start= 2020,
                                       art_coverage_decrease = FALSE,
                                       art_interrupt_rate_increase = 2020,
                                       art_decr_start = 0,
                                       cumulative_years = 50,
                                       summary_name = "summary"))


task <- "thembisa"
root <- "Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly/contexts"
orderly_root <- "Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly"
path_bundles <- file.path(orderly_root, "bundles")
output_path <- "output"

# parameter values - change for each run

pitc_reduction_years<- c(2025)
pitc_reduction_percentage <- seq(0,100,10)
condom_usage_reduction <- TRUE
condom_usage_decrease <- seq(0,7,1)
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0 
art_incr_start <- 2025
art_coverage_decrease <- FALSE
art_interrupt_rate_increase <- 0
art_decr_start <- 2025
cumulative_years <- 50
summary_name <- "summary"

# make a dataframe of all combinations of parameter values

pars <- expand_grid(pitc_reduction_years, 
                    pitc_reduction_percentage, 
                    condom_usage_reduction,
                    condom_usage_decrease,
                    condom_decr_start,
                    condom_usage_promotion,
                    condom_incr_start,
                    condom_usage_increase,
                    art_coverage_increase,
                    art_interrupt_rate_decrease,
                    art_incr_start,
                    art_coverage_decrease,
                    art_interrupt_rate_increase,
                    art_decr_start,
                    cumulative_years,
                    summary_name)

# pack up bundles 
bundle <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
  orderly::orderly_bundle_pack(
    path_bundles,
    task,
    parameters = list(pitc_reduction_years = pars$pitc_reduction_years[i],
                      pitc_reduction_percentage = pars$pitc_reduction_percentage[i],
                      condom_usage_reduction = pars$condom_usage_reduction[i],
                      condom_usage_decrease = pars$condom_usage_decrease[i],
                      condom_decr_start = pars$condom_decr_start[i],
                      condom_usage_promotion = pars$condom_usage_promotion[i],
                      condom_usage_increase = pars$condom_usage_increase[i],
                      condom_incr_start = pars$condom_incr_start[i],
                      art_coverage_increase = pars$art_coverage_increase[i],
                      art_interrupt_rate_decrease = pars$art_interrupt_rate_decrease[i],
                      art_incr_start = pars$art_incr_start[i],
                      art_coverage_decrease = pars$art_coverage_decrease[i],
                      art_interrupt_rate_increase = pars$art_interrupt_rate_increase[i],
                      art_decr_start = pars$art_decr_start[i],
                      cumulative_years = pars$cumulative_years[i],
                      summary_name = pars$summary_name[i]),
    root = orderly_root)})



# set up config, packages, sources, creating context and making queue object
config <- didehpc::didehpc_config(credentials = "spr21",cluster = "small")
packages = c("tidyr", "dplyr","ggplot2", "readr", "purrr", "orderly")
sources <- c("R/modify_rollout_orderly.R", "R/read_and_run_orderly.R", "R/support_modify_inputs_orderly.R", "R/cluster_function_orderly.R")
ctx <- context::context_save(path = root, packages = packages, sources = sources)
obj <- didehpc::queue_didehpc(context = ctx, config = config)

# check that context is set up
h <- obj$enqueue(packageVersion("ggplot2"))
h$log()
h$status()
h$result()

# fetch bundles paths
paths <- lapply(bundle, function(x) {
  paste(last(strsplit(dirname(x$path), "/")[[1]]), 
        basename(x$path), sep = "/")
})
paths <- lapply(paths, function(x){
  file.path(orderly_root, x)
})

# send orderly tasks to cluster
t <- obj$lapply(paths, orderly::orderly_bundle_run, workdir = output_path)
batch_name <- t$name
# results
obj$task_bundle_get(batch_name)$results()
# status
t$status()
which(t$status()=="RUNNING")
# look at logs of tasks
tasks <- t$tasks
# check log of specific tasks - task 1 below
tasks[[4]]$log()
t$status()
# import to archive
for (output in t$wait(100)[]) {
  out <- strsplit(output$path, "\\\\")[[1]]
  output_filename <- out[length(out)]
  orderly::orderly_bundle_import(file.path(workdir, output_path, output_filename),
                                 root = orderly_root)
}

# find all the most recent tasks in archive that meet criteria in parameters
# can change pars to include multiple future variability 

archive_task_names <- unlist(lapply(seq_len(nrow(pars)), function(i) {
  orderly::orderly_search(name = "thembisa", 
                          query = "latest(parameter:pitc_reduction_years == pitc_reduction_years && 
                          parameter:pitc_reduction_percentage == pitc_reduction_percentage &&  
                          parameter:condom_usage_reduction == condom_usage_reduction &&
                          parameter:condom_usage_decrease == condom_usage_decrease &&
                          parameter:condom_decr_start == condom_incr_start &&
                          parameter:condom_usage_promotion == condom_usage_promotion &&
                          parameter:condom_usage_increase == condom_usage_increase &&
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
                                            condom_decr_start = pars$condom_incr_start[i],
                                            condom_usage_promotion = pars$condom_usage_promotion[i],
                                            condom_usage_increase = pars$condom_usage_increase[i],
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

sum(is.na(archive_task_names))
archive_task_names <- archive_task_names[which(!is.na(archive_task_names))]
# combine the summary csvs
filepaths <- paste0("Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa/", archive_task_names, "/results/summary.csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- archive_task_names  
combined_summary <- bind_rows(temp, .id = "task_name")

# check type of scenario and use for csv name
csv_name <- unique(combined_summary$future_variability)

# calculate HIV elimination year and incidence in 2100
inc_and_elim <- find_inc_and_elimination(combined_summary)

# combine the cumulative csvs
filepaths <- paste0("Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa/", archive_task_names, "/results/cumulative_summary.csv")
temp <- lapply(filepaths, read.csv)
names(temp) <- archive_task_names  
combined_cumulative <- bind_rows(temp, .id = "task_name")

# save csvs in network drive

csv_cumulative <- paste0("H:/ordely_outputs/", csv_name, "_cumulative.csv")
csv_inc_elim <- paste0("H:/ordely_outputs/", csv_name, "_inc_elim.csv")
csv_summary <- paste0("H:/ordely_outputs/", csv_name, "_summary.csv")


write_csv(combined_summary, csv_summary)
write_csv(inc_and_elim, csv_inc_elim)
write_csv(combined_cumulative, csv_cumulative)


