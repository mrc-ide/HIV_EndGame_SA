# check that thembise.exe is up to date
setwd("Q:/Git/HIV_EndGame_SA/THEMBISAv18")
system("compile.bat")
system("./thembisa.exe")
# copy this version of thembisa.exe into the orderly src

# move into orderly directory and check
workdir <- "Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa"
setwd(workdir)
getwd()

# check that ordely works where you are
orderly::orderly_run(parameters = list(pitc_reduction_years = 2025, 
                     pitc_reduction_percentage = 100, 
                     condom_usage_reduction = FALSE,
                     condom_usage_decrease = 0,
                     condom_incr_start = 2020, 
                     art_coverage_increase = FALSE,
                     art_interrupt_rate_decrease = 0,
                     art_incr_start= 2020,
                     art_coverage_decrease = FALSE,
                     art_interrupt_rate_increase = 2020,
                     art_decr_start = 0,
                     cumulative_years = 50,
                     summary_name = "summary"))



orderly_config() 

task <- "thembisa"
root <- "Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa/contexts"
orderly_root <- "Q:/Git/HIV_EndGame_SA/orderly/thembisa_orderly"
path_bundles <- file.path(root, "bundles")
output_path <- "output"

# parameter values - change for each run

pitc_reduction_years<- c(2025)
pitc_reduction_percentage <- c(0,100)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
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
                    condom_incr_start, 
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
config <- didehpc::didehpc_config(credentials = "spr21", workdir=workdir,cluster = "big", template = "24Core", cores = 2)
packages = c("tidyr", "dplyr","ggplot2", "readr", "purrr", "orderly")
sources <- c("R/modify_rollout_orderly.R", "R/read_and_run_orderly.R", "R/support_modify_inputs_orderly.R", "R/cluster_function_orderly.R")
ctx <- context::context_save(path = workdir, packages = packages, sources = sources)
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
  file.path("contexts", x)
})

# send orderly tasks to cluster
t <- obj$lapply(paths, orderly::orderly_bundle_run, workdir = output_path)
batch_name <- t$name
# results
obj$task_bundle_get(batch_name)$results()
# status
t$status()
# look at logs of tasks
tasks <- t$tasks
# check log of specific tasks
tasks[[1]]

for (output in t$wait(100)) {
  out <- strsplit(output$path, "\\\\")[[1]]
  output_filename <- out[length(out)]
  orderly::orderly_bundle_import(file.path(workdir, output_path, output_filename),
                                 root = orderly_root)
}

