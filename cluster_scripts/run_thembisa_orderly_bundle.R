library(orderly)
library(dplyr)
library(tidyr)
library(purrr)
library(parallel)


# THESE FILEPATHS NEED TO BE EDITED - here() doesn't work well in orderly

workdir <- "~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa"
setwd(workdir)
root <- "~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly/contexts"
orderly_root <- "~/Documents/HIV_EndGame_SA/orderly/thembisa_orderly"
path_bundles <- file.path(orderly_root, "bundles")


#### TESTING REDUCTION ####

# parameter values 

pitc_reduction_years<- 2025
pitc_reduction_percentage <- seq(0, 100, 25)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
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
cumulative_years_list <- 0
change_mmc <- FALSE
mmc_rel_rate <- FALSE
mmc_change_start <- FALSE
change_prep <- FALSE
prep_rel_rate <- FALSE
prep_change_start <- FALSE
summary_name <- "summary"

# make a dataframe of all combinations of parameter values

pars <- expand_grid(pitc_reduction_years, 
                    pitc_reduction_percentage,
                    condom_usage_reduction,
                    condom_usage_decrease,
                    condom_decr_start,
                    condom_usage_promotion,
                    condom_usage_increase,
                    condom_incr_start,
                    art_coverage_increase,
                    art_interrupt_rate_decrease,
                    art_incr_start,
                    art_coverage_decrease,
                    art_interrupt_rate_increase,
                    art_decr_start,
                    cumulative_years_list,
                    change_mmc,
                    mmc_rel_rate,
                    mmc_change_start,
                    change_prep ,
                    prep_rel_rate, 
                    prep_change_start,
                    summary_name)


# pack up bundle
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
                      cumulative_years_list = pars$cumulative_years_list[i],
                      change_mmc = pars$change_mmc[i],
                      mmc_rel_rate = pars$mmc_rel_rate[i],
                      mmc_change_start = pars$mmc_change_start[i],
                      change_prep = pars$mmc_change_start[i],
                      prep_rel_rate = pars$prep_rel_rate[i], 
                      prep_change_start =pars$prep_change_start[i],
                      summary_name = pars$summary_name[i]),
    root = orderly_root)})

# delete output folder from src before packing bundles
unlink("output", recursive = TRUE)

# fetch bundles paths

paths <- lapply(bundle, function(x) {
  paste(last(strsplit(dirname(x$path), "/")[[1]]), 
        basename(x$path), sep = "/")
})
paths <- lapply(paths, function(x){
  file.path(orderly_root, x)
})

# run bundle of tasks
lapply(paths, orderly::orderly_bundle_run, workdir = output_path)




