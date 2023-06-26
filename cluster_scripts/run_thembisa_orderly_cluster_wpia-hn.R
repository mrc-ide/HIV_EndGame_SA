library(orderly)
library(dplyr)
library(tidyr)
library(purrr)
library(parallel)

# check that thembise.exe is up to date
setwd("M:/HIV_EndGame_SA/THEMBISAv18")
system("compile.bat")
system("./thembisa.exe")

# copy this version of thembisa.exe into the orderly src

# move into orderly directory and check
workdir <- "M:/HIV_EndGame_SA/orderly/thembisa_orderly/src/thembisa"
setwd(workdir)
getwd()

# # check that orderly works where you are
# orderly::orderly_run(parameters = list(pitc_reduction_years = 2025,
#                                        pitc_reduction_percentage = 5,
#                                        condom_usage_reduction = FALSE,
#                                        condom_usage_decrease = 0,
#                                        condom_decr_start = 2020,
#                                        condom_usage_promotion = TRUE,
#                                        condom_incr_start = 2025,
#                                        condom_usage_increase = 1,
#                                        art_coverage_increase = FALSE,
#                                        art_interrupt_rate_decrease = 0,
#                                        art_incr_start= 2020,
#                                        art_coverage_decrease = FALSE,
#                                        art_interrupt_rate_increase = 2020,
#                                        art_decr_start = 0,
#                                        cumulative_years = 50,
#                                        summary_name = "summary"))


task <- "thembisa"
root <- "M:/HIV_EndGame_SA/orderly/thembisa_orderly/contexts"
orderly_root <- "M:/HIV_EndGame_SA/orderly/thembisa_orderly"
path_bundles <- file.path(orderly_root, "bundles")
output_path <- "output"

#### CONDOM REDUCTION ####

# parameter values - change for each run

pitc_reduction_years<- 2030
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- TRUE
condom_usage_decrease <- seq(0, 14, 0.5)
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

# parameter values - change for each run

pitc_reduction_years<- 2035
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- TRUE
condom_usage_decrease <- seq(0, 14, 0.5)
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
bundle2 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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


# parameter values - change for each run

pitc_reduction_years<- 2040
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- TRUE
condom_usage_decrease <- seq(0, 14, 0.5)
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
bundle3 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2045
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- TRUE
condom_usage_decrease <- seq(0, 14, 0.5)
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
bundle4 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2050
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- TRUE
condom_usage_decrease <- seq(0, 14, 0.5)
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
bundle5 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

#### CONDOM PROMOTION ####

pitc_reduction_years<- 2030
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- TRUE
condom_usage_increase <- seq(0, 14, 0.5)
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
bundle6 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2035
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- TRUE
condom_usage_increase <- seq(0, 14, 0.5)
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
bundle7 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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


# parameter values - change for each run

pitc_reduction_years<- 2040
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- TRUE
condom_usage_increase <- seq(0, 14, 0.5)
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
bundle8 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2045
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- TRUE
condom_usage_increase <- seq(0, 14, 0.5)
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
bundle9 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2050
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- TRUE
condom_usage_increase <- seq(0, 14, 0.5)
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
bundle10 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

#### ART COVERAGE  INCREASE ####

pitc_reduction_years<- 2030
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- TRUE
art_interrupt_rate_decrease <- seq(0, 14, 0.5)
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
bundle11 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2035
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- TRUE
art_interrupt_rate_decrease <- seq(0, 14, 0.5)
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
bundle12 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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


# parameter values - change for each run

pitc_reduction_years<- 2040
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- TRUE
art_interrupt_rate_decrease <- seq(0, 14, 0.5)
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
bundle13 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2045
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- TRUE
art_interrupt_rate_decrease <- seq(0, 14, 0.5)
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
bundle14 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2050
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- TRUE
art_interrupt_rate_decrease <- seq(0, 14, 0.5)
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
bundle15 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

#### ART COVERAGE  DECREASE ####

pitc_reduction_years<- 2030
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0
art_incr_start <- 2025
art_coverage_decrease <- TRUE
art_interrupt_rate_increase <- seq(0, 14, 0.5) 
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
bundle16 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2035
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0
art_incr_start <- 2025
art_coverage_decrease <- TRUE
art_interrupt_rate_increase <- seq(0, 14, 0.5) 
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
bundle17 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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


# parameter values - change for each run

pitc_reduction_years<- 2040
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0
art_incr_start <- 2025
art_coverage_decrease <- TRUE
art_interrupt_rate_increase <- seq(0, 14, 0.5) 
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
bundle18 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2045
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0
art_incr_start <- 2025
art_coverage_decrease <- TRUE
art_interrupt_rate_increase <- seq(0, 14, 0.5) 
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
bundle19 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# parameter values - change for each run

pitc_reduction_years<- 2050
pitc_reduction_percentage <- seq(0, 100, 5)
condom_usage_reduction <- FALSE
condom_usage_decrease <- 0
condom_incr_start <- 2025
condom_usage_promotion <- FALSE
condom_usage_increase <- 0
condom_decr_start <- 2025
art_coverage_increase <- FALSE
art_interrupt_rate_decrease <- 0
art_incr_start <- 2025
art_coverage_decrease <- TRUE
art_interrupt_rate_increase <- seq(0, 14, 0.5) 
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
bundle20 <- lapply(X = seq_len(nrow(pars)), FUN = function(i){
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

# delete output folder from src before packing bundles
unlink("output", recursive = TRUE)

# set up config, packages, sources, creating context and making queue object
share <- didehpc::path_mapping("Stefan", "M:", "//wpia-hn.hpc.dide.ic.ac.uk/hiv-inference/Stefan", "M:")
config <- didehpc::didehpc_config(credentials = "spr21",template = "AllNodes", cluster = "wpia-hn", shares = share, cores = 1)
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
# now running bundle18
t1 <- Sys.time()
paths <- lapply(bundle18, function(x) {
  paste(last(strsplit(dirname(x$path), "/")[[1]]), 
        basename(x$path), sep = "/")
})
paths <- lapply(paths, function(x){
  file.path(orderly_root, x)
})

# send orderly tasks to cluster
t <- obj$lapply(paths, orderly::orderly_bundle_run, workdir = output_path)
Sys.sleep(125)
browseURL("https://youtu.be/1iYEwFDLVnM?t=218")
batch_name <- t$name
# results
obj$task_bundle_get(batch_name)$results()
# status
t$status()
which(t$status()=="RUNNING")
length(which(t$status()=="ERROR"))
# look at logs of tasks
tasks <- t$tasks
# check log of specific tasks - task 1 below
tasks[[609]]$log()
complete <- which(t$status()=="COMPLETE")
t2 <- Sys.time()
Sys.sleep(760)
browseURL("https://youtu.be/1iYEwFDLVnM?t=218")
# import to archive
for (output in t$wait(100)[complete]) {
  out <- strsplit(output$path, "\\\\")[[1]]
  output_filename <- out[length(out)]
  orderly::orderly_bundle_import(file.path(workdir, output_path, output_filename),
                                 root = orderly_root)
}
browseURL("https://youtu.be/prZ4RWWku7Y?t=217")
# find all the most recent tasks in archive that meet criteria in parameters
# can change pars to include multiple future variability 
# t1 <- Sys.time()
# archive_task_names <- unlist(lapply(seq_len(nrow(pars)), function(i) {
#   orderly::orderly_search(name = "thembisa", 
#                           query = "latest(parameter:pitc_reduction_years == pitc_reduction_years && 
#                           parameter:pitc_reduction_percentage == pitc_reduction_percentage &&  
#                           parameter:condom_usage_reduction == condom_usage_reduction &&
#                           parameter:condom_usage_decrease == condom_usage_decrease &&
#                           parameter:condom_decr_start == condom_incr_start &&
#                           parameter:condom_usage_promotion == condom_usage_promotion &&
#                           parameter:condom_usage_increase == condom_usage_increase &&
#                           parameter:condom_incr_start == condom_incr_start &&
#                           parameter:art_coverage_increase == art_coverage_increase &&
#                           parameter:art_interrupt_rate_decrease == art_interrupt_rate_decrease &&
#                           parameter:art_incr_start == art_incr_start &&
#                           parameter:art_coverage_decrease == art_coverage_decrease &&
#                           parameter:art_interrupt_rate_increase == art_interrupt_rate_increase &&
#                           parameter:art_decr_start == art_decr_start &&
#                           parameter:cumulative_years == cumulative_years &&
#                           parameter:summary_name == summary_name)",
#                           parameters = list(pitc_reduction_years = pars$pitc_reduction_years[i],
#                                             pitc_reduction_percentage = pars$pitc_reduction_percentage[i],
#                                             condom_usage_reduction = pars$condom_usage_reduction[i],
#                                             condom_usage_decrease = pars$condom_usage_decrease[i],
#                                             condom_decr_start = pars$condom_incr_start[i],
#                                             condom_usage_promotion = pars$condom_usage_promotion[i],
#                                             condom_usage_increase = pars$condom_usage_increase[i],
#                                             condom_incr_start = pars$condom_incr_start[i],
#                                             art_coverage_increase = pars$art_coverage_increase[i],
#                                             art_interrupt_rate_decrease = pars$art_interrupt_rate_decrease[i],
#                                             art_incr_start = pars$art_incr_start[i],
#                                             art_coverage_decrease = pars$art_coverage_decrease[i],
#                                             art_interrupt_rate_increase = pars$art_interrupt_rate_increase[i],
#                                             art_decr_start = pars$art_decr_start[i],
#                                             cumulative_years = pars$cumulative_years[i],
#                                             summary_name = pars$summary_name[i])
#   )}))
# t2 <- Sys.time()
# t3 <- t2 - t1
t1 <- Sys.time()
archive_task_names <- list.files("M:/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa")
# combine the summary csvs
#filepaths1 <- paste0("M:/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa/", archive_task_names[1:7], "/results/summary.csv")
filepaths2 <- paste0("M:/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa/", archive_task_names, "/pack/results/summary.csv")
temp <- lapply(filepaths2, read.csv)
#temp2 <- lapply(filepaths2, read.csv)
#temp <- append(temp1, temp2)
names(temp) <- archive_task_names[which(!is.na(archive_task_names))]  
combined_summary <- bind_rows(temp, .id = "task_name")

# check type of scenario and use for csv name
scenario_name <- unique(combined_summary$future_variability)
year_name <- unique(combined_summary$pitc_reduction_year)
csv_name <- paste0(scenario_name, sep = "_", year_name)

# calculate HIV elimination year and incidence in 2100
# inc_and_elim <- find_inc_and_elimination(combined_summary)

# combine the cumulative csvs
#filepaths1 <- paste0("M:/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa/", archive_task_names[1:336], "/results/cumulative_summary.csv")
filepaths2 <- paste0("M:/HIV_EndGame_SA/orderly/thembisa_orderly/archive/thembisa/", archive_task_names, "/pack/results/cumulative_summary.csv")
#temp1 <- lapply(filepaths1, read.csv)
temp2 <- lapply(filepaths2, read.csv)
#temp <- append(temp1, temp2)
names(temp2) <- archive_task_names  
combined_cumulative <- bind_rows(temp2, .id = "task_name")

# save csvs in network drive

csv_cumulative <- paste0("H:/ordely_outputs/", csv_name, "_cumulative.csv")
# csv_inc_elim <- paste0("H:/ordely_outputs/", csv_name, "_inc_elim.csv")
csv_summary <- paste0("H:/ordely_outputs/", csv_name, "_summary.csv")


write_csv(combined_summary, csv_summary)
# write_csv(inc_and_elim, csv_inc_elim)
write_csv(combined_cumulative, csv_cumulative)
t2 <- Sys.time()
t3 <- t2 - t1
browseURL("https://youtu.be/prZ4RWWku7Y?t=217")


