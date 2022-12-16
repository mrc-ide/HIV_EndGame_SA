source(didehpc_config.R)

packages <- "ggplot2"
library(ggplot2)
library("dplyr")
library("rstan")


ctx <- context::context_save("contexts")
obj <- didehpc::queue_didehpc(context = ctx)

obj$install_packages("ggplot2")


packageVersion("didehpc")
