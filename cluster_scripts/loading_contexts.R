source(didehpc_config.R)

packages <- "ggplot2"
library(ggplot2)
library("dplyr")
library("rstan")


ctx <- context::context_save("contexts", packages = packages)
obj <- didehpc::queue_didehpc(context = ctx)

obj$install_packages("ggplot2")


t <- obj$enqueue(packageVersion("ggplot2"))
t$wait(10)

