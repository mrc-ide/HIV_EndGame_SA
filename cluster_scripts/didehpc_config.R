# install.packages("drat") # installer for adding "mrc-ide"
# drat:::add("mrc-ide")
# install.packages("didehpc")
library(didehpc) # load the didehpc support package

# to edit the .rprofile load the following package 
library(usethis)
# run this function 

# usethis::edit_r_profile()
# add the following text to the .Rprofile, save and restart Rstudio
# options(
#   didehpc.username = "spr21",
#   didehpc.home = "H:/HIV_EndGame_SA"
# )


# To see the configuration that will be run if you don't do anything (else), run:

config <- didehpc::didehpc_config(credentials = list(username = "spr21", password = "Wednesday123!"))
# config
