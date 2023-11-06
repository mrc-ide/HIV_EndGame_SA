#### Single age outputs ####

age_outputs <- read.delim("OutputByAge.txt", header=FALSE) # read txt file
t_age_outputs <- t(age_outputs) # transpose
age_outputs_new <- as.data.frame(as_tibble(t_age_outputs)) # covert to dataframe
age_outputs_new <- age_outputs_new[-117,] # remove last empty row
age_outputs_new$year <- seq(1985,2100,1) # add year column

indicators <- c("MalePop", "FemPop", "MaleInc", "FemInc", "MalePrev",
                "FemPrev", "MaleMort", "FemMort", "MaleDiag", "FemDiag",
                "MaleART", "FemART", "MaleAIDSdeaths", "FemAIDSdeaths")

# The columns in the dataframe have single-age values for each indicator
# There are 91 single ages for all indicators (except "MaleAIDSdeaths" & "FemAIDSdeaths")
# e.g. column 1 is MalePop aged 0, and column 91 is MalePop aged 90 years
# "MaleAIDSdeaths" & "FemAIDSdeaths" have 92 single-ages. Might be to include stillbirths
# each observation is the mean of all parameter sets 


# Would like to convert dataframe to a long format 
# Columns: year, indicator, age, value (maybe sex too)

