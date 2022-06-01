source("R/support_modify_inputs.R")

# Read in input in Thembisa format
data <- readLines("THEMBISAv18/Rollout.txt")

# Create data dictionary and save to manually add names
rollout_dictionary <- create_data_dictionary(data)

dir.create("data_dictionaries", FALSE, TRUE)
filepath <- "data_dictionaries/rollout.csv"
save_dictionary(rollout_dictionary, filepath)

# Manually add in descriptive names for each row of data

# Read in dictionary with data names
dictionary <- read.csv(filepath)


# Format data for easy modification in R
formatted_data <- format_data(data, dictionary)

## Make whatever changes to the input that you want here


## Convert back to Thembisa format
rollout <- convert_to_thembisa_format(formatted_data, data, dictionary)

dir.create("modified_inputs", FALSE, TRUE)
write(rollout, "modified_inputs/rollout.txt")

