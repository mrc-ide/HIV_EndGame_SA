plot_result_over_time <- function(parameter, output_name, title_of_plot, final_year = 2070){
  # Import parameter txt
  parameter_txt <- paste(parameter, "txt", sep = ".")
  parameter_output <- read.delim(parameter_txt, header=FALSE)
  # make data frame for summary stats over time
  Years <- 1986:final_year
  mean_output <- rep(NA, length(Years))
  sd_output <- rep(NA, length(Years))
  upper_ci <- rep(NA, length(Years))
  lower_ci <- rep(NA, length(Years))
  summary_stats <- data.frame(Years, mean_output, sd_output, upper_ci, lower_ci)
  # calculate summary stats for each year 
  for (i in 3:(length(Years)+2)){
    summary_stats$mean_output[i-2] <- mean(parameter_output[,i])
    summary_stats$sd_output[i-2] <- sd(parameter_output[,i])
    summary_stats$upper_ci[i-2] <-  summary_stats$mean_output[i-2] + 
      qnorm(0.975)*summary_stats$sd_output[i-2]/sqrt(dim(parameter_output)[1])
    summary_stats$lower_ci[i-2] <-  summary_stats$mean_output[i-2] - 
      qnorm(0.975)*summary_stats$sd_output[i-2]/sqrt(dim(parameter_output)[1])
  }
  # display as plot with line for mean and shaded area for 95%CI
  ggplot(summary_stats, aes(Years, mean_output)) + 
    geom_line() + 
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.1) +
    theme_bw() + 
    ggtitle(title_of_plot) + 
    ylab(as.character(output_name)) 
}
# Example - uncomment to run
# plot_result_over_time(parameter = "MMC15to19", 
#                       output_name = "Number of circumcisions", 
#                       title_of_plot = "Number of MMC per year 15-19 yr olds")


edit_rollout <- function(parameter, new_values, starting_year=1986, final_year=2070){
  rollout <- as.matrix(read.table("Original_Rollout.txt", sep = "\n", strip.white = TRUE))

  # Edit value
  original_values <- strsplit(rollout[parameter], split = "\t")
  for (i in (starting_year-1985):(final_year-1984)){
    original_values[[1]][i] <- new_values
  }
  edited_values <- paste(original_values[[1]], collapse = "\t")
  # reassign edited values to rollout
  rollout[parameter] <- edited_values
  
  # Export the edited file
  write.table(rollout, "Rollout.txt", sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
}

# Example - uncomment to run

# edit_rollout(parameter = 128, starting_year = 2020, new_values = 0.7500)
# 
# # Compile and Run the C++ code
# # Then to check, plot it again
# 
# plot_result_over_time(parameter = "MMC15to19", 
#                       output_name = "Number of circumcisions", 
#                       title_of_plot = "Number of MMC per year 15-19 yr olds after change")


