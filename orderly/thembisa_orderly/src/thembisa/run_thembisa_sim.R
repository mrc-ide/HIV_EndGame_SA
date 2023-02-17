source("R/modify_rollout_orderly.R")
source("R/support_modify_inputs_orderly.R")
# Create data dictionary and save to manually add names
rollout_dictionary <- create_data_dictionary(data)

run_on_cluster(pitc_reduction_years,
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