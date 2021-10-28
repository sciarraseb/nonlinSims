#' Scales times values according to the designated time period (new comment).
#'
#' @param time_period number of days over which change occurs
#' @param num_time_points number of time points
#' @return Returns a data table.
#' @export
compute_mult_simulations <- function(num_sims, pop_params, cov_matrix, schedule, starting_values, response_group_size){
  
  results <- list()
  
  for (simulation in 1:num_sims) {
    results[[simulation]] <- compute_ind_simulation(pop_params = pop_params, 
                                                     cov_matrix = cov_matrix, 
                                                     schedule = schedule, 
                                                     response_group_size = response_group_size)
  
  }
  
  results <- data.table(matrix(data = unlist(results), 
                               ncol=length(unlist(results[1])), 
                               byrow = T, 
                               dimnames = list(1:length(results), 
                                               names(results[[1]]))))
  
  return(results)

}

compute_ind_simulation <- function(pop_params, cov_matrix, schedule, response_group_size){
  
  #setup variables
  num_measurements <- ncol(cov_matrix) - 3
  
  param_table <- generate_ind_param_values(pop_param_list = pop_params, 
                                           response_group_size = response_group_size, 
                                           num_time_points = num_measurements, 
                                           cov_matrix = cov_matrix)
  
  
  data <- generate_group_scores(num_measurements = num_measurements, 
                                param_table = param_table,measurement_days = schedule$measurement_days, 
                                time_period = 360, scaling_constant = 3)
  
  data_wide <- pivot_wider(data = data[ ,1:3], names_from = 'measurement_day', values_from = 'obs_score', 
                           names_prefix = sprintf('t%d_', 1:uniqueN(data$measurement_day)))
  
  
  #generate starting values 
  starting_values <- generate_multiple_starting_value_sets(num_sets = 1, data = data, data_wide = data_wide)
  
  #create placeholder model  
  latent_growth_model <- create_logistic_growth_model(data_wide = data_wide, 
                                                      model_name = 'lg_nonlinear', starting_values = starting_values[1, ])
  
  #find good starting values 
  latent_growth_model <- mxAutoStart(model = latent_growth_model)
  
  model_results <- mxTryHard(latent_growth_model)
  
  analysis_output <- list('status' = model_results$output$status$code, 
                              'code' = model_results$output$status$status, 
                              'minus_2_likelihood' = model_results$output$Minus2LogLikelihood)
                              


  return(analysis_output)
}