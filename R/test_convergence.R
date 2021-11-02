#' Scales times values according to the designated time period (new comment).
#'
#' @param time_period number of days over which change occurs
#' @param num_time_points number of time points
#' @return Returns a data table.
#' @export
test_convergence <- function(factor_list, num_iterations, pop_params, response_group_size) {
  
  #compute experiment conditions
  exp_conditions <- expand.grid(factor_list)
  perc_converge <- c()
  
  for (condition in 1:nrow(exp_conditions)) {
    
    #setup variable
    num_measurements <- exp_conditions$num_measurements[condition]
    
    #setup of population parameters
    pop_params$beta_fixed <- exp_conditions$midpoint[condition]
    
    schedule <- compute_measurement_schedule(time_period = 360, 
                                             num_measurements =  num_measurements, 
                                             base_time_length = 30, 
                                             measurement_spacing = exp_conditions$spacing[condition])
    
    cov_matrix <- generate_three_param_cov_matrix(num_time_points = num_measurements, pop_param_list = pop_params)
  
    #test convergence in each condition
    convergence_results <- compute_mult_simulations(num_iterations = num_iterations, 
                                                   pop_params = pop_params,
                                                   cov_matrix = cov_matrix, 
                                                   schedule = schedule, 
                                                   response_group_size = response_group_size)
   
    perc_converge[condition] <- sum(convergence_results$code == 0)/num_iterations
  }
  
  exp_conditions$perc_converge <- perc_converge
  
  return(exp_conditions)
  
} 