#' Tests effectiveness of starting value procedure. 
#'
#' @param factor_list 
#' @param num_iterations number of time points
#' @param pop_params 
#' @param xresponse_group_size
#' @return  Returns a data table
#' @export
test_convergence <- function(factor_list, num_iterations, pop_params, response_group_size, num_cores) {
  
  #compute experiment conditions
  exp_conditions <- data.table(expand.grid(factor_list))
  perc_converge_total <- rep(x = NA, times = nrow(exp_conditions))
  

    #test convergence in each condition
  for (condition in 1:nrow(exp_conditions)) {
    
    perc_converge <- mclapply(X = num_iterations, 
                                    FUN = test_condition, 
                                    pop_params = pop_params, 
                                    response_group_size = response_group_size, 
                              
                                    num_measurements = exp_conditions$num_measurements[condition], 
                                    measurement_spacing = exp_conditions$spacing[condition], 
                                    midpoint_value = exp_conditions$midpoint[condition],
                                    mc.cores = 3)

    perc_converge_total[condition] <- perc_converge
  
  }
  
  exp_conditions$perc_converge <- perc_converge_total
  
  return(exp_conditions)
}

test_condition <- function(num_iterations, pop_params, response_group_size, 
                           num_measurements, measurement_spacing, midpoint_value) {
  
  num_measurements <- num_measurements
  
  
  #setup of population parameters
  pop_params$beta_fixed <- midpoint_value
  
  schedule <- compute_measurement_schedule(time_period = 360, 
                                           num_measurements =  num_measurements, 
                                           base_time_length = 30, 
                                           measurement_spacing = measurement_spacing)
  
  cov_matrix <- generate_three_param_cov_matrix(num_time_points = num_measurements, pop_param_list = pop_params)
  
  #test convergence in each condition
  convergence_results <- as.data.table(lapply(X = num_iterations, FUN = run_simulations,
                                pop_params = pop_params,
                                cov_matrix = cov_matrix, 
                                schedule = schedule, 
                                response_group_size = response_group_size))
  
  perc_converge <-  (sum(convergence_results$code == 0)/num_iterations) * 100
  
  return(perc_converge)
}
