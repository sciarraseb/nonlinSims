#' Scales times values according to the designated time period (new comment).
#'
#' @param time_period number of days over which change occurs
#' @param num_time_points number of time points
#' @return Returns a data table.
#' @export
compute_mult_simulations <- function(num_iterations, pop_params, cov_matrix, schedule, response_group_size){
  
  results <- parallel::mclapply(X = 1:num_iterations, FUN = compute_ind_simulation, 
                      pop_params = pop_params, 
                      cov_matrix = cov_matrix, 
                      schedule = schedule,
                      response_group_size = 30, 
                      mc.cores = parallel::detectCores() - 1)
  
  #extract column names first
  col_names <- names(results[[1]])
  
  results <- data.table(matrix(unlist(results, use.names = T), ncol = length(results[[1]]), byrow = T))

  #assign column names
  names(results) <- col_names
  
  return(results)

}

compute_ind_simulation <- function(num_iterations, pop_params, cov_matrix, schedule, response_group_size){
  
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
  
  names(data_wide)[-1] <- generate_manifest_var_names(data_wide)
  
  
  #create placeholder model  
  latent_growth_model <- create_logistic_growth_model(data_wide = data_wide, model_name = 'lg_nonlinear')
  
  #find good starting values 
  latent_growth_model <- mxAutoStart(model = latent_growth_model)
  
  #run model with 10 different sets of starting values 
  model_results <- mxTryHard(latent_growth_model)
  
  #indicate model status 
  analysis_output <- c('code' = model_results$output$status$status,
                       'status' = model_results$output$status$code, 
                       'minus_2_likelihood' = model_results$output$Minus2LogLikelihood)
                              

  return(analysis_output)
}