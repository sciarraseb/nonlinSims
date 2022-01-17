#' Runs Monte Carlo simulations.
#'
#' @param num_iterations number of iterations 
#' @param pop_params list of population parameters 
#' @param schedule measurement schedule
#' @return Returns a data table.
#' @export
run_exp_simulation <- function(factor_list, num_iterations, pop_params, num_cores, seed) {
  
  #ensure reproducibility 
  set.seed(seed)
  RNGkind("L'Ecuyer-CMRG")
  
  #compute experiment conditions
  exp_conditions <- data.table(expand.grid(factor_list))
  exp_conditions$spacing <- as.character(exp_conditions$spacing)
  
  #list object that will contain data.tables 
  num_conditions <- nrow(exp_conditions)
  results_list <- vector(mode = 'list', length = num_conditions)
  
  #create table of time structuredness values if it is a manipulated variable
  if ('time_structuredness' %in% names(factor_list)) {
    
    ts_values <- generate_time_struc_table(
      satiation_point = list('time_structured' = NA, 'fast_response' = 4.32, 'slow_response' = 10.80), 
      satiation_value = list('time_structured' = NA, 'fast_response' = .80, 'slow_response' = .80))
  }
  
  
  #test convergence in each condition
  for (condition in 1:num_conditions) {
    
    #find time-structured condition 
    time_struc_condition <- exp_conditions$time_structuredness[condition]
    
    results_list[condition] <- mclapply(X = num_iterations, 
                                  FUN = run_condition_simulation, 
                                  pop_params = pop_params, 
                                  response_group_size = exp_conditions$sample_size[condition], 
                                  
                                  num_measurements = exp_conditions$num_measurements[condition], 
                                  measurement_spacing = exp_conditions$spacing[condition], 
                                  midpoint_value = exp_conditions$midpoint[condition],
                                  time_structuredness_values = ts_values[ts_values$response_rate == time_struc_condition, ], 
                                  
                                  mc.cores = num_cores, 
                                  mc.set.seed = T)
    }
  
  simulation_results <- rbindlist(l = results_list, use.names = T, fill = T)
  
  return(simulation_results)
}


generate_time_struc_table <- function(satiation_value, satiation_point) {
  
  ts_values <- data.table('response_rate' = c('time_structured', 'fast_response', 'slow_response'), 
                          'satiation_point' = c(NA, satiation_point$fast_response, satiation_point$slow_response), 
                          'satiation_value' = c(NA, satiation_value$fast_response, satiation_value$slow_response))
  
  return(ts_values)
}

run_condition_simulation <- function(num_iterations, pop_params, response_group_size, 
                                     num_measurements, measurement_spacing, midpoint_value, 
                                     time_structuredness_values) {
  
  #setup of population parameters
  schedule <- compute_measurement_schedule(time_period = 360, 
                                           num_measurements =  num_measurements, 
                                           smallest_int_length = 30, 
                                           measurement_spacing = measurement_spacing)
  
  #add beta value
  pop_params$beta_fixed <- midpoint_value 
  
  #generate covariance matrix with specified beta value
  cov_matrix <- generate_four_param_cov_matrix(num_time_points = num_measurements, pop_param_list = pop_params)
  
  #run simulations for each condition
  convergence_results <- lapply(X = 1:num_iterations, FUN = run_ind_simulation,
                                pop_params = pop_params,
                                cov_matrix = cov_matrix, 
                                measurement_spacing = measurement_spacing,
                                schedule = schedule, 
                                response_group_size = response_group_size, 
                                time_structuredness_values = time_structuredness_values) 

  #collapse results
  convergence_results <- rbindlist(convergence_results, use.names = T, fill = T)
  
  return(convergence_results)
}


run_ind_simulation <- function(num_iterations, pop_params, cov_matrix, measurement_spacing, schedule, 
                               response_group_size, time_structuredness_values){
  
  #setup variables; subtract 4 because the first 4 rows of cov_matrix contain values for logistic function parameters
  #(theta, alpha, beta, gamma)
  num_measurements <- ncol(cov_matrix) - 4
  
  param_table <- generate_ind_param_values(pop_param_list = pop_params, 
                                           response_group_size = response_group_size, 
                                           num_time_points = num_measurements, 
                                           cov_matrix = cov_matrix)
  
 
  
  #generate data to either be time structured or time unstructured 
  if (time_structuredness_values$response_rate == 'time_structured') {
    
    data <- generate_people_scores(num_measurements = num_measurements, 
                                   param_table = param_table,
                                   measurement_days = schedule$measurement_days, 
                                   time_period = 360)  
  } else { 
      delay_ls <- generate_time_delays(satiation_point = time_structuredness_values$satiation_point,
                                      satiation_value = time_structuredness_values$satiation_value, 
                                      response_window_length = 36, 
                                      num_respondents = response_group_size, 
                                      num_measurements = num_measurements)
    
      delay_dt <- generate_delay_table(delay_ls = delay_ls)
      
      data <- generate_people_scores(num_measurements = num_measurements, 
                                   param_table = param_table,
                                   measurement_days = schedule$measurement_days, 
                                   time_period = 360, 
                                   delay_values = delay_dt)
  }

  data_wide <- pivot_wider(data = data[ ,1:3], names_from = 'measurement_day', values_from = 'obs_score', 
                           names_prefix = sprintf('t%d_', 1:uniqueN(data$measurement_day)))
  
  names(data_wide)[-1] <- generate_manifest_var_names(data_wide)
  
  #start_values <- try(compute_starting_values(data = data), silent = T)
  
  #if (class(start_values) == 'try-error') {
    
  latent_growth_model <- create_logistic_growth_model_ns(data_wide = data_wide, model_name = 'auto_start')
  latent_growth_model <- mxAutoStart(model = latent_growth_model)
  #}
  
  #else { 
  #latent_growth_model <- create_logistic_growth_model(data_wide = data_wide, model_name = 'custom_start', starting_values = start_values)
  #}


  #run model with 11 different sets of starting values 
  model_results <- mxTryHard(latent_growth_model)
  
  #return simulation parameter values, random seed number, & convergence output 
  analysis_output <- data.table('number_measurements' = ncol(data_wide) - 1, 
                      'measurement_spacing' = measurement_spacing, 
                      'midpoint' = pop_params$beta_fixed, 
                      'sample_size' = response_group_size, 
                      'time_structuredness' = time_structuredness_values$response_rate, 
                      'status' = model_results$output$status$status,
                      'code' = model_results$output$status$code, 
                      'minus_2_likelihood' = as.numeric(model_results$output$Minus2LogLikelihood), 
                      'type_of_start' = model_results$name, 
                      
                      'theta_fixed' = as.numeric(model_results$output$estimate['theta_fixed']), 
                      'alpha_fixed' =  as.numeric(model_results$output$estimate['alpha_fixed']), 
                      'beta_fixed' = as.numeric(model_results$output$estimate['beta_fixed']), 
                      'gamma_fixed' = as.numeric(model_results$output$estimate['gamma_fixed']), 
                      
                      'theta_rand' = as.numeric(model_results$output$estimate['theta_rand']), 
                      'alpha_rand' =  as.numeric(model_results$output$estimate['alpha_rand']), 
                      'beta_rand' = as.numeric(model_results$output$estimate['beta_rand']), 
                      'gamma_rand' = as.numeric(model_results$output$estimate['gamma_rand']), 
                      'epsilon' = as.numeric(model_results$output$estimate['epsilon']))
  
  return (analysis_output)
}




