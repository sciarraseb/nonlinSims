#' Runs Monte Carlo simulations.
#'
#' @param num_iterations number of iterations 
#' @param pop_params list of population parameters 
#' @param schedule measurement schedule
#' @param response_group_size sample size  
#' @return Returns a data table.
#' @export
run_exp_simulation_4l <- function(factor_list, num_iterations, pop_params, response_group_size, num_cores, seed) {
  
  set.seed(seed)
  
  
  if (.Platform$OS.type == 'windows') {
    
    #needed to ensure reproducibility 
    RNGkind("L'Ecuyer-CMRG")
    
    
    #setup variables 
    exp_conditions <- data.table(expand.grid(factor_list))
    exp_conditions$spacing <- as.character(exp_conditions$spacing)
    total_results <- data.table()
    
    
    #open clusters
    simulation_cluster <- makeCluster(spec = num_cores)
    
    #load relevant variables into cluster
    clusterExport(cl = simulation_cluster, varlist = list("exp_conditions", "total_results"))
    
    #load packages into each node cluster 
    clusterEvalQ(cl = simulation_cluster, expr =  {
      library(nonlinSims)
      library(parallel)
      library(tidyverse)
      library(OpenMx)
      library(data.table)
    })
    
    
    #run simulation in cluster
    for (condition in 1:nrow(exp_conditions)) {
      
      iteration_results <- as.data.table(parLapply(cl = simulation_cluster, X = num_iterations, 
                                                   fun = run_condition_simulation_4l, 
                                                   pop_params = pop_params, 
                                                   response_group_size = response_group_size, 
                                                   num_measurements = exp_conditions$num_measurements[condition], 
                                                   measurement_spacing = exp_conditions$spacing[condition], 
                                                   midpoint_value = exp_conditions$midpoint[condition]))
      
      total_results <- rbind(total_results, iteration_results)
    }
    
    #export data set 
    clusterExport(cl = simulation_cluster, varlist = list('total_results'))
    
    #close cluster
    stopCluster(cl = simulation_cluster)
    
  }
  
  if(.Platform$OS.type == 'unix'){
    
    RNGkind("L'Ecuyer-CMRG")
    
    #compute experiment conditions
    exp_conditions <- data.table(expand.grid(factor_list))
    exp_conditions$spacing <- as.character(exp_conditions$spacing)
    total_results <- data.table()
    
    #test convergence in each condition
    for (condition in 1:nrow(exp_conditions)) {
      
      iteration_results <- as.data.table(mclapply(X = num_iterations, 
                                                  FUN = run_condition_simulation_4l, 
                                                  pop_params = pop_params, 
                                                  response_group_size = response_group_size, 
                                                  
                                                  num_measurements = exp_conditions$num_measurements[condition], 
                                                  measurement_spacing = exp_conditions$spacing[condition], 
                                                  midpoint_value = exp_conditions$midpoint[condition],
                                                  mc.cores = num_cores, mc.set.seed = T))
      
      total_results <- rbind(total_results, iteration_results)
      
    }
  }
  return(total_results)
}

run_condition_simulation_4l <- function(num_iterations, pop_params, response_group_size, 
                                     num_measurements, measurement_spacing, midpoint_value) {
  
  #setup of population parameters
  pop_params$beta_fixed <- midpoint_value #update beta value
  
  schedule <- compute_measurement_schedule(time_period = 360, 
                                           num_measurements =  num_measurements, 
                                           smallest_int_length = 30, 
                                           measurement_spacing = measurement_spacing)
  
  cov_matrix <- generate_four_param_cov_matrix(num_time_points = num_measurements, pop_param_list = pop_params)
  
  #test convergence in each condition
  convergence_results <- lapply(X = 1:num_iterations, FUN = run_ind_simulation_4l,
                                pop_params = pop_params,
                                cov_matrix = cov_matrix, 
                                measurement_spacing = measurement_spacing,
                                schedule = schedule, 
                                response_group_size = response_group_size)
  
  #extract column names first
  col_names <- names(convergence_results[[1]])
  
  convergence_results <- data.table(matrix(unlist(convergence_results, use.names = T), 
                                           ncol = length(convergence_results[[1]]), byrow = T))
  
  #assign column names
  names(convergence_results) <- col_names
  
  
  return(convergence_results)
}

run_ind_simulation_4l <- function(num_iterations, pop_params, cov_matrix, measurement_spacing, schedule, response_group_size){
  
  #setup variables
  num_measurements <- ncol(cov_matrix) - 4
  
  param_table <- generate_4l_ind_param_values(pop_param_list = pop_params, 
                                           response_group_size = response_group_size, 
                                           num_time_points = num_measurements, 
                                           cov_matrix = cov_matrix)
  
  data <- generate_group_scores_4l(num_measurements = num_measurements, 
                                param_table = param_table,measurement_days = schedule$measurement_days, 
                                time_period = 360)
  
  data_wide <- pivot_wider(data = data[ ,1:3], names_from = 'measurement_day', values_from = 'obs_score', 
                           names_prefix = sprintf('t%d_', 1:uniqueN(data$measurement_day)))
  
  names(data_wide)[-1] <- generate_manifest_var_names(data_wide)
  
  
  #create placeholder model  
  latent_growth_model <- create_logistic_growth_model_4l_ns(data_wide = data_wide, model_name = 'lg_nonlinear')
  
  #find good starting values 
  latent_growth_model <- mxAutoStart(model = latent_growth_model)
  
  #run model with 10 different sets of starting values 
  model_results <- mxTryHard(latent_growth_model)
  
  #return simulation parameter values, random seed number, & convergence output 
  analysis_output <- c('number_measurements' = ncol(data_wide) - 1, 
                       'measurement_spacing' = measurement_spacing, 
                       'midpoint' = pop_params$beta_fixed, 
                       'status' = model_results$output$status$status,
                       'code' = model_results$output$status$code, 
                       'minus_2_likelihood' = model_results$output$Minus2LogLikelihood)
  
  
  return(analysis_output)
}




