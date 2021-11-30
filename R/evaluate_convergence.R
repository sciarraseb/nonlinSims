#' Tests effectiveness of starting value procedure. 
#'
#' @param factor_list 
#' @param num_iterations number of time points
#' @param pop_params 
#' @param xresponse_group_size
#' @return  Returns a data table
#' @export
run_exp_simulation <- function(factor_list, num_iterations, pop_params, response_group_size, num_cores) {
  
  
  #compute experiment conditions
  exp_conditions <- data.table(expand.grid(factor_list))
  exp_conditions$spacing <- as.character(exp_conditions$spacing)
  
  total_results <- data.table()
  
  if (.Platform$OS.type == 'windows') {
    
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
                                                   fun = evaluate_condition, 
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
    #test convergence in each condition
    for (condition in 1:nrow(exp_conditions)) {
      
      iteration_results <- as.data.table(mclapply(X = num_iterations, 
                                                  FUN = evaluate_condition, 
                                                  pop_params = pop_params, 
                                                  response_group_size = response_group_size, 
                                                  
                                                  num_measurements = exp_conditions$num_measurements[condition], 
                                                  measurement_spacing = exp_conditions$spacing[condition], 
                                                  midpoint_value = exp_conditions$midpoint[condition],
                                                  mc.cores = num_cores))
      
      total_results <- rbind(total_results, iteration_results)
      
    }
  }
  return(total_results)
}

evaluate_condition <- function(num_iterations, pop_params, response_group_size, 
                           num_measurements, measurement_spacing, midpoint_value) {
  
  num_measurements <- num_measurements
  
  
  #setup of population parameters
  pop_params$beta_fixed <- midpoint_value
  
  schedule <- compute_measurement_schedule(time_period = 360, 
                                           num_measurements =  num_measurements, 
                                           smallest_int_length = 30, 
                                           measurement_spacing = measurement_spacing)
  
  cov_matrix <- generate_three_param_cov_matrix(num_time_points = num_measurements, pop_param_list = pop_params)
  
  #test convergence in each condition
  convergence_results <- as.data.table(lapply(X = num_iterations, FUN = run_simulations,
                                pop_params = pop_params,
                                cov_matrix = cov_matrix, 
                                measurement_spacing = measurement_spacing,
                                schedule = schedule, 
                                response_group_size = response_group_size))
  
  #perc_converge <-  (sum(convergence_results$code == 0)/num_iterations) * 100
  
  return(convergence_results)
}
