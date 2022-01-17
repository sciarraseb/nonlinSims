#' Generates scores at each time point for each person.
#'
#' Scores are generates according to three-parameter logistic function. 
#' @md
#' @param num_measurements number of measurements
#' @param param_table table containing parameter values for each person
#' @param measurement_days days of measurement
#' @param time_period length of time over which measurements are taken
#' @return Returns a data table.
#' @import data.table
#' @export
generate_people_scores <- function(num_measurements, param_table, measurement_days, time_period, delay_values = NA) {
  
  
  #setup variables for creating empty datatable
  num_people <- nrow(param_table)
  num_scores <- num_people*num_measurements
  
  #pre-specify table size
  empty_data <- data.table('ID' = as.factor(rep(1:num_people, each = num_measurements)),  
                           'measurement_day' = rep(measurement_days, times = num_people),
                           'obs_score' = numeric(length = num_scores),
                           'true_score' = numeric(length = num_scores))
  
  #compute time-unstructured delay values if delay_values argument is non-empty
  if (length(delay_values) > 1) {
    empty_data$actual_measurement_day <- empty_data$measurement_day + delay_values$delay_value
  }
  
  #pre-specify lists that will temporarily contain observed and population scores
  obs_scores_ls <- vector(mode = 'list', length = num_people)
  true_scores_ls <- vector(mode = 'list', length = num_people)
  
  #generates time_unstructured data 
  if('actual_measurement_day' %in% names(empty_data)) {
    
    for (person in 1:nrow(param_table)) {
      obs_scores_ls[[person]] <- generate_ind_scores(ind_param_values = param_table[person, ],
                                                        num_measurements = num_measurements, 
                                                        measurement_days = empty_data$actual_measurement_day[empty_data$ID == person])
      
      true_scores_ls[[person]] <- generate_pop_scores(ind_param_values = param_table[person, ],
                                                         num_measurements = num_measurements, 
                                                         measurement_days = empty_data$actual_measurement_day[empty_data$ID == person])
    }
  }
  

  #generates time-structured data 
  else {
    
    for (person in 1:nrow(param_table)) {
      obs_scores_ls[[person]] <- generate_ind_scores(ind_param_values = param_table[person, ],
                                                        num_measurements = num_measurements, 
                                                        measurement_days = measurement_days)
    
      true_scores_ls[[person]] <- generate_pop_scores(ind_param_values = param_table[person, ],
                                                         num_measurements = num_measurements, 
                                                         measurement_days = measurement_days)
    }
  }
  
  #unlist obs_scores_ls and true_scores_ls and append to data table
  empty_data$obs_score <- unlist(obs_scores_ls)
  empty_data$true_score <- unlist(true_scores_ls)
  
  return(empty_data)
}

generate_ind_scores <- function(ind_param_values, num_measurements, measurement_days){
  
  #extract error values for a person
  ind_error_values <- extract_ind_error_values(ind_param_values = ind_param_values,
                                               num_measurements = num_measurements)
  
  #extract parameter values for a person
  theta_j <- ind_param_values[['theta_fixed']] + ind_param_values[['theta_random']]
  alpha_j <- ind_param_values[['alpha_fixed']] + ind_param_values[['alpha_random']]
  beta_j <- ind_param_values[['beta_fixed']] + ind_param_values[['beta_random']]
  gamma_j <- ind_param_values[['gamma_fixed']] + ind_param_values[['gamma_random']]
  
  #compute scores at each time point for one person
  ##if time structuredness is manipulated, then the value for measurement_days must be generated accordingly. 
  ind_scores <-  theta_j + (alpha_j - theta_j)/(1 + exp((beta_j - measurement_days)/gamma_j)) + ind_error_values
  
  return(ind_scores)
}

generate_pop_scores <- function(ind_param_values, num_measurements, measurement_days) {
  
  #extract parameter values for a person
  theta_j <- ind_param_values[['theta_fixed']] + ind_param_values[['theta_random']]
  alpha_j <- ind_param_values[['alpha_fixed']] + ind_param_values[['alpha_random']]
  
  beta_j <- ind_param_values[['beta_fixed']] + ind_param_values[['beta_random']]
  gamma_j <- ind_param_values[['gamma_fixed']] + ind_param_values[['gamma_random']]
  
  #compute scores at each time point for one person
  pop_scores <-  theta_j + (alpha_j - theta_j)/(1 + exp((beta_j - measurement_days)/gamma_j))
  
  return(pop_scores)
}

extract_ind_error_values <- function(ind_param_values, num_measurements){
  
  #extract name of error columns and use them to return vector of error values
  error_cols <- tail(names(ind_param_values), num_measurements)
  ind_error_values <- as.vector(t(ind_param_values[ , ..error_cols]))
  
  return(ind_error_values)
}

