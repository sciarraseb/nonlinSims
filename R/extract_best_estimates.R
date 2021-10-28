#' Extracts parameter estimates from best-fitting model    
#'
#' Description of function.
#' @md
#' @param data number of measurements
#' @param param_table table containing parameter values for each person
#' @param measurement_days days of measurement
#' @param time_period length of time over which measurements are taken
#' @return Returns a data table.
#' @export
extract_best_estimates <- function(fit_results, starting_values) {
  
  best_starting_value_set <- as.numeric(str_extract(fit_results[1, ]$model_num, pattern = "(\\d)+"))
  best_fitting_model <- create_logistic_growth_model(data_wide = data_wide, 
                                                     model_name = 'lg_nonlinear', starting_values = starting_values[best_starting_value_set, ])
  
  best_fitting_output <- summary(mxRun(best_fitting_model))$parameter
  
  parameter_estimates <- data.table('parameter' = best_fitting_output$name, 
                                    'estimate' = best_fitting_output$Estimate)
  
  return(parameter_estimates)
}