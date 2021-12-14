#' Generates parameter values for each person in a response group.
#'
#' The four parameters that characterize the logistic pattern of change take on the
#' following meanings:
#' * diff: different between first and last values (i.e., difference between two plateaus)
#' * beta: amount of time to reach midway point (i.e., 50% of the distance between theta and alpha) from time = 0
#' * gamma: amount of time to reach satiation point (i.e., 73% of distance between theta and alpha) from midpoint
#' @md
#' @param diff_fixed fixed-effect value for diff
#' @param beta_fixed fixed-effect value for beta
#' @param gamma_fixed fixed-effect value for gamma
#'
#' @param response_group_size response group size
#' @param num_time_points number of time points
#' @param cov_matrix covariance matrix generated from generate_cov_matrix

#'
#' @return Returns a data table.
#' @export
generate_ind_param_values <- function(pop_param_list,
                                      response_group_size, num_time_points,
                                      cov_matrix) {
  
  #generate random effects (i.e., random deviations) for diff, beta, and gamma for each person
  ##note: random-effects are assumed to have mean = 0
  ## note: empirical=F so values do not define sample-level parameters
  random_effects <-  MASS::mvrnorm(n = response_group_size,
                                   mu=rep(0, times = nrow(cov_matrix)),
                                   Sigma = cov_matrix, empirical = FALSE) %>% as.data.table()
  
  var_names <- c('diff_random', 'beta_random', 'gamma_random')
  colnames(random_effects) = c(var_names, sprintf(fmt = 'error_%d', 0:(num_time_points - 1)))
  
  #create data table with all parameter values for each participant.
  param_table <- cbind(ID = as.factor(c(1:response_group_size)),
                       diff_fixed = pop_param_list$diff_fixed,
                       beta_fixed = pop_param_list$beta_fixed,
                       gamma_fixed = pop_param_list$gamma_fixed,
                       random_effects)
  
  return(param_table)
}



