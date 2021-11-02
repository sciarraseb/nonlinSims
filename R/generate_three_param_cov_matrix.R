#' Generates covariance matrix for nonlinear longitudinal data that follow logistic pattern.
#'
#' Four parameters (fixed effects) are used to characterize the logistic pattern and, due to the hierarchical nature of the to-be-generated data,
#' each parameter has a corresponding value of variability (i.e., random-effect). The random effects are used to generate the covariance
#' matrix. Note that correlations between random effects must also be set and that correlations between random effects and error variance
#' at each time point are set to 0 by default (cor_param_error = 0). Internally, the function also assumes zero-value correlations between
#' error variances at each time point  The four parameters that characterize the logistic pattern of change take on the
#' following meanings:
#' * diff: different between first and last values (i.e., difference between two plateaus)
#' * beta: amount of time to reach midway point (i.e., 50% of the distance between theta and alpha) from time = 0
#' * gamma: amount of time to reach satiation point (i.e., 73% of distance between theta and alpha) from midpoint
#' @md
#' @param num_time_points number of time points
#' @param pop_param_list list of population parameters returned from generate_pop_param_list
#' @return Returns a covariance matrix.
#' @export
generate_three_param_cov_matrix <- function(num_time_points, pop_param_list) {
  
  #Create function that generates empty covariance matrix.
  empty_cov_matrix <- create_empty_cov_matrix(num_time_points)
  full_cov_matrix <- fill_empty_cov_matrix(empty_cov_matrix, pop_param_list, num_time_points)
  
  return(full_cov_matrix)
}

create_empty_cov_matrix <- function(num_time_points){
  
  #setup variables that describe structure of matrix
  var_names <- c('diff', 'beta', 'gamma')
  col_and_row_names <- c(var_names,  sprintf(fmt = 'error_%d', 0:(num_time_points - 1)))
  nrow_and_ncol <- length(var_names) + num_time_points
  
  #assemble empty covariance matrix that will be filled
  covariance_matrix <- matrix(nrow = nrow_and_ncol,
                              ncol = nrow_and_ncol,
                              dimnames = list(c(col_and_row_names), c(col_and_row_names)))
  
  return(covariance_matrix)
}

fill_empty_cov_matrix <- function(empty_cov_matrix, pop_param_list,
                                  num_time_points) {
  
  #setup for sd_values (used throughout the function)
  sd_diff <- pop_param_list$sd_diff
  sd_beta <- pop_param_list$sd_beta
  sd_gamma <- pop_param_list$sd_gamma
  
  #DIAGONAL
  #fill diagonal values of covariance matrix
  diag(empty_cov_matrix) <- c(sd_diff^2, sd_beta^2, sd_gamma^2,
                              rep(pop_param_list$sd_error^2, times = num_time_points))
  
  #LOWER HALF
  #fill off-diagonal cells for lower triangular half; off-diagonal section will always be the same regardless
  #of the number of time points
  empty_cov_matrix[2,1] <- pop_param_list$cor_diff_beta*sd_diff*sd_beta     #diff-beta
  empty_cov_matrix[3,1] <- pop_param_list$cor_diff_gamma*sd_diff*sd_gamma   #theta-beta
  empty_cov_matrix[3,2] <- pop_param_list$cor_beta_gamma*sd_beta*sd_gamma   #beta-alpha
  
  #fill in remaining values in lower triangular half with zero; note diag = T/F (does not matter; just have to match)
  lower_half_matrix <- empty_cov_matrix[lower.tri(empty_cov_matrix, diag = F)]
  lower_half_matrix[is.na(lower_half_matrix)] <- 0
  empty_cov_matrix[lower.tri(empty_cov_matrix, diag = F)] <- lower_half_matrix
  
  #UPPER HALF
  empty_cov_matrix <- Matrix::forceSymmetric(x = empty_cov_matrix, uplo = 'L')
  
  return(empty_cov_matrix)
}
