#' Generates list containing values for each population-level parameter.
#'
#' Four parameters (fixed effects) are used to characterize the logistic pattern and, due to the hierarchical nature of the to-be-generated data,
#' each parameter has a corresponding value of variability (i.e., random-effect). The random effects are used to generate the covariance
#' matrix. Note that correlations between random effects must also be set and that correlations between random effects and error variance
#' at each time point are set to 0 by default (cor_param_error = 0). Internally, the function also assumes zero-value correlations between
#' error variances at each time point  The four parameters that characterize the logistic pattern of change take on the
#' following meanings:
#' * theta: starting value (first plateau)
#' * alpha: ending value (second plateau)
#' * beta: amount of time to reach midway point (i.e., 50% of the distance between theta and alpha) from time = 0
#' * gamma: amount of time to reach satiation point (i.e., 73% of distance between theta and alpha) from midpoint
#' @md
#' @param num_time_points number of time points
#'
#' @param sd_theta standard deviation of  first plateau
#' @param sd_alpha standard deviation of second plateau
#' @param sd_beta standard deviation of beta
#' @param sd_gamma standard deviation of gamma
#' @param sd_error standard deviation of errors
#'
#' @param cor_theta_alpha theta_alpha  correlation
#' @param cor_beta_theta beta_theta correlation
#' @param cor_gamma_theta gamma_theta correlation
#' @param cor_beta_alpha beta_alpha correlation
#' @param cor_gamma_alpha gamma_alpha correlation
#' @param cor_gamma_beta gamma_beta-gamma correlation

#' @param cor_param_error parameter-error correlation
#'
#' @param scaling_constant constant that scales scores
#' @return Returns a covariance matrix.
#' @export
generate_four_param_pop_curve <- function(theta_fixed, alpha_fixed, beta_fixed, gamma_fixed,
                                           sd_theta, sd_alpha, sd_beta, sd_gamma, sd_error,
                                           cor_alpha_theta = 0,
                                           cor_beta_theta = 0, cor_beta_alpha = 0,
                                           cor_gamma_theta = 0, cor_gamma_alpha = 0, cor_gamma_beta = 0,
                                           cor_param_error = 0) {
  
  return(list(theta_fixed = theta_fixed, alpha_fixed = alpha_fixed, beta_fixed = beta_fixed, gamma_fixed = gamma_fixed,
              sd_theta = sd_theta, sd_alpha = sd_alpha, sd_beta = sd_beta, sd_gamma = sd_gamma, 
              sd_error = sd_error,
              cor_alpha_theta = cor_alpha_theta,
              cor_beta_theta = cor_beta_theta, cor_beta_alpha = cor_beta_alpha, 
              cor_gamma_theta = cor_gamma_theta, cor_gamma_alpha = cor_gamma_alpha,cor_gamma_beta = cor_gamma_beta,
              cor_param_error = cor_param_error))
}
