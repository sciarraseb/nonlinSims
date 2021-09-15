#' Generates list containing values for each population-level parameter.
#'
#' Four parameters (fixed effects) are used to characterize the logistic pattern and, due to the hierarchical nature of the to-be-generated data,
#' each parameter has a corresponding value of variability (i.e., random-effect). The random effects are used to generate the covariance
#' matrix. Note that correlations between random effects must also be set and that correlations between random effects and error variance
#' at each time point are set to 0 by default (cor_param_error = 0). Internally, the function also assumes zero-value correlations between
#' error variances at each time point  The four parameters that characterize the logistic pattern of change take on the
#' following meanings:
#' * diff: difference between two plateaus of curve
#' * beta: amount of time to reach midway point (i.e., 50% of the distance between theta and alpha) from time = 0
#' * gamma: amount of time to reach satiation point (i.e., 73% of distance between theta and alpha) from midpoint
#' @md
#' @param num_time_points number of time points
#'
#' @param sd_diff standard deviation of difference between the first and second plateaus
#' @param sd_beta standard deviation of beta
#' @param sd_gamma standard deviation of gamma
#' @param sd_error standard deviation of errors
#'
#' @param cor_diff_beta diff-beta correlation
#' @param beta-gamma correlation
#' @param cor_beta_gamma beta-gamma correlation
#' @param cor_param_error parameter-error correlation
#'
#' @param scaling_constant constant that scales scores
#' @return Returns a covariance matrix.
#' @export
generate_three_param_pop_curve <- function(diff_fixed, beta_fixed, gamma_fixed,
                                           sd_diff, sd_beta, sd_gamma, sd_error,
                                           cor_diff_beta,
                                           cor_diff_gamma, cor_beta_gamma,
                                           cor_param_error = 0,
                                           scaling_constant
) {
  
  return(list(diff_fixed = diff_fixed, beta_fixed = beta_fixed, gamma_fixed = gamma_fixed,
              sd_diff = sd_diff, sd_beta = sd_beta, sd_gamma = sd_gamma, sd_error = sd_error,
              cor_diff_beta = cor_diff_beta,
              cor_diff_gamma = cor_diff_gamma, cor_beta_gamma = cor_beta_gamma,
              cor_param_error = cor_param_error,
              scaling_constant = scaling_constant))
}
