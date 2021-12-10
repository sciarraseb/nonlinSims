#' Computes starting values for four-parameter logistic curve model. 
#'
#' Data are modelled with a structured latent growth curve model.
#' @md
#' @param data_wide wide version of data 
#' @param model_name name of model 
#' @export
compute_starting_values_4l <- function(data){
  
  fixed_starts <- getInitial(obs_score ~ SSfpl(input = measurement_day, A = theta, B = alpha, xmid = beta, scal = gamma), data = data)
  
  ind_est_values <- nlsList(obs_score ~ SSfpl(input = measurement_day, A = theta, B = alpha, xmid = beta, scal = gamma)| ID, data = data, 
                                start = fixed_starts, warn.nls = F)
  
  rand_starts <- ranef(ind_est_values)
  
  #theta_rand starting values  
  theta_rand <- compute_rand_start_values(rand_starts = rand_starts, parameter = 'theta')
  alpha_rand <- compute_rand_start_values(rand_starts = rand_starts, parameter = 'alpha')
  beta_rand <- compute_rand_start_values(rand_starts = rand_starts, parameter = 'beta')
  gamma_rand <- compute_rand_start_values(rand_starts = rand_starts, parameter = 'gamma')
  
  start_values <- list('theta_fixed' = fixed_starts['theta'], 
                       'alpha_fixed' = fixed_starts['alpha'], 
                       'beta_fixed' = fixed_starts['beta'], 
                       'gamma_fixed' = fixed_starts['gamma'],
                       
                       'theta_rand' = theta_rand, 
                       'alpha_rand' = alpha_rand, 
                       'beta_rand' = beta_rand,  
                       'gamma_rand' = gamma_rand, 
                       'epsilon' = runif(n = 1, min = 1, max = 5))
  
  return(start_values)
}

compute_rand_start_values <- function(rand_starts, quantile_range = c(.025, .975), parameter) {
  
  col_num <- which(colnames(rand_starts) == parameter)
  
  ordered_param <- rand_starts[order(rand_starts[ ,col_num]), col_num]
  
  middle_percentiles_param <- quantile(ordered_param, probs = quantile_range, na.rm = T)
  target_quantile_range <- na.exclude(ordered_param[ordered_param >= middle_percentiles_param[1] & ordered_param <= middle_percentiles_param[2]])
  
  return(var(target_quantile_range, na.rm = T))
  
}