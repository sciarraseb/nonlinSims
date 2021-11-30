test_that("generate_three_param_cov_matrix correctly generates variance_covariance matrix", {
  
  sd_scale <- 1.5
  common_effect_size <- 0.32
  diff_fixed <- sd_scale * common_effect_size
  
  num_measurements <- 5
  time_period <- 360
  scaling_factor <- time_period/num_measurements
  
  beta_fixed <- 60
  gamma_fixed <- 20
  
  #random effects 
  sd_diff <- 0.15
  sd_beta <- 10
  sd_gamma <- 5
  sd_error <- 0.05
  
  #correlations
  cor_diff_beta <- 0
  cor_diff_gamma <- 0
  cor_beta_gamma <- 0
  scaling_constant <- 3
  
  params <- generate_three_param_pop_curve(diff_fixed = diff_fixed, beta_fixed = beta_fixed[1], gamma_fixed = gamma_fixed, 
                                           sd_diff = sd_diff, sd_beta = sd_beta, sd_gamma = sd_gamma, sd_error = sd_error, 
                                           cor_diff_beta = cor_diff_beta, cor_diff_gamma = cor_diff_gamma, cor_beta_gamma = cor_beta_gamma,
                                           scaling_constant = scaling_constant)
  
  cov_matrix <- generate_three_param_cov_matrix(num_time_points = num_measurements, pop_param_list = pop_params)
  
  #expectations 
  ##test diagonal values
  expect_equal(cov_matrix['diff', 'diff'], sd_diff^2)
  expect_equal(cov_matrix['beta', 'beta'], sd_beta^2)
  expect_equal(cov_matrix['gamma', 'gamma'], sd_gamma^2)
  expect_equal(cov_matrix['error_0', 'error_0'], sd_error^2) 
  
  ##test all non-diagonal values are zero
  expect_equal(cov_matrix[upper.tri(cov_matrix)], rep(0, length(cov_matrix[upper.tri(cov_matrix)])))
  
  ##secondary test of non-diagonal values when correlations are nonzero 
  cor_diff_beta_nz <- 0.3
  params_nz <- generate_three_param_pop_curve(diff_fixed = diff_fixed, beta_fixed = beta_fixed[1], gamma_fixed = gamma_fixed, 
                                           sd_diff = sd_diff, sd_beta = sd_beta, sd_gamma = sd_gamma, sd_error = sd_error, 
                                           cor_diff_beta = cor_diff_beta_nz, cor_diff_gamma = cor_diff_gamma, cor_beta_gamma = cor_beta_gamma,
                                           scaling_constant = scaling_constant)
  
  cov_matrix_nz <- generate_three_param_cov_matrix(num_time_points = num_measurements, pop_param_list = params_nz)
  
  expect_equal(cov_matrix_nz['diff', 'beta'], cor_diff_beta_nz*sd_beta*sd_diff)
  
})
