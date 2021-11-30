test_that("Evaluate model creation procedure", {
  
  sd_scale <- 1.5
  common_effect_size <- 0.32
  diff_fixed <- sd_scale * common_effect_size
  
  num_measurements <- 9
  time_period <- 360
  scaling_factor <- time_period/num_measurements
  
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
  
  
  #INDEPENDENT VARIABLE SETUP (note that sample size is fixed at 225)
  ##Set beta values at 80, 180, and 280 
  beta_fixed <- seq(from = 80, to = 280, by = 100)[1]
  
  #Set number of measurements to either 5, 7, 9, or 11
  num_measurements <- seq(from = 5, to = 11, by = 2)[4]
  
  schedule <- compute_measurement_schedule(time_period = 360, num_measurements = num_measurements, smallest_int_length = 30, measurement_spacing = 'equal')
  
  
  #Set up of population-level parameters
  pop_params <- generate_three_param_pop_curve(diff_fixed = diff_fixed, beta_fixed = beta_fixed, gamma_fixed = gamma_fixed, 
                                               sd_diff = sd_diff, sd_beta = sd_beta, sd_gamma = sd_gamma, sd_error = sd_error, 
                                               cor_diff_beta = cor_diff_beta, cor_diff_gamma = cor_diff_gamma, cor_beta_gamma = cor_beta_gamma,
                                               scaling_constant = scaling_constant)
  
  cov_matrix <- generate_three_param_cov_matrix(num_time_points = num_measurements, pop_param_list = pop_params)
  
  param_table <- generate_ind_param_values(pop_param_list = pop_params, 
                                           response_group_size = 100, 
                                           num_time_points = num_measurements, 
                                           cov_matrix = cov_matrix)
  
  data <- generate_group_scores(num_measurements = num_measurements, 
                                param_table = param_table,measurement_days = schedule$measurement_days, 
                                time_period = 360, scaling_constant = 3)
  
  data_wide <- pivot_wider(data = data[ ,1:3], names_from = 'measurement_day', values_from = 'obs_score', 
                           names_prefix = sprintf('t%d_', 1:uniqueN(data$measurement_day)))
  
  names(data_wide)[-1] <- nonlinSims:::generate_manifest_var_names(data_wide)
  
  model <- create_logistic_growth_model(data_wide = data_wide, model_name = 'test')
  
  #check that mean values used match values returned by compute_manifest_means]
  expect_equal(model$M$values[1:num_measurements], nonlinSims:::compute_manifest_means(data_wide = data_wide))
  
  #check that partial derivatives match 
  log_equation <- expression(d/(1 + exp((b - time)/g)))

  expect_identical(object = model$Dl$formula, D(log_equation, 'd'))
  expect_identical(object = model$Bl$formula, D(log_equation, 'b'))
  expect_identical(object = model$Gl$formula, D(log_equation, 'g'))

  }
)



