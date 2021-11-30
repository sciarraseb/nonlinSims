test_that("Measurement schedules are correctly generated", {
  
  largest_num_measurements <- 20
  num_measurements <- seq(from = 4, to = largest_num_measurements, by = 1)

#1) Check that equal spacing schedules have correct interval lengths --------

  for (measurement_number in num_measurements) {
    
    num_spacings <- measurement_number - 1
    expect_equal(compute_measurement_schedule(time_period = 360, num_measurements = measurement_number, smallest_int_length = 30, measurement_spacing = 'equal')$interval_lengths, 
                 rep(360/num_spacings, times = num_spacings))
  }
  

#2) Check that time_inc schedules have increasingly longer interval lengths (for measurement values of 5, 7, 9, and 11)  --------

  exp_num_measurements <- c(5, 7, 9, 11)
  
  for (measurement_number in exp_num_measurements){
    
    interval_values <- compute_measurement_schedule(time_period = 360, num_measurements = measurement_number, smallest_int_length = 30, measurement_spacing = 'time_inc')$interval_lengths
    expect_equal(object = order(interval_values), 1:(measurement_number-1))
  }
  

# 3) Check that time_dec schedules have correct interval lengths ----------
  
  for (measurement_number in exp_num_measurements){
    
    interval_values <- compute_measurement_schedule(time_period = 360, num_measurements = measurement_number, smallest_int_length = 30, measurement_spacing = 'time_dec')$interval_lengths
    expect_equal(object = order(interval_values, decreasing = T), 1:(measurement_number-1))
  }
  

# 4) Check that compute_num_middle_extreme_measurements computes ccorrect values for number of middle and extreme measurements --------

  expected_output <- list('num_middle_measurements' = c(2, 3, 2, 3, 4, 3, 4, 5, 4, 5, 6, 5, 6, 7, 6, 7, 8),
                          'num_extreme_measurements' = c(1, 1, rep(2:6, each = 3)))
  
  expect_equal(compute_num_middle_extreme_measurements(num_measurements = num_measurements),  expected_output)


# 5) Check that middle_ext interval lengths are correct for experimental values-------------------

    for (measurement_number in exp_num_measurements){
    
    num_middle_measurements <- compute_num_middle_extreme_measurements(num_measurements = measurement_number)$num_middle_measurements
    num_extreme_measurements <- compute_num_middle_extreme_measurements(num_measurements = measurement_number)$num_extreme_measurements
    middle_extreme_int_length <- (360 - (measurement_number-1 - 2)*30)/2
    
    expected_output <- c(rep(30, times = num_extreme_measurements - 1), 
      middle_extreme_int_length, 
      rep(30, times = num_middle_measurements - 1), 
      middle_extreme_int_length,
      rep(30, times = num_extreme_measurements - 1))
    
    
    expect_equal(compute_measurement_schedule(time_period = 360, num_measurements = measurement_number,
                                               smallest_int_length = 30, measurement_spacing = 'mid_ext')$interval_lengths, 
                 expected_output)
  }


# 6)Check that last measurement day always matches designated time period --------
  
  spacing_conditions <- c('equal', 'time_inc', 'time_dec', 'mid_ext')
  all_measurement_conditions <- expand.grid('num_measurements' = as.numeric(num_measurements), 
                                            'spacing_conditions' = as.character(spacing_conditions))
  
  ##iterate through each spacing-measurement number condition and check that last day is 360 
  for (condition in 1:nrow(all_measurement_conditions)) {
    num_measurements <- all_measurement_conditions$num_measurements[condition]
    
    expect_equal(compute_measurement_schedule(time_period = 360, smallest_int_length = 30, 
                                              measurement_spacing = all_measurement_conditions$spacing_conditions[condition], 
                                              num_measurements = num_measurements)$measurement_days[num_measurements], 360)
  }
  
  
}
)
