#' Scales times values according to the designated time period (new comment).
#'
#' @param time_period number of days over which change occurs
#' @param num_time_points number of time points
#' @return Returns a data table.
#' @export
compute_measurement_schedule <- function(time_period, num_measurements, base_time_length, measurement_spacing) {
  
  if (measurement_spacing == 'equal') {
    interval_length_list <- compute_equal_spacing_schedule(time_period, num_measurements, base_time_length)
  }
  
  else if(measurement_spacing == 'time_inc') {
    interval_length_list <- compute_time_increasing_schedule(time_period, num_measurements, base_time_length)
  }
  
  else if(measurement_spacing == 'time_dec') {
    interval_length_list <- compute_time_decreasing_schedule(time_period, num_measurements, base_time_length)
  }
  
  else if(measurement_spacing == 'mid_ext') {
    interval_length_list <- compute_middle_extreme_schedule(time_period, num_measurements, base_time_length)
  }
  
  else {
    return("The designated measurement spacing method is not valid.")
  }
  
  return(interval_length_list)
}

compute_equal_spacing_schedule <- function(time_period, num_measurements, base_time_length){
  
  #first determine the days on which measurements are taken
  num_intervals <- num_measurements - 1
  
  interval_length <- time_period/num_intervals
  
  interval_lengths <- rep(interval_length, times = num_intervals)
  
  return(list('interval_length' = interval_lengths,
              'measurement_days' = c(1, cumsum(interval_lengths))))
}

compute_time_increasing_schedule<- function(time_period, num_measurements, base_time_length) {
  
  #compute length of constant by first calculating how many days remain after subtracting base_time_length for each interval.
  ##num_measurements-1 = number of intervals
  remaining_num_days <- time_period - (num_measurements-1)*base_time_length
  ##The number of constants = num_measurements - 1
  constant_length <- remaining_num_days/sum(seq(0,(num_measurements-2)))
  
  interval_lengths <- seq(0,(num_measurements-2))*constant_length + base_time_length
  
  return(list('interval_length' = interval_lengths,
              'measurement_days' = c(1, cumsum(interval_lengths))))
}

compute_time_decreasing_schedule<- function(time_period, num_measurements, base_time_length) {
  
  interval_length_list <- compute_time_increasing_schedule(time_period, num_measurements, base_time_length)
  
  return(list('interval_length' = rev(interval_length_list$interval_length),
              'measurement_days' = (time_period + 1) - rev(interval_length_list$measurement_days)))
}

compute_middle_extreme_schedule <- function(time_period, num_measurements, base_time_length){
  
  num_middle_and_extreme_measurements <- compute_num_middle_extreme_measurements(num_measurements)
  num_middle_intervals <- num_middle_and_extreme_measurements$num_middle_measurements - 1
  num_extreme_intervals <- num_middle_and_extreme_measurements$num_extreme_measurements*2 - 2
  
  secondary_spacing_interval <- (time_period - sum(num_middle_intervals,num_extreme_intervals)*base_time_length)/2
  
  interval_lengths <- c(rep(base_time_length, times = num_extreme_intervals/2),
                        secondary_spacing_interval,
                        rep(base_time_length, times = num_middle_intervals),
                        secondary_spacing_interval,
                        rep(base_time_length, times = num_extreme_intervals/2))
  
  return(list('interval_length' = interval_lengths,
              'measurement_days' = c(1, cumsum(interval_lengths))))
}

compute_num_middle_extreme_measurements <- function(num_measurements) {
  #remaining measurements is a temporary variable used to determine if an even number of measurements are left for the extremities (i.e., beginining and end)
  remaining_measurements <- num_measurements - ceiling(num_measurements/2)
  
  num_middle_measurements <- ifelse(remaining_measurements%%2 == 0, ceiling(num_measurements/2), floor(num_measurements/2))
  num_extreme_measurements <- (num_measurements - num_middle_measurements)/2
  
  
  return(c(list('num_middle_measurements' = num_middle_measurements,
                'num_extreme_measurements' = num_extreme_measurements)))
  
}

