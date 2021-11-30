#' Computes measurement schedules. 
#'
#' @param time_period number of days over which change occurs
#' @param num_measurements number of measurements
#' @param measurement_spacing type of measurement spacing (one of 'equal', 'time_inc', 'time_dec', and 'mid_ext')
#' @param smallest_int_length length of smallest intervals 
#' @return Returns a list.
#' @export
compute_measurement_schedule <- function(time_period, num_measurements, smallest_int_length, measurement_spacing) {
  
  if (measurement_spacing == 'equal') {
    interval_length_list <- compute_equal_spacing_schedule(time_period, num_measurements)
  }
  
  else if(measurement_spacing == 'time_inc') {
    interval_length_list <- compute_time_increasing_schedule(time_period, num_measurements, smallest_int_length)
  }
  
  else if(measurement_spacing == 'time_dec') {
    interval_length_list <- compute_time_decreasing_schedule(time_period, num_measurements, smallest_int_length)
  }
  
  else if(measurement_spacing == 'mid_ext') {
    interval_length_list <- compute_middle_extreme_schedule(time_period, num_measurements, smallest_int_length)
  }
  
  else {
    return("The designated measurement spacing method is not valid.")
  }
  
  return(interval_length_list)
}

compute_equal_spacing_schedule <- function(time_period, num_measurements){
  
  if (num_measurements < 2) {
    stop("num_measurements must be greater than 2 to compute equal-spacing schedule")
  }
  
  #first determine the days on which measurements are taken
  num_intervals <- num_measurements - 1
  
  interval_lengths <- time_period/num_intervals
  
  interval_lengths <- rep(interval_lengths, times = num_intervals)
  
  return(list('interval_lengths' = interval_lengths,
              'measurement_days' = c(1, cumsum(interval_lengths))))
}

compute_time_increasing_schedule<- function(time_period, num_measurements, smallest_int_length) {
  
  if (num_measurements < 3) {
    stop("num_measurements must be greater than 3 to compute time-increasing schedule")
  }
  
  #compute length of constant by first calculating how many days remain after subtracting smallest_int_length for each interval.
  ##num_measurements-1 = number of intervals
  remaining_num_days <- time_period - (num_measurements-1)*smallest_int_length
  
  ##The number of constants = num_measurements - 1
  constant_length <- remaining_num_days/sum(seq(0,(num_measurements-2)))
  
  interval_lengths <- seq(0,(num_measurements-2))*constant_length + smallest_int_length
  
  return(list('interval_lengths' = interval_lengths,
              'measurement_days' = c(1, cumsum(interval_lengths))))
}

compute_time_decreasing_schedule<- function(time_period, num_measurements, smallest_int_length) {
  
  if (num_measurements < 3) {
    stop("num_measurements must be greater than 3 to compute time-decreasing schedule")
  }
  
  interval_length_list <- compute_time_increasing_schedule(time_period, num_measurements, smallest_int_length)
  
  return(list('interval_lengths' = rev(interval_length_list$interval_lengths),
              'measurement_days' = (time_period + 1) - rev(interval_length_list$measurement_days)))
}

compute_middle_extreme_schedule <- function(time_period, num_measurements, smallest_int_length){
  
  num_middle_and_extreme_measurements <- compute_num_middle_extreme_measurements(num_measurements)
  num_middle_intervals <- num_middle_and_extreme_measurements$num_middle_measurements - 1
  num_extreme_intervals <- num_middle_and_extreme_measurements$num_extreme_measurements*2 - 2
  
  secondary_spacing_interval <- (time_period - sum(num_middle_intervals,num_extreme_intervals)*smallest_int_length)/2
  
  interval_lengths <- c(rep(smallest_int_length, times = num_extreme_intervals/2),
                        secondary_spacing_interval,
                        rep(smallest_int_length, times = num_middle_intervals),
                        secondary_spacing_interval,
                        rep(smallest_int_length, times = num_extreme_intervals/2))
  
  return(list('interval_lengths' = interval_lengths,
              'measurement_days' = c(1, cumsum(interval_lengths))))
}

compute_num_middle_extreme_measurements <- function(num_measurements) {
  
  #check that num_measurements is greater than 3
  for (value in 1:length(num_measurements)) {
   if (num_measurements[value] <= 3) {
     stop("Each value in num_measurements must be greater than 3 to compute mid_ext schedule")
   }
  }
  
  ##set number of measurements in middle and extremities equal to each other if num_measurements is divisible by 3 (without remainder)
  #if (num_measurements%%3 == 0) { 
  #  num_middle_measurements <- num_measurements[num_measurements%%3 == 0]/3
  #  num_extreme_measurements <- num_middle_measurements
  #}
  
  
  #divide by three because num_measurements must be divided into three parts: two parts with even-integer length (extremes) 
  #and one part with odd-integer length (middle)
  num_extreme_measurements <- floor(num_measurements/3) 
  num_middle_measurements <- num_measurements - 2*num_extreme_measurements
    
  return(c(list('num_middle_measurements' = num_middle_measurements,
                'num_extreme_measurements' = num_extreme_measurements)))
  
}

