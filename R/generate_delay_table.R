#' Generates time structuredness manipulation. 
#'
#' @param num_iterations number of iterations 
#' @param pop_params list of population parameters 
#' @param schedule measurement schedule
#' @param response_group_size sample size  
#' @return Returns a data table.
#' @export
generate_delay_table <- function(delay_ls){
  
  num_measurements <- length(delay_ls)
  num_respondents <- length(delay_ls[[1]])
  
  #create a data.table and randomly assign delay values to participants
  delay_table <- data.table('ID' = rep(1:num_respondents, times = num_measurements), 
                            'delay_value' = unlist(lapply(delay_ls, sample))) 
  
  #group by table 
  delay_table <- delay_table[order(ID)]
  
  return(delay_table)
}


generate_time_delays <- function(satiation_point, satiation_value, response_window_length,
                                 num_respondents, num_measurements) {
  
  ##response function = M*(1 - exp(-ax))
  #compute value of 'a' that generates appropriate response curve
  a <- log(1 - satiation_value)/-satiation_point
  
  #Find the PDF of the CDF M(1 - exp(-ax)). This is done by computing the first derivative. 
  cdf_exp <- expression(M*(1 - exp(-a*day)))
  pdf_exp <- D(expr = cdf_exp, 'day')
  
  #compute probability density curve using pdf function 
  day <- seq(from = 0, to = 36, by = .0001)
  M <- 1
  probability_values <- eval(pdf_exp)
  
  #sample delay values using probabilities from pdf. 
  sampled_delays_ls <- rerun(.n = num_measurements, 
                             sample(x = day, size = num_respondents, replace = T, prob = probability_values))
  
  return(sampled_delays_ls)
}



