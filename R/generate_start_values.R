#' Generates starting values for estimating nonlinear parameters. 
#'
#' Starting value procedure largely mirrors that of MPLUS. 
#' @md
#' @param num_sets number of starting value sets to generate 
#' @param time_fixed_effects names of fixed-effects parameters relating to time (i.e., beta and gamma in case of three-parameter logistic curve)
#' @param time_rand_effects names of random-effects parameters relating to time (i.e., beta and gamma in case of three-parameter logistic curve)
#' @param diff_effects names of parameters that define the difference between starting and ending values of the logistic curve
#' @param error name of error parameter
#' @return Returns a data table of starting values 
#' @export
generate_multiple_starting_value_sets <- function(num_sets, data, data_wide){
  
  time_rand_effects <- c('beta_rand', 'gamma_rand')
  
  starting_value_set <- list()
  
  for (set in 1:num_sets){
    starting_value_set[[set]] <- generate_starting_values(data, time_rand_effects, data_wide)
  }
  
  starting_value_set_mat <- data.table(matrix(data = unlist(starting_value_set), 
                                   ncol=length(unlist(starting_value_set[1])), 
                                   byrow = T, 
                                   dimnames = list(1:length(starting_value_set), 
                                                   names(starting_value_set[[1]]))))
  return(starting_value_set_mat)
  
}

generate_starting_values <- function(data, time_rand_effects, data_wide){
  
  rand_effect_starts <- compute_rand_effect_starts(data = data, time_rand_effects = time_rand_effects) 
  
  fixed_effect_starts <- compute_fixed_effect_starts(data = data, rand_effect_starts = rand_effect_starts)

  residual_starts <- compute_residual_starts(data_wide = data_wide)
  
  
  starting_values <- c(fixed_effect_starts, rand_effect_starts, residual_starts)
  
  return(unlist(starting_values))
}


compute_rand_effect_starts <- function(data, time_rand_effects){
  diff_rand <- compute_diff_rand_start(data = data)
  beta_gamma_rand <- generate_beta_gamma_rand_starts(time_rand_effects = c('beta_rand', 'gamma_rand'))
  
  return(c(diff_rand, beta_gamma_rand))
}

generate_beta_gamma_rand_starts <- function(time_rand_effects) {
  
  starting_values_list <- list()
  
  #default value in MPlus (for growth models, "individual regressions of the outcome variable on time"); I can't obtain an empirical estimation so I will 
  #leave it at 10 for beta and 5 for gamma 
  #base scale variable is zero so there is no perturbation of random effects; see Asparouhov & Muthen (2019)
  
  for (effect in time_rand_effects) {
    
    if(effect == 'beta_rand'){
      starting_values_list[[effect]] <-  10
    }
    
    #for gamma_rand
    else {
      starting_values_list[[effect]] <- 5
    }
  }
  
  return(starting_values_list)
}

compute_diff_rand_start <- function(data){
  
  diff_rand <- var(data$obs_score[data$measurement_day == 360] - data$obs_score[data$measurement_day == 1])
  return(list('diff_rand' = diff_rand))
}


compute_fixed_effect_starts <- function(data, rand_effect_starts){
  
  #use self-starting function to compute estimates 
  xy <- sortedXyData(x = data$measurement_day, y = data$obs_score, data = data)
  z <- xy[["y"]]
  rng <- c(z[1], z[length(z)]) #take difference between first and last values 
  dz <- diff(rng)
  z <- (z - rng[1L] + 0.05 * dz)/(1.1 * dz)
  xy[["z"]] <- log(z/(1 - z))
  aux <- coef(lm(x ~ z, xy))
  
  #apply w + srb framework from MPlus routine
  scale_var <- 5
  
  diff_fixed_start <- dz + scale_var * runif(n = 1, min = -0.5, max = 0.5) *  (2*max(c(1, sqrt(rand_effect_starts$diff_rand))))
  beta_fixed_start <- as.numeric(aux[1]) + scale_var * runif(n = 1, min = -0.5, max = 0.5) *  (2*max(c(1, sqrt(rand_effect_starts$beta_rand))))
  gamma_fixed_start <- as.numeric(aux[2]) + scale_var * runif(n = 1, min = -0.5, max = 0.5) *  (2*max(c(1, sqrt(rand_effect_starts$gamma_rand))))
  
  
  return(list('diff_fixed' = diff_fixed_start, 
              'beta_fixed' = beta_fixed_start, 
              'gamma_fixed' = gamma_fixed_start))
  
  
}


compute_residual_starts <- function(data_wide){
  
  mean_var <- compute_avg_manifest_variance(data_wide = data_wide)
  residual_start <- runif(n = 1, min = 0.20, max = 0.80) * mean_var
  
  #w <- .05
  #scale <- 5 #scale variable
  #b <- 2 #default value as per Asparouhov & Muthen (2019) 
  #r <- runif(n = 1, min = -0.5, max = 0.5) #random number
  #
  #starting_values_list[[error]] <-  w + scale*b*r #insert starting value into list 
  
  return(list('epsilon' = residual_start))
}

compute_avg_manifest_variance <- function(data_wide) {
  
  #compute variance at each time point and return in a vector 
  measurement_day_var <- apply(X = data_wide[ ,2:ncol(data_wide)], MARGIN = 2, FUN = var)
  
  #compute mean of variances 
  mean_var <- mean(measurement_day_var)
  
  return(mean_var)
}














