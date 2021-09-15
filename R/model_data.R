#' Analyzes data with nonlinear latent growth curve model.
#'
#' Description of function.
#' @md
#' @param data number of measurements
#' @param param_table table containing parameter values for each person
#' @param measurement_days days of measurement
#' @param time_period length of time over which measurements are taken
#' @return Returns a data table.
#' @export
create_logistic_growth_model <- function(data, model_name, diff_fixed_est, beta_fixed_est, gamma_fixed_est,
                                         var_diff_est, var_beta_est, var_gamma_est, error) {
  
  modelling_equation <- expression(d/(1 + e^((b-x)/g)))
  
  model <- mxModel(model = model_name,
                   type = 'RAM',
                   mxData(observed = data, type = 'raw'),
                   
                   manifestVars = generate_manifest_var_names(data = data),
                   latentVars = c('diff', 'beta', 'gamma'),
                   
                   #Manifest means
                   manMeans = mxPath(from = 'one', to = manifestVars, free = FALSE, arrows = 1, values = 0),
                   
                   #Residual variances
                   mxPath(from = manifestVars,
                          arrows=2, free=TRUE, values = epsilon, labels='epsilon'),
                   
                   #Latent variable covariances and variances
                   mxPath(from = latentVars,
                          connect='unique.pairs', arrows=2,
                          #aa(var_diff), ab(cov_diff_beta), ac(cov_diff_gamma), bb(var_beta), bc(var_beta_gamma), cc(var_gamma)
                          free = c(TRUE,
                                   TRUE, TRUE,
                                   TRUE, TRUE, TRUE),
                          values=c(var_diff,
                                   cov_diff_beta,cov_diff_gamma,
                                   var_beta, var_beta_gamma, var_gamma),
                          labels=c('var_diff',
                                   'cov_diff_beta','cov_diff_gamma',
                                   'var_beta', 'var_beta_gamma', 'var_gamma')),
                   
                   #Factor loadings
                   mxPath(from = 'diff', to = manifestVars, arrows=1, free=FALSE,
                          labels = c('Al[1,1]','Al[2,1]','Al[3,1]','Al[4,1]')),
                   mxPath(from='beta', to = c('t0', 't1', 't2', 't3'), arrows=1,  free=FALSE,
                          labels=c('Bl[1,1]','Bl[2,1]','Bl[3,1]','Bl[4,1]')),
                   mxPath(from='gamma', to = c('t0', 't1', 't2', 't3'), arrows=1,  free=FALSE,
                          labels=c('Gl[1,1]','Gl[2,1]','Gl[3,1]','Gl[4,1]')),
                   
                   #Latent variable means (linear parameters). Implicitly coded is that the nonlinear parameters do not have estimated means
                   mxPath(from = 'one', to = c('diff'),free = c(T, T), arrows = 1, values = diff_est,
                          labels = 'diff_est'),
                   
                   #time vector
                   #mxMatrix('Full', 4, 1, free = FALSE, values = c(0, 1, 2, 3), name = 'x'),
                   
                   #Functional constraints
                   mxMatrix('Full', 4, 1, free = TRUE, values = diff_est, labels = 'diff', name = 'd'),
                   mxMatrix('Full', 4, 1, free = TRUE, values = beta_est, labels = 'beta', name = 'b'),
                   mxMatrix('Full', 4, 1, free = TRUE, values = gamma_est, labels = 'gamma', name = 'g'),
                   
                   mxAlgebra(expression = D(modelling_equation, name = 'd'), name="Dl"),
                   mxAlgebra(expression =  D(modelling_equation, name = 'b'), name = 'Bl'),
                   mxAlgebra(expression =  D(modelling_equation, name = 'g'), name = 'Gl'),
                   
                   #Enable likelihood vector
                   mxFitFunctionML(vector = TRUE)
  )
  
  return(model)
}

generate_manifest_var_names <- function(data) {
  
  num_measurements <- data[ , uniqueN(measurement_day)]
  return(sprintf(fmt = 't%d', 0:(num_measurements-1)))
}



#D(expr = expression(d/(1 + e^((b-x)/t))), name = 'b')
