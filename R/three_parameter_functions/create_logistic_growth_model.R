#' Analyzes data with nonlinear latent growth curve model.
#'
#' Data are modelled with a structured latent growth curve model.
#' @md
#' @param data_wide wide version of data 
#' @param model_name name of model 
#' @export
create_logistic_growth_model <- function(data_wide, model_name, starting_values) {
  
  #initial checks 
  tryCatch(expr = model_name, error = function(e) {message("Error: model_name is not a character vector")})
  
  manifest_vars <- generate_manifest_var_names(data = data_wide)
  latent_vars <- c('diff', 'beta', 'gamma')
  
  measurement_days <- as.numeric(str_extract(string = names(data_wide[ 2:ncol(data_wide)]), pattern = '[^_]*$'))
  manifest_means <- compute_manifest_means(data_wide = data_wide)
  
  model <- mxModel(model = model_name,
                   type = 'RAM', independent = T,
                   mxData(observed = data_wide, type = 'raw'),
                   
                   manifestVars = manifest_vars,
                   latentVars = latent_vars,
                   
                   #Residual variances; by using one label, they are assumed to all be equal (homogeneity of variance)
                   mxPath(from = manifest_vars,
                          arrows=2, free=TRUE,  labels='epsilon', values = starting_values$epsilon), #  lbound = 1e-3),
                   
                   #Set manifest means to observed means
                   mxPath(from = 'one', to = manifest_vars, free = FALSE, arrows = 1, values = manifest_means),
                   
                   #Latent variable covariances and variances
                   mxPath(from = latent_vars,
                          connect='unique.pairs', arrows=2,
                          #aa(diff_rand), ab(cov_diff_beta), ac(cov_diff_gamma), bb(beta_rand), bc(var_beta_gamma), cc(gamma_rand)
                          free = c(TRUE,
                                   FALSE, FALSE,
                                   TRUE, FALSE, TRUE),
                          values=c(starting_values$diff_rand,
                                   NA,NA,
                                   starting_values$beta_rand, NA, starting_values$gamma_rand),
                          labels=c('diff_rand',
                                   'NA(cov_diff_beta)','NA(cov_diff_gamma)',
                                   'beta_rand', 'NA(var_beta_gamma)', 'gamma_rand'),
                          lbound = c(1e-3, 
                                     NA, NA, 
                                     2, NA, 1), 
                          ubound = c(2, 
                                     NA, NA, 
                                    90^2, NA, 90^2)),
                   
                   #Latent variable means (linear parameters). Note that the nonlinear parameters of beta and gamma do not have estimated means
                   mxPath(from = 'one', to = 'diff', free = TRUE, arrows = 1,
                          labels = 'diff_fixed', lbound = 0, ubound = 7, values = starting_values$diff_fixed),
                   
                   #Functional constraints
                   mxMatrix(type = 'Full', nrow = length(manifest_vars), ncol = 1, free = TRUE, 
                            labels = 'diff_fixed', name = 'd',  lbound = 0,  ubound = 7, values = starting_values$diff_fixed),
                   mxMatrix(type = 'Full', nrow = length(manifest_vars), ncol = 1, free = TRUE, 
                            labels = 'beta_fixed', name = 'b', lbound = 1, ubound = 360, values = starting_values$beta_fixed),
                   mxMatrix(type = 'Full', nrow = length(manifest_vars), ncol = 1, free = TRUE, 
                            labels = 'gamma_fixed', name = 'g', lbound = 1, ubound = 360, values = starting_values$gamma_fixed),
                   mxMatrix(type = 'Full', nrow = length(manifest_vars), ncol = 1, free = FALSE, 
                            values = measurement_days, name = 'time'),
                   
                   #Algebra specifying first partial derivatives; 
                   mxAlgebra(expression = 1/(1 + exp((b - time)/g)), name="Dl"),
                   mxAlgebra(expression = -(d * (exp((b - time)/g) * (1/g))/(1 + exp((b - time)/g))^2), name = 'Bl'),
                   mxAlgebra(expression =  d * (exp((b - time)/g) * ((b - time)/g^2))/(1 + exp((b - time)/g))^2, name = 'Gl'),
                   
                   #Factor loadings; all fixed and, importantly, constrained to change according to their partial derivatives (i.e., nonlinear functions) 
                   mxPath(from = 'diff', to = manifest_vars, arrows=1, free=FALSE,  
                          labels = sprintf(fmt = 'Dl[%d,1]', 1:(ncol(data_wide)-1))),  
                   mxPath(from='beta', to = manifest_vars, arrows=1,  free=FALSE,
                          labels =  sprintf(fmt = 'Bl[%d,1]', 1:(ncol(data_wide)-1))),
                   mxPath(from='gamma', to = manifest_vars, arrows=1,  free=FALSE,
                          labels =  sprintf(fmt = 'Gl[%d,1]', 1:(ncol(data_wide)-1))),

                   mxFitFunctionML(vector = FALSE)
                   
  )
  
  return(model)
}

generate_manifest_var_names <- function(data_wide) {
  
  manifest_names <- str_extract(string = names(data_wide[ , 2:ncol(data_wide)]), 
                                pattern = "^t\\d+_\\d+")
  return(manifest_names)
}

compute_manifest_means <- function(data_wide){
  
  manifest_means <- as.numeric(sapply(X = data_wide[2:ncol(data_wide)], FUN = mean))
  return(manifest_means)
}



