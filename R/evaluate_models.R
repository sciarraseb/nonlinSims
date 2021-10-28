#' Fits model with several sets of starting values.   
#'
#' Description of function.
#' @md
#' @param data number of measurements
#' @param param_table table containing parameter values for each person
#' @param measurement_days days of measurement
#' @param time_period length of time over which measurements are taken
#' @return Returns a data table.
#' @export
evaluate_models <- function(initial_model, starting_values) {
  
  #load OpenMx in parallel version
  require(snowfall)
  sfInit(parallel = TRUE, cpus = detectCores() - 1) #save 1 core to prevent computer from crashing
  sfLibrary(OpenMx)
  
  #assemble parent model and set each submodel to have a different set of starting values 
  merged_model <- assemble_model_starts(model = initial_model, starting_values = starting_values)
  
  #run each submodell unsafe mdodel initialized to prevent any premature exiting
  fit_results <- mxRun(merged_model, unsafe = T) 
  
  primary_model_info <- omxSapply(x = fit_results$submodels, extract_model_info)
  
  #sometimes as.data.table(data.table::transpose((omxSapply(x = fit_results$submodels, extract_model_info))))
  #or data.table(t(omxSapply(x = fit_results$submodels, extract_model_info)))

  primary_model_info_dt <-data.table(t(omxSapply(x = fit_results$submodels, extract_model_info)))
  colnames(primary_model_info_dt) <-  c("model_num", "status", "Minus2LogLikelihood", "iterations")

  #convert columns to numeric values
  primary_model_info_dt$Minus2LogLikelihood <- as.numeric(primary_model_info_dt$Minus2LogLikelihood)
  primary_model_info_dt$iterations <- as.numeric(primary_model_info_dt$iterations)
  
  #order rows by fit 
  primary_model_info_dt <- primary_model_info_dt[order(status, Minus2LogLikelihood, status, iterations)]
  
  #primary_model_info_dt[order(Minus2LogLikelihood)]
  
  #close cluster
  sfStop()
  
  return(primary_model_info_dt)
}

extract_model_info<- function(submodel){
  
  primary_info  <- c(submodel$name,
                     submodel$output$status$code,
                     submodel$output$Minus2LogLikelihood,
                     submodel$output$iterations)
  #round(model$expectation$output$weights[1], 4)) #temporarily omitted 
  return(primary_info)
}
  
assemble_model_starts <- function(model, starting_values) {
  
  parent_model <- mxModel(name = "parent_model")
  sub_models <- lapply(X = 1:nrow(starting_values), FUN = update_starting_values, model = model, starting_values = starting_values)
  #sub_models <- lapply(X = 1:nrow(starting_values), FUN = mxAutoStart(), model = model, starting_values = starting_values)
  
  merged_model <- mxModel(parent_model, sub_models)
  
  return(merged_model)
}
 
update_starting_values <- function(model_number, model, starting_values){
  
  param_name_order <- names(omxGetParameters(model))

    #set model with new starting values  
    model_name <- paste("Iteration", model_number, sep="")
    new_model <- omxSetParameters(model = model, 
                                  name = model_name,
                                  labels = param_name_order, 
                                  values = as.numeric(setcolorder(x = starting_values[model_number, ], 
                                                                  neworder = param_name_order)))
    
    #return model with new starting values  
    return(new_model)
}

