
predict_internal <- function (object, parameter, data, object_data, base, level, ...) {
  
  stopifnot(is.jags_analysis(object))
  stopifnot(nmodels(object) == 1)
  stopifnot(is.character(parameter))
  stopifnot(length(parameter) == 1)
  stopifnot(is.data.frame(data) || is_data_list(data))
  stopifnot(is.null(base) || (is.data.frame(base) && is.data.frame(data)))
  stopifnot(is.numeric(level))
  stopifnot(length(level) == 1)
  stopifnot(level == 0 || (level >= 0.75 && level <= 0.99))
  
  emcmc <- get_derived (object, monitor=parameter, data = data, object_data = object_data)  
  
  if (is.data.frame(base)) {
    
    base <- get_derived (object, monitor = parameter, data = base, object_data = object_data)
    
    base <- multiply (base, nrow(data))   
    emcmc <- (emcmc - base) / base
  }
  
  if(level != 0) {
    emcmc <- coef (emcmc, parm = parameter, level = level)
  } else {
    emcmc <- as.data.frame(t(as.matrix (emcmc, parameter)))
  }
  
  if (is.data.frame(data)) {
    emcmc <- cbind (data,emcmc)
    class(emcmc) <- c("data.frame","jags_sample")
  }
  return (emcmc)
}