
predict_internal <- function (object, parm, data, base, level, ...) {
  
  stopifnot(is.jags_analysis(object))
  stopifnot(nmodels(object) == 1)
  stopifnot(is.character(parm))
  stopifnot(length(parm) == 1)
  stopifnot(is.data.frame(data) || is_data_list(data))
  stopifnot(is.null(base) || (is.data.frame(base) && is.data.frame(data)))
  stopifnot(is.numeric(level))
  stopifnot(length(level) == 1)
  stopifnot(level == 0 || (level >= 0.75 && level <= 0.99))
  
  emcmc <- get_derived (object, monitor=parm, data = data) 
    
  if (is.data.frame(base)) {
    
    base <- get_derived (object, monitor = parm, data = base)
    
    base <- multiply (base, nrow(data))   
    emcmc <- (emcmc - base) / base
  }
  
  if(level != 0) {
    emcmc <- coef (emcmc, parm = parm, level = level)
  } else {
    emcmc <- as.data.frame(t(as.matrix (emcmc, parm)))
  }
  
  if (is.data.frame(data)) {
    emcmc <- cbind (data,emcmc)
    class(emcmc) <- c("data.frame","jags_sample")
  }
  return (emcmc)
}
