
predict_jagr <- function (object, parm, data, base, level, estimate, ...) {
  
  stopifnot(is.jags_analysis(object) && is_one_model(object))
  stopifnot(is_character_scalar(parm))
  stopifnot(is.jags_data(data))
  stopifnot(is.null(base) || is.jags_data(base))
  stopifnot(is_numeric_scalar(level))
  stopifnot(is_bounded(level, 0.75, 0.99) || level == 0)
  stopifnot(estimate %in% c("mean","median"))
  
  emcmc <- derived (object, parm = parm, data = data) 
    
  if (is.data.frame(base)) {
    
    base <- derived (object, parm = parm, data = base)
    
    base <- multiply (base, nrow(data))   
    emcmc <- (emcmc - base) / base
  }
  
  if(level != 0) {
    emcmc <- coef (emcmc, parm = parm, level = level, estimate = estimate)
  } else {
    emcmc <- as.data.frame(t(as.matrix (emcmc, parm)))
  }
  
  if (is.data.frame(data)) {
    emcmc <- cbind (data,emcmc)
    class(emcmc) <- c("data.frame","jags_sample")
  }
  return (emcmc)
}
