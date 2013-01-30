
add_reps <- function (power, nreps = 10, ...) {
  
  if(!is.gspower(power))
    stop ("power should be class gspower")
  
  if(!(length(nreps) == 1 && nreps[1] >= 1))
    stop("nreps should be a single integer >= 1")
  
  pars <- power$parvalues
  names(pars) <- power$parnames
  
  power2 <- gspower (model_block = power$model_block, data_block = power$data_block, 
                    values = power$values, nreps = nreps, pars = pars, ...)

  power$nreps <- power$nreps + power2$nreps
  power$nconfail <- power$nconfail + power2$nconfail
  
  for (i in 1:length(power$analyses)) {
    power$analyses[[i]] <- c(power$analyses[[i]],power2$analyses[[i]])
  }
  
  
  
  power <- calc_power(power)
  
  return (power)
}
