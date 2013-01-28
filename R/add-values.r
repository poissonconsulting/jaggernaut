
add_values <- function (power1, power2) {
  
  if(!is.gspower(power1))
    stop ("power1 should be class gspower")

  if(!is.gspower(power2))
    stop ("power2 should be class gspower")
  
  if(!identical(power1$nreps,power2$nreps))
    stop("power1 and power2 must have the same number of reps")
  
  if(!is_same(power1$model_block,power2$model_block))
    stop("power1 and power2 must have the same model_blocks")

  if(!is_same(power1$data_block,power2$data_block))
    stop("power1 and power2 must have the same data_blocks")  
  
 object<-list(
    model_block = power1$model_block,
    data_block= power1$data_block,
    values = rbind(power1$values,power2$values),
    nvalues = power1$nvalues + power2$nvalues,
    nreps = power1$nreps,
    nconfail = c(power1$nconfail,power2$nconfail), 
    analyses = c(power1$analyses,power2$analyses),
    parnames = NULL,
    parvalues = NULL,
    power = NULL
  )
  
  class(object) <- c("gspower")
  
  object <- calc_power(object)
  
  return (object)
}
