
#' @export
add_jags <- function (object1, object2, ...) {
  UseMethod("add_jags", object1)
}

#' @method add_jags jags_simulation
#' @export 
add_jags.jags_simulation <- function (object1, object2, mode = "current", ...)
{
  if(!is.jags_simulation(object1))
    stop("object1 should be of class jags_simulation")

  if(!is.jags_simulation(object2))
    stop("object2 should be of class jags_simulation")
  
  if(!identical(object1$data_model,object2$data_model))
    stop("objects must have identical data_models")
  
  if(any(!colnames(object1$values) %in% colnames(object2$values)))
    stop("objects must have values with the same names")
  
  if(any(!colnames(object2$values) %in% colnames(object1$values)))
    stop("objects must have values with the same names")
  
  object2$values <- subset(object2$values, select = colnames(object1$values))
  
  diff <- abs(object1$nrep - object2$nrep)
  
  if(diff != 0) {
    if (object1$nrep > object2$nrep) {
      object2 <- update_jags(object2, nrep = diff, mode = mode)
    } else {
      object1 <- update_jags(object1, nrep = diff, mode = mode)
    }
  }
  
  object1$values <- rbind(object1$values, object2$values)
  object1$nvalues <- nrow(object1$values)
  object1$simulated <- c(object1$simulated,object2$simulated)
  
  return (object1)
}
