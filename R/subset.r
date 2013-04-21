
subset.janalysis <- function (x, model = 0, ...)
{ 
  model <- as.integer(model)
  
  if(length(model) != 1) {
    stop("model should be an integer vector of length 1")
  }
    
  if(!model %in% 0:x$n.model)
    stop("model values cannot be less than 0 or greater than n.model")
  
  if (x$n.model == 1)
    return (x)
  
  newObject <- list()
  newObject$analyses <- list()
  if(model == 0) {
    newObject$analyses[[1]] <- x$analyses[[rownames(x$dic)[1]]]
    
  } else {
    newObject$analyses[[1]] <- x$analyses[[model]]
  }
  newObject$dic <- x$dic[rownames(x$dic) == paste0("Model",model),,drop=T]
  newObject$n.model <- 1
  
  class(newObject) <- "janalysis"
  
  return (newObject)
}
