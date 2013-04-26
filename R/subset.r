
subset.jags_analysis <- function (x, model_number = 0, ...)
{   
  if (!is.jags_analysis(x))
    stop ("x must be class jags_analysis")
  
  if (!is.numeric(model_number))
    stop ("model_number must be class numeric")
  
  if (length(model_number) != 1)
    stop ("model_number must be length one")
    
  if(!(model_number >= 0 & model_number <= nmodel(x)))
    stop(paste("model_number must lie between 0 and the number of models (in this case",nmodel(x),")"))
  
  model_number <- as.integer(model_number)
  
  if (nmodel(x) == 1)
    return (x)
  
  newObject <- list()
  newObject$analyses <- list()
  if(model_number == 0) {
    newObject$analyses[[1]] <- x$analyses[[rownames(x$dic)[1]]]
    
  } else {
    newObject$analyses[[1]] <- x$analyses[[model_number]]
  }
  newObject$dic <- x$dic[rownames(x$dic) == paste0("Model",model_number),,drop=T]
  newObject$n.model <- 1
  
  class(newObject) <- "jags_analysis"
  
  return (newObject)
}
