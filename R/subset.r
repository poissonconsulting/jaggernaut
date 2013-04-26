
subset.jags_analysis <- function (x, model_number = 0, ...)
{   
  if (!is.numeric(model_number))
    stop ("model_number must be class numeric")
  
  if (length(model_number) != 1)
    stop ("model_number must be length one")
    
  if(!(model_number >= 0 & model_number <= x$n.model))
    stop(paste("model_number must lie between 0 and the number of models (in this case",x$n.model,")"))
  
  model_number <- as.integer(model_number)
  
  if (x$n.model == 1)
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
