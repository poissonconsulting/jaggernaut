
arrayicise <- function (object) {

  if (!is.list(object))
    return (object)
  
  values <- unlist(object)
  
  dim <- length(object)
  object <- object[[1]]
  while (is.list(object)) {
    dim <- c(dim,length(object))
    object <- object[[1]]    
  }
  stopifnot(cumprod(dim) == length(values)) 
  
  array <- array(values, dim = dim)
  return (array)
}
