
arrayicise <- function (object) {
  
  stopifnot(is.list(object))
  
  values <- unlist(object)
  
  dim <- length(object)
  object <- object[[1]]
  while (is.list(object)) {
    dim <- c(dim,length(object))
    object <- object[[1]]    
  }
  stopifnot(prod(dim) == length(values)) 
  
  array <- array(values, dim = dim)
  return (array)
}
