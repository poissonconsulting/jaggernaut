
matrixise <- function (object) {
  
  stopifnot(is.list(object))
  
  values <- unlist(object)
  
  dim <- c(length(object),length(object[[1]]))
  
  stopifnot(prod(dim) == length(values)) 
  
  matrix <- matrix(values, nrow = dim[1], ncol = dim[2], byrow = TRUE)
  return (matrix)
}
