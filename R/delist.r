
delist <- function (object) {
  
  if (!is.list(object) || is.data.frame(object) || length(object) != 1)
    return (object)
  
  object <- object[[1]]
  
  return (delist(object))
}
