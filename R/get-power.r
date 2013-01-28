
get_power <- function (object) {
  if(!is.gspower(object))
    stop ("object should be class gspower")
  
  return (cbind(object$values, as.data.frame(object$power)))
}