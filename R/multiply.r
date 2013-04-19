
multiply<- function (object, ...) {
  UseMethod("multiply", object)
}

multiply_mcarray <- function (object, times = 1) {
  if (!(is.integer(times) && length (times) == 1))
    stop ("times should be an integer")
  
  if (times < 1)
    stop ("times should be 1 or greater")
  
  if (times == 1)
    return (object)
  
  dnames <- names(dim (object))
  dim(object) <- c(1,dim(object))
    
  commas<- paste0(rep.int(",",length(dim(object))-1),collapse="")
  cmd <- paste0("object <- object[rep.int(1,times)",commas,"]",collapse="")
  
  eval(parse(text = cmd))
  
  names(dim(object)) <- c(NULL,dnames)
  class(object)<-'mcarray'
  
  return (object)
  
}

multiply.gsmcmc <- function (object, times = 1, ...) {

  object$mcmc <- lapply (object$mcmc,multiply_mcarray,times)
  
  return (object)
}
