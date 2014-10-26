
clist <- function (object, object2, recursive = 1) {
  
  recursive <- as.integer(recursive)

  stopifnot(is.list(object))
  stopifnot(is.list(object2))
  stopifnot(is.integer(recursive))
  stopifnot(recursive %in% 1:2)
  
  if(recursive == 1) {
    names <- names(object)
    names2 <- names(object2)
    
    len <- length(object)
    for (i in seq_along(object2)) {
      object[[i + len]] <- object2[[i]]
    }
    
    if(!is.null(names) && !is.null(names2)) {
      names(object) <- c(names, names2)
    } else
      names(object) <- NULL
  } else { 
    stopifnot(recursive == 2)
    stopifnot(length(object) == length(object2))
    
    for (i in 1:length(object)) {
      object[[i]] <- clist(object[[i]], object2[[i]])
    }
  }
  return (object)
}
