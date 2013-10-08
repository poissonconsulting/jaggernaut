
clist <- function (object, object2) {

  stopifnot(is.list(object))
  stopifnot(is.list(object2))

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
  return (object)
}
