
name_object <- function (object, name) {
  UseMethod("name_object", object)
}

name_object.default <- function (object, names) {
  return (object)
}

name_object.matrix <- function (object, names) {
  stopifnot(length(dim(object)) == length(names))
  
  dim <- dim(object)
  dimnames <- list()
  for (i in 1:length(names)) {
    dimnames[[i]] <- paste0(names[i], 1:dim[i])
  }
  dimnames(object) <- dimnames
  
  return (object)
}

name_object.array <- function (object, names) {
  stopifnot(length(dim(object)) == length(names))
  
  dim <- dim(object)
  dimnames <- list()
  for (i in 1:length(names)) {
    dimnames[[i]] <- paste0(names[i], 1:dim[i])
  }
  dimnames(object) <- dimnames
  
  return (object)
}

name_object_list <- function (object, names) {
  stopifnot(is.list(object))
  return (name_object(object, names))
}

name_object.list <- function (object, names) {
  
  names(object) <- paste0(names[1],1:length(object))
  
  if (length(names) > 1) 
    object <- lapply(object,name_object_list,names[-1])
  
  return (object)
}
