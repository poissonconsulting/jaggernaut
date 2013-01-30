
add_iterations <- function (object, iteration) {
  UseMethod("add_iterations", object)
}

add_iterations.mcarray <- function (object, iteration) {
  if (!inherits (object, "mcarray"))
    stop ("object should be class mcarray")
  if (!inherits (iteration, "mcarray"))
    stop ("iteration should be class mcarray")
  if (nchain (object) != nchain (iteration))
    stop ("object and iteration should have the same number of chains")
  
  dimobj <- dim (object)
  dimiter <- dim (iteration)
  dnames <- names(dim (object))
      
  if (!identical(dimobj[-(length(dimobj)-1)],dimiter[-(length(dimiter)-1)]))
    stop ("object and iteration should have the same dimensions (except iterations)")

  class(object)<-"array"
  class(iteration)<-"array"
  object <- abind (object,iteration,along=length(dimobj)-1)

  names(dim(object)) <- dnames
  class(object)<-"mcarray"
  
  return (object)
}

add_iterations.list <- function (object, iteration) {
  if (!inherits (object, "list"))
    stop ("object should be class list")
  if (!inherits (iteration, "list"))
    stop ("iteration should be class list")
  if (length (object)!=length(iteration))
    stop ("object and iteration should be the same length")
  if (!identical(names(object),names(iteration)))
    stop ("object and iteration should have the same names")
  
  for (i in seq(along = object))
    object[[i]] <- add_iterations(object[[i]],iteration[[i]])
  
  return (object)
}