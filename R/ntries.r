
#' @export
ntries <- function (object, ...) {
  UseMethod("ntries", object)
}

"ntries<-" <- function (object, value) {
  UseMethod("ntries<-", object)
}

#' @method ntries jags_data_list
#' @export 
ntries.jags_data_list <- function (object, ...) {
  return (attr(object, "ntries"))
}

ntries_jags_data_list <- function (object, ...) {
  stopifnot(is.jags_data_list(object))
  return (return (ntries(object, ...)))
}

#' @method ntries jags_simulation
#' @export
ntries.jags_simulation <- function (object, combine = FALSE, ...) {
  
  lapply_ntries_jags_data_list <- function (object) {    
    return (lapply(object, ntries_jags_data_list))
  }
  
  data <- data_jags(object)
  
  ntries <- lapply(data, lapply_ntries_jags_data_list)
  
  ntries <- matrixise(ntries)
  ntries <- name_object(t(ntries),c("replicate","value"))
  
  if(!combine)
    return (ntries)
  return (apply(ntries, MARGIN = 2, FUN = mean))
}

"ntries<-.jags_data_list" <- function (object, value) {
  stopifnot(is.numeric(value) && length(value) == 1)
  
  value <- as.integer(value)
  stopifnot(value %in% 1:10)
  
  attr(object, "ntries") <- value 
  
  return (object)
}
