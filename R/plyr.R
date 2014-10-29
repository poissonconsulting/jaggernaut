#' @importFrom plyr ldply
ldply_jg <- function (.data, .fun = NULL, ..., .recursive = 1) {
  
  .recursive <- as.integer(.recursive)

  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  stopifnot(is.integer(.recursive) && .recursive >= 1)
  
  if (.recursive == 1) {
    x <- ldply(.data = .data, .fun = .fun, ...)
  } else {
    fun1 <- function (.data, fun2, ..., recursive) {
      return (ldply_jg(.data = .data, .fun = fun2, ..., .recursive = recursive))      
    }
    .recursive <- .recursive - 1
    x <- ldply_jg(.data = .data, .fun = fun1, fun2 = .fun, ..., 
                  recursive = .recursive)
  }
  return (x)
}

#' @importFrom plyr llply
llply_jg <- function (.data, .fun, ..., .recursive = 1) {
  
  .recursive <- as.integer(.recursive)
  
  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  stopifnot(is.integer(.recursive) && .recursive >= 1)
  
  if (.recursive == 1) {
    x <- llply(.data = .data, .fun = .fun, ...)
  } else {
    fun1 <- function (.data, fun2, ..., recursive) {
      return (llply_jg(.data = .data, .fun = fun2, ..., .recursive = recursive))      
    }
    .recursive <- .recursive - 1
    x <- llply_jg(.data = .data, .fun = fun1, fun2 = .fun, ..., .recursive = .recursive, recursive = .recursive)
  }
  return (x)
}
