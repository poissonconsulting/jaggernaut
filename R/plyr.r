
ddply_jg <- function (.data, .variables, .fun, ..., .drop = TRUE, .parallel = FALSE) {
  
  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  
  parallel <- .parallel && opts_jagr("parallel") && opts_jagr("mode") != "debug" && length(.data) > 1
  
  x <- plyr::ddply(.data = .data, .variables = .variables, 
                   .fun = .fun, ..., .drop = .drop, .parallel = parallel)
  
  return (x)
}

ldply_jg <- function (.data, .fun = NULL, ..., .recursive = 1, .parallel = FALSE) {
  
  .recursive <- as.integer(.recursive)
  
  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  stopifnot(is.integer(.recursive) && .recursive >= 1)
  
  parallel <- .parallel && opts_jagr("parallel") && opts_jagr("mode") != "debug" && length(.data) > 1
  
  if (.recursive == 1) {
    x <- plyr::ldply(.data = .data, .fun = .fun, ..., .parallel = parallel)
  } else {
    fun1 <- function (.data, fun2, ..., recursive, parallel) {
      return (ldply_jg(.data = .data, .fun = fun2, ..., .recursive = recursive,
                       .parallel = parallel))      
    }
    .recursive <- .recursive - 1
    x <- ldply_jg(.data = .data, .fun = fun1, fun2 = .fun, ..., 
                  recursive = .recursive, parallel = .parallel)
  }
  return (x)
}

llply_jg <- function (.data, .fun, ..., .recursive = 1, .parallel = FALSE) {
  
  .recursive <- as.integer(.recursive)
  
  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  stopifnot(is.integer(.recursive) && .recursive >= 1)
  
  parallel <- .parallel && opts_jagr("parallel") && opts_jagr("mode") != "debug" && length(.data) > 1
  
  if (.recursive == 1) {
    x <- plyr::llply(.data = .data, .fun = .fun, ..., .parallel = parallel)
  } else {
    fun1 <- function (.data, fun2, ..., recursive, parallel) {
      return (llply_jg(.data = .data, .fun = fun2, ..., .recursive = recursive,
                       .parallel = parallel))      
    }
    .recursive <- .recursive - 1
    x <- llply_jg(.data = .data, .fun = fun1, fun2 = .fun, ..., recursive = .recursive, parallel = .parallel)
  }
  return (x)
}
