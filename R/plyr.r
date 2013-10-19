
ddply_jg <- function (.data, .variables, .fun, ..., .drop = TRUE) {
  
  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  
  parallel <- opts_jagr("parallel") && opts_jagr("mode") != "debug" && length(.data) > 1
  
  x <- plyr::ddply(.data = .data, .variables = .variables, 
                   .fun = .fun, ..., .drop = .drop, .parallel = parallel)
  
  return (x)
}

ldply_jg <- function (.data, .fun = NULL, ...) {
  
  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  
  parallel <- opts_jagr("parallel") && opts_jagr("mode") != "debug" && length(.data) > 1
  
  x <- plyr::ldply(.data = .data, .fun = .fun, ..., .parallel = parallel)
  
  return (x)
}

llply_jg <- function (.data, .fun, ...) {
  
  stopifnot(is.list(.data) && length(.data) > 0)
  stopifnot(is.function(.fun))
  
  parallel <- opts_jagr("parallel") && opts_jagr("mode") != "debug" && length(.data) > 1
  
  x <- plyr::llply(.data = .data, .fun = .fun, ..., .parallel = parallel)
  
  return (x)
}
