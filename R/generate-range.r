
generate_range <- function (object, ...) {
  UseMethod("generate_range", object)
}

generate_range.dvariable <- function(object, length_out=30) {
  stop(paste("generate_range not available for objects of class",class(object)))
}

generate_range.vlogical<-function (object, length_out=30) {
  return (c(FALSE,TRUE))
}

generate_range.vinteger <- function (object, length_out=30) {
  x <- seq(from=object$min, to=object$max, length.out=length_out)
  x <- unique(as.integer(round(x)))
  return (x)
}

generate_range.vnumeric <- function (object, length_out=30) {
  return (seq(from=object$min, to=object$max, length.out=length_out))
}

generate_range.vfactor<-function (object, length_out=30) {
  return (object$levels)
}

generate_range.vdate <- function (object, length_out=30) {
  x <- seq(from=object$min, to=object$max, length.out=length_out)
  x <- unique(x)
  return (x)
}

generate_range.vposixt <- function (object, length_out=30) {
  x <- seq(from=object$min, to=object$max, length.out=length_out)
  x <- unique(x)
  return (x)
}
