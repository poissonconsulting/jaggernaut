
generate_range <- function (object, ...) {
  UseMethod("generate_range", object)
}

generate_range.dlogical<-function (object, length.out=30) {
  return (c(FALSE,TRUE))
}

generate_range.dvariable <- function(object, length.out=30) {
  x <- seq(from=object$min, to=object$max, length.out=length.out)
  return (x)
}

generate_range.dinteger <- function (object, length.out=30) {
  x <- seq(from=object$min, to=object$max, length.out=length.out)
  x <- unique(as.integer(round(x)))
  return (x)
}

generate_range.dfactor<-function (object, length.out=30) {
  return (object$levels)
}

generate_range.ddate <- function (object, length.out=30) {
  x <- seq(from=object$min, to=object$max, length.out=length.out)
  x <- unique(x)
  return (x)
}

generate_range.dposixt <- function (object, length.out=30) {
  x <- seq(from=object$min, to=object$max, length.out=length.out)
  x <- unique(x)
  return (x)
}

