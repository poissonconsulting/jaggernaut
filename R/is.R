#' @title Test convertible data list
#'
#' @description
#' Test whether x is a list of data suitable for conversion for
#' input into JAGS, WinBUGS or 
#' OpenBUGS.
#' @param x the object to test
#' @return A logical scalar
#' @seealso \code{\link{is_convertible_data}}
#' @examples
#' is_convertible_data_list(trees)
#' is_convertible_data_list(as.list(trees))
#' trees$Comment <- "text"
#' is_convertible_data_list(trees)
#' @export
is_convertible_data_list <- function (x) {
  
  if(!is.list(x) || is.data.frame(x) || !is.character(names(x)))
    return (FALSE)
  
  bol <- sapply(x, inherits, "logical")
  
  for (class in c("integer","numeric","factor","Date","POSIXt","matrix","array")) {
    bol <- bol | sapply(x, inherits, class)
  }
  all(bol)
}

#' @title Test data frame
#'
#' @description
#' Tests whether x is a data.frame suitable for conversion for input into 
#' JAGS, WinBUGS or 
#' OpenBUGS.
#' @param x the object to test
#' @return A logical scalar
#' @seealso \code{\link{is_convertible_data}}
#' @examples
#' is_convertible_data_frame(trees)
#' is_convertible_data_frame(as.list(trees))
#' trees$Comment <- "text"
#' is_convertible_data_frame(trees)
#' @export
is_convertible_data_frame <- function (x) {
  return (is.data.frame(x) && is_convertible_data_list(as.list(x)))
}

#' @title Test data set
#'
#' @description
#' Tests whether x is a data.frame or list of data suitable for conversion for 
#' input into 
#' JAGS, WinBUGS or OpenBUGS.
#' @param x the object to test
#' @return A logical scalar
#' @seealso \code{\link{is_convertible_data_list}} and 
#' \code{\link{is_convertible_data_frame}}
#' @examples
#' is_convertible_data(trees)
#' is_convertible_data(as.list(trees))
#' trees$Comment <- "text"
#' is_convertible_data(trees)
#' @export
is_convertible_data <- function (x) {
  return (is_convertible_data_frame(x) || is_convertible_data_list(x))
}

#' @title Test data set
#'
#' @description
#' Tests whether x is converted data that can be input into
#' JAGS, WinBUGS or OpenBUGS.
#' @param x the object to test
#' @return A flag (logical scalar) indicating whether converted data
#' @seealso \code{\link{is_convertible_data}} and 
#' \code{\link{convert_data}}
#' @examples
#' is_converted_data_list(trees)
#' is_converted_data_list(convert_data(trees))
#' is_converted_data_list(list(x = 1:10, y = 0.33, z = matrix(1:9, ncol = 3)))
#' is_converted_data_list(list(x = 1:10, y = 0.33, z = matrix(1:9, ncol = 3)))
#' is_converted_data_list(list(x = factor(1:10), y = 0.33, z = matrix(1:9, ncol = 3)))
#' @export
is_converted_data_list <- function (x) {
  if(is.data.frame(x) || !is.list(x))
    return (FALSE)
  
  all(sapply(x, is.numeric))
}

#' @title Test data set
#'
#' @description
#' Tests whether x is converted data that can be input into
#' JAGS, WinBUGS or OpenBUGS.
#' @param x the object to test
#' @return A flag (logical scalar) indicating whether converted data
#' @seealso \code{\link{is_convertible_data}} and 
#' \code{\link{convert_data}}
#' @examples
#' is_converted_data_frame(trees)
#' is_converted_data_frame(convert_data(trees))
#' @export
is_converted_data_frame <- function (x) {
  if(!is.data.frame(x))
    return (FALSE)
  
  all(sapply(x, is.numeric))
}

#' @title Test data set
#'
#' @description
#' Tests whether x is converted data that can be input into
#' JAGS, WinBUGS or OpenBUGS.
#' @param x the object to test
#' @return A flag (logical scalar) indicating whether converted data
#' @seealso \code{\link{is_convertible_data}} and 
#' \code{\link{convert_data}}
#' @examples
#' is_converted_data(trees)
#' is_converted_data(convert_data(trees))
#' is_converted_data(list(x = 1:10, y = 0.33, z = matrix(1:9, ncol = 3)))
#' is_converted_data(list(x = 1:10, y = 0.33, z = matrix(1:9, ncol = 3)))
#' is_converted_data(list(x = factor(1:10), y = 0.33, z = matrix(1:9, ncol = 3)))
#' @export
is_converted_data <- function (x) {
  if(!is.list(x))
    return (FALSE)
  
  all(sapply(x, is.numeric))
}

is.POSIXt <- function (x) {
  return (inherits(x, "POSIXt"))
}

is_null <- function (x) {
  return (is.null(x))
}

is_any_missing <- function (x) {
  return (is_null(x) || !noNA(x))
}

is_unique <- function (x) {
  return (!any(duplicated(x)))
}

is_scalar <- function (x) {
  return (is.vector(x) && length(x) == 1)
}

is_vector <- function (x) {
  return (is.vector(x) && length(x) >= 1)
}

is_logical <- function (x) {
  return (is.logical(x))
}

is_integer <- function (x) {
  return (is.integer(x) || all.equal(x, as.integer(x)) == TRUE)
}

is_numeric <- function (x) {
  return (is.numeric(x) || is.integer(x))
}

is_character <- function (x) {
  return (is.character(x) || is.factor(x))
}

is_factor <- function (x) {
  return (is.factor(x))
}

is_list <- function (x) {
  return (inherits(x, "list"))
}

is_function <- function (x) {
  return (is.function (x))
}

is_logical_scalar <- function (x) {
  return (!is_any_missing(x) && is_logical(x) && is_scalar(x))
}

is_integer_scalar <- function (x) {
  return (!is_any_missing(x) && is_integer(x) && is_scalar(x))
}

is_numeric_scalar <- function (x) {
  return (!is_any_missing(x) && is_numeric(x) && is_scalar(x))
}

is_character_scalar <- function (x) {
  return (!is_any_missing(x) && is_character(x) && is_vector(x))
}

is_logical_vector <- function (x) {
  return (!is_any_missing(x) && is_logical(x) && is_vector(x))
}

is_integer_vector <- function (x) {
  return (!is_any_missing(x) && is_integer(x) && is_vector(x))
}

is_numeric_vector <- function (x) {
  return (!is_any_missing(x) && is_numeric(x) && is_vector(x))
}

is_character_vector <- function (x) {
  return (!is_any_missing(x) && is_character(x) && is_vector(x))
}

is_FALSE <- function (x) {
  return (is_logical_scalar(x) && identical(x, FALSE))
}

is_TRUE <- function (x) {
  return (is_logical_scalar(x) && identical(x, TRUE))
}

is_named <- function (x) {
  return (is_character_vector(names(x)) && is_unique(names(x)))
}

is_named_list <- function (x) {
  return (is_list(x) && is_named(x))
}

is_bounded <- function (x, min = -Inf, max = Inf) {
  stopifnot(is_numeric_scalar(x))
  stopifnot(is_numeric_scalar(min))
  stopifnot(is_numeric_scalar(max))
  return (all(x >= min & x <= max))
}

is_one_model <- function (x) {
  return (nmodels(x) == 1)
}

is_list_mcarray <- function (x) {
  return (is.list(x) && all(unlist(lapply(x, is.mcarray))))
}

is_list_list <- function (x) {
  return (is_list(x) && all(unlist(lapply(x, is_list))))
}

is_list_jags <- function (x) {
  return (is.list(x) && all(unlist(lapply(x, is.jags))))
}

is_list_jags_sample <- function (x) {
  return (is.list(x) && all(unlist(lapply(x, is.jags_sample))))
}
