
is_null <- function (x) {
  return (is.null(x))
}

is_any_missing <- function (x) {
  return (is_null(x) || any(is.na(x)) || any(x == ""))
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
  return (is.list(x))
}

is_data_frame <- function (x) {
  return (is.data.frame(x))
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

is_named <- function (x) {
  return (is_character_vector(names(x)) && is_unique(names(x)))
}

is_named_list <- function (x) {
  return (is_list(x) && is_named(x))
}

is_bounded <- function (x, min = -Inf, max = Inf) {
  stopifnot(is_numeric_scalar(min))
  stopifnot(is_numeric_scalar(max))
  return (all(x >= min & x <= max))
}

is_data_list <- function (x) {
  
  if (!is_list(x) || is_data_frame(x) || length(x) == 0 || !is_named_list(x)) {
    return (FALSE)
  }
  
  bol <- sapply(x,inherits,"logical")
  
  for (class in c("integer","numeric","factor","Date","POSIXt","matrix","array")) {
    bol <- bol | sapply(x,inherits,class)
  }
  return (all(bol))
}

is_one_model <- function (x) {
  return (nmodels(x) == 1)
}
