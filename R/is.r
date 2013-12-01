
is_all_informative <- function (x) {
  return (!is.null(x) && !any(is.na(x)) && !any(x == ""))
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
  return (is.character(x))
}

is_factor <- function (x) {
  return (is.factor(x))
}

is_informative_logical_scalar <- function (x) {
  return (is_all_informative(x) && is_logical(x) && is_scalar(x))
}

is_informative_integer_scalar <- function (x) {
  return (is_all_informative(x) && is_integer(x) && is_scalar(x))
}

is_informative_numeric_scalar <- function (x) {
  return (is_all_informative(x) && is_numeric(x) && is_scalar(x))
}

is_informative_character_scalar <- function (x) {
  return (is_all_informative(x) && is_character(x) && is_vector(x))
}

is_all_informative_logical_vector <- function (x) {
  return (is_all_informative(x) && is_logical(x) && is_vector(x))
}

is_all_informative_integer_vector <- function (x) {
  return (is_all_informative(x) && is_integer(x) && is_vector(x))
}

is_all_informative_numeric_vector <- function (x) {
  return (is_all_informative(x) && is_numeric(x) && is_vector(x))
}

is_all_informative_character_vector <- function (x) {
  return (is_all_informative(x) && is_character(x) && is_vector(x))
}

is_bounded <- function (x, min = -Inf, max = Inf) {
  stopifnot(is_informative_numeric_scalar(min))
  stopifnot(is_informative_numeric_scalar(max))
  return (all(x >= min & x <= max))
}

is_named <- function (x) {
  return (!is.null(names(x)))
}

is_one_model <- function (x) {
  return (nmodels(x) == 1)
}

is_data_list <- function (data) {
  if (!is.list(data)) {
    return (FALSE)
  }
  if(is.data.frame(data)) {
    return (FALSE)
  }
  
  if(length(data) == 0)
    return (TRUE)
  
  names <- names(data)
  if(is.null(names)) {
    return (FALSE)
  }
  if (any(names == "")) {
    return (FALSE)
  }
  
  bol <- sapply(data,inherits,"logical")
  
  for (class in c("integer","numeric","factor","Date","POSIXt","matrix","array")) {
    bol <- bol | sapply(data,inherits,class)
  }
  return (all(bol))
}
