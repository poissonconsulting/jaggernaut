
is_scalar <- function (x)
  is.vector(x) && length(x) == 1

is_vector <- function (x)
  is.vector(x) && length(x) > 1

is_length <- function (x)
  is_scalar(x) || is_vector(x)

is_character <- function (x)
  is.character(x)

is_numeric <- function (x)
  is.numeric(x)

is_no_missing <- function (x)
  !any(is.na(x))

is_defined <- function (x)
  is_no_missing(x)

is_indicator <- function (x)
 is.logical(x) & is_scalar(x) & is_defined(x)

is_named <- function (x)
  !is.null(names(x)) 

is_one_model <- function (x) 
  nmodels(x) == 1

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
