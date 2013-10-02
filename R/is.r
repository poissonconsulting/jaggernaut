
is_scalar <- function (x) {
  return (is.vector(x) && length(x) == 1)
}

is_length <- function (x) {
  return (is.vector(x) && length(x) >= 1)
}

is_defined <- function (x) {
  return (is.vector(x) & all(!is.na(x)))
}

is_indicator <- function (x) {
 return (is.logical(x) & is_scalar(x) & is_defined(x)) 
}

is_named <- function (x) {
  return (!is.null(names(x))) 
}

is.mcarray <- function (object) {
  inherits(object, "mcarray")
}

is.jags_mcmc <- function (object) {
  inherits(object, "jags_mcmc")
}

is.jagr_model <- function (object) {
  inherits(object, "jagr_model")
}

is.jags_model <- function (object) {
  inherits(object, "jags_model")
}

is.jags_data_model <- function (object) {
  inherits(object, "jags_data_model")
}

is.jagr_analysis <- function (object) {
  inherits(object, "jagr_analysis")
}

is.jagr_simulation <- function (object) {
  inherits(object, "jagr_simulation")
}

is.jags_analysis <- function (object) {
  inherits(object, "jags_analysis")
}

is.jags_simulation <- function (object) {
  inherits(object, "jags_simulation")
}

is.jags_sample <- function (object) {
  inherits(object, "jags_sample")
}

is_data_list <- function (data) {
  if (!is.list(data)) {
    return (FALSE)
  }
  if(is.data.frame(data)) {
    return (FALSE)
  }
  
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



