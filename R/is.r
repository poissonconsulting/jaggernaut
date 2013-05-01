
is_scalar <- function (x) {
  return (is.vector(x) && length(x) == 1)
}

is.mcarray <- function (object) {
  inherits(object, "mcarray")
}

is.jags_mcmc <- function (object) {
  inherits(object, "jags_mcmc")
}

is.jags_model <- function (object) {
  inherits(object, "jags_model")
}

is.jagr_analysis <- function (object) {
  inherits(object, "jagr_analysis")
}

is.jags_analysis <- function (object) {
  inherits(object, "jags_analysis")
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



