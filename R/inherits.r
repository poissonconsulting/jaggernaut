
is.mcarray <- function (object) {
  return (inherits(object, "mcarray"))
}

is.list_mcarray <- function (object) {
  return (is.list(object) && all(unlist(lapply(object, is.mcarray))))
}

is.jags <- function (object) {
  return (inherits(object, "jags"))
}

is.list_jags <- function (object) {
  return (is.list(object) && all(unlist(lapply(object, is.jags))))
}

is.jags_data <- function (object) {
  return (is.jags_data_list(object))
}

is.jags_data_list <- function (object) {
  return (inherits(object, "jags_data_list"))
}

is.jags_data_frame <- function (object) {
  return (inherits(object, "jags_data_frame"))
}

is.jagr_chains <- function (object) {
  return (inherits(object, "jagr_chains"))
}
  
is.jagr_model <- function (object) {
  return (inherits(object, "jagr_model"))
}
  
is.jags_data_model <- function (object) {
  return (inherits(object, "jags_data_model"))
}
  
is.jagr_analysis_model <- function (object) {
  return (inherits(object, "jagr_analysis_model"))
}
  
is.jags_model <- function (object) {
  return (inherits(object, "jags_model"))
}
  
is.jagr_power_analysis <- function (object) {
  return (inherits(object, "jagr_power_analysis"))
}
  
is.jagr_analysis <- function (object) {
  return (inherits(object, "jagr_analysis"))
}
  
is.jags_analysis <- function (object) {
  return (inherits(object, "jags_analysis"))
}
  
is.jags_simulation <- function (object) {
  return (inherits(object, "jags_simulation"))
}
  
is.jags_power_analysis <- function (object) {
  return (inherits(object, "jags_power_analysis"))
}
