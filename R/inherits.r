
is.mcarray <- function (object)
  inherits(object, "mcarray")

is.jagr_chains <- function (object)
  inherits(object, "jagr_chains")

is.jagr_model <- function (object)
  inherits(object, "jagr_model")

is.jags_data_model <- function (object)
  inherits(object, "jags_data_model")

is.jagr_analysis_model <- function (object)
  inherits(object, "jagr_analysis_model")

is.jags_model <- function (object)
  inherits(object, "jags_model")

is.jagr_power_analysis <- function (object)
  inherits(object, "jagr_power_analysis")

is.jagr_analysis <- function (object)
  inherits(object, "jagr_analysis")

is.jags_analysis <- function (object)
  inherits(object, "jags_analysis")

is.jags_simulation <- function (object)
  inherits(object, "jags_simulation")

is.jags_power_analysis <- function (object)
  inherits(object, "jags_power_analysis")
