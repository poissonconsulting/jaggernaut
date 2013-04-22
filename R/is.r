
is.mcarray <- function (object) {
  inherits(object, "mcarray")
}

is.gsmcmc <- function (object) {
  inherits(object, "gsmcmc")
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

is.gssimulation <- function (object) {
  inherits(object, "gssimulation")
}

is.gspower <- function (object) {
  inherits(object, "gspower")
}


