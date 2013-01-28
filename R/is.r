
is.mcarray <- function (object) {
  inherits(object, "mcarray")
}

is.gsmcmc <- function (object) {
  inherits(object, "gsmcmc")
}

is.jmodel <- function (object) {
  inherits(object, "jmodel")
}

is.janalysis <- function (object) {
  inherits(object, "janalysis")
}

is.gssimulation <- function (object) {
  inherits(object, "gssimulation")
}

is.gspower <- function (object) {
  inherits(object, "gspower")
}


