
is.mcarray <- function (x) {
  return (inherits(x, "mcarray"))
}

on_failure(is.mcarray) <- function(call, env) {
  paste0(deparse(call$x), " is not a mcarray")
}

is.jags <- function (x) {
  return (inherits(x, "jags"))
}

on_failure(is.mcarray) <- function(call, env) {
  paste0(deparse(call$x), " is not a jags")
}

is.jagr_chains <- function (x) {
  return (inherits(x, "jagr_chains"))
}

#' @title Test for objects of class jags_sample
#'
#' @description
#' Tests for objects of class \code{jags_sample}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_sample}.
#' @seealso \code{\link{merge_jags_samples}},  
#' \code{\link{ddply_jags_sample}} and \code{\link{coef.jags_sample}}.
#' @export
is.jags_sample <- function (x) {
  return (inherits(x, "jags_sample"))
}

is.jagr_model <- function (x) {
  return (inherits(x, "jagr_model"))
}
  
is.jagr_analysis_model <- function (x) {
  return (inherits(x, "jagr_analysis_model"))
}

#' @title Test for objects of class jags_model
#'
#' @description
#' Tests for objects of class \code{jags_model}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_model}.
#' @seealso \code{\link{jags_model}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_model <- function (x) {
  return (inherits(x, "jags_model"))
}

on_failure(is.jags_model) <- function(call, env) {
  paste0(deparse(call$x), " is not a jags_model")
}
  
is.jagr_power_analysis <- function (x) {
  return (inherits(x, "jagr_power_analysis"))
}
  
is.jagr_analysis <- function (x) {
  return (inherits(x, "jagr_analysis"))
}

#' @title Test for objects of class jags_analysis
#'
#' @description
#' Tests for objects of class \code{jags_analysis}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_analysis}.
#' @seealso \code{\link{jags_analysis}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_analysis <- function (x) {
  return (inherits(x, "jags_analysis"))
}

on_failure(is.jags_analysis) <- function(call, env) {
  paste0(deparse(call$x), " is not a jags_analysis")
}

#' @title Test for objects of class jags_aggregation
#'
#' @description
#' Tests for objects of class \code{jags_aggregation}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_aggregation}.
#' @export
is.jags_aggregation <- function (x) {
  return (inherits(x, "jags_aggregation"))
}
