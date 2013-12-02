
is.mcarray <- function (x) {
  return (inherits(x, "mcarray"))
}

is.list_mcarray <- function (x) {
  return (is.list(x) && all(unlist(lapply(x, is.mcarray))))
}

is.jags <- function (x) {
  return (inherits(x, "jags"))
}

is.list_jags <- function (x) {
  return (is.list(x) && all(unlist(lapply(x, is.jags))))
}

is.jagr_data <- function (x) {
  return (inherits(x, "jagr_data"))
}

is.jags_data <- function (x) {
  return (is.jags_data_list(x))
}

#' @title Test for objects of class jags_data_list
#'
#' @description
#' Tests for objects of class \code{jags_data_list}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_data_list}.
#' @seealso \code{\link{as.jags_data_list}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_data_list <- function (x) {
  return (inherits(x, "jags_data_list"))
}

#' @title Test for objects of class jags_data_frame
#'
#' @description
#' Tests for objects of class \code{jags_data_frame}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_data_frame}.
#' @seealso \code{\link{as.jags_data_frame}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_data_frame <- function (x) {
  return (inherits(x, "jags_data_frame"))
}

is.jagr_chains <- function (x) {
  return (inherits(x, "jagr_chains"))
}
  
is.jagr_model <- function (x) {
  return (inherits(x, "jagr_model"))
}

#' @title Test for objects of class jags_data_model
#'
#' @description
#' Tests for objects of class \code{jags_data_model}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_data_model}.
#' @seealso \code{\link{jags_data_model}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_data_model <- function (x) {
  return (inherits(x, "jags_data_model"))
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

#' @title Test for objects of class jags_simulation
#'
#' @description
#' Tests for objects of class \code{jags_simulation}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_simulation}.
#' @seealso \code{\link{jags_simulation}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_simulation <- function (x) {
  return (inherits(x, "jags_simulation"))
}

#' @title Test for objects of class jags_power_analysis
#'
#' @description
#' Tests for objects of class \code{jags_power_analysis}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_power_analysis}.
#' @seealso \code{\link{jags_power_analysis}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_power_analysis <- function (x) {
  return (inherits(x, "jags_power_analysis"))
}

#' @title Test for objects of class jags_sample
#'
#' @description
#' Tests for objects of class \code{jags_sample}.  
#' 
#' @param x the object to be tested.
#' @return a logical element indicating whether or not \code{x} is of 
#' class \code{jags_sample}.
#' @seealso \code{\link{jags_sample}} and 
#' \code{\link{jaggernaut}}.
#' @export
is.jags_sample <- function (x) {
  return (inherits(x, "jags_sample"))
}
