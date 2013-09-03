
#' @title Perform a JAGS simulation
#'
#' @description 
#' Performs a JAGS simulation by using a 
#' \code{jags_model} and a data frame of values  
#' to generate a simulation data frame using JAGS (Plummer 2012). 
#' 
#' @param model a \code{jags_model}.
#' @param values a data.frame of values.
#' @param quiet a logical element indicating whether to print updates.
#' @return a \code{jags_simulation} object
#' @references 
#' Plummer M (2012) JAGS Version 3.3.0 User Manual \url{http://sourceforge.net/projects/mcmc-jags/files/Manuals/}
#' @seealso \code{\link{jags_model}} and \code{\link{jaggernaut}}
#' @examples
#' 
#' mod <- jags_model("
#' data { 
#'  for (i in 1:nx) { 
#'    x[i] ~ dpois(bIntercept) 
#'    for (j in 1:nx) {
#'      y[i,j] ~ dpois(bIntercept) 
#'    }
#'  } 
#'  z <- bIntercept
#'}    
#' ")
#'
#' val <- data.frame(nx = 10, bIntercept = 10)
#' 
#' sim <- jags_simulation (mod, val, quiet = TRUE)
#' 
#' @export
jags_simulation <- function (model, values, quiet = opts_jagr("quiet")) {
  
  if (!is.jags_model(model)) 
    stop("model must be class jags_model")
  
  if(!(is.data.frame(values) && nrow(values) == 1))
    stop ("values must be a data frame with one row of parameter values")
  
  if(!is.logical(quiet))
    stop("quiet must be class logical")
  
  if(!length(quiet) == 1)
    stop("quiet must be a single value")
  
  if(is.na(quiet) == 1)
    stop("quiet must be TRUE or FALSE")
  
  if(!"basemod" %in% list.modules())
    load.module("basemod")  
  
  if(!"bugs" %in% list.modules())
    load.module("bugs")
  
  object <- jagr_simulation(model = model, data = values, quiet = quiet)
    
  est <- calc_estimates(object)
   
  est <- est[rownames(est) != "deviance",]
    
  data <- extract_estimates(est)[["estimate"]]
  
  object$simulated <- data
  
  class(object) <- "jags_simulation"
  
  return (object)
}
