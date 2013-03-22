
#' @title Calculate derived values for a JAGS analysis
#'
#' @description
#' Calculates values for a derived parameter.
#' 
#' @param object a janalysis object.
#' @param parameter a character element naming the derived parameter of interest.
#' @param data a data.frame of the data values over which to calculate the
#' expected values of the derived parameter. If data.frame is "" xx.
#' @param base a boolean element indicating whether or not to express 
#' the expected value as a percent change of a base level or a data frame 
#' defining the base level.
#' @param values a data frame with a single row that defines the value of particular
#' variables. The variables in data and base are replaced by the corresponding values.
#' @param derived_model a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameter. 
#' If NULL the value is taken from the JAGS model for which the JAGS analysis was performed. 
#' @param random a named list which specifies which parameters to treat 
#' as random variables. If NULL the value is taken from the JAGS model for which the JAGS analysis was performed.
#' @param length.out an integer element indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @param estimates a logical scalar indicating whether to return the individual
#' iterations or the median and 95% credibility intervals.
#' @param average a logical scalar indicating whether to model average - not yet implemented
#' @return the input data frame with the median and 95% credibility intervals 
#' (or iterations) for
#' the derived parameter of interest
#' @seealso \code{\link{jmodel}}, \code{\link[jaggernaut]{janalysis}}
#' @examples
#' model <- jmodel(
#'  model = "model { 
#'    bLambda ~ dunif(0,10) 
#'    for (i in 1:nrow) { 
#'      x[i]~dpois(bLambda) 
#'    } 
#'  }",
#'  derived_model = "model { 
#'    for (i in 1:nrow) { 
#'      eResidual[i] <- x[i] - bLambda
#'    } 
#'  }",
#'  select = c("x")
#' )
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' derived(analysis, "eResidual", data = NULL)
#' @export 
derived <- function (object, parameter, data = "", base = FALSE, 
                           values = NULL, derived_model = NULL, random = NULL, 
                           length.out = 30, estimates = TRUE,  average = FALSE) {
  
  if (!is.janalysis(object))
    stop ("object should be class janalysis")
 
  return (calc_expected(object, 
                         parameter = parameter, data = data, 
                         base = base, values = values, 
                        derived_model = derived_model, random = random, 
                         length.out = length.out, 
                         calc_estimates = estimates))
  
}
  