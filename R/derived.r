
#' @title Calculate estimates for a derived parameter
#'
#' @description
#' Calculate estimates for a derived parameter in a JAGS analysis
#' 
#' @param object a jags_analysis object.
#' @param parameter a character element naming the derived parameter for which 
#' the estimates should be calculated.
#' @param data a data.frame of the data values over which to calculate the
#' estimates of the derived parameter or a character vector specify the variable
#' or variable combination
#' for which to calculate the estimates of the derived parameter.
#' If NULL (the default) the derived parameter is calculated for each row in the original data set.
#' @param base a boolean element indicating whether or not to express 
#' the expected value as a percent change of a base level or a data frame 
#' defining the base level.
#' @param values a data frame with a single row that defines the value of particular
#' variables. The variables in the arguments data and base are replaced by the corresponding values.
#' @param model an integer vector specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param derived_code a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameters for each row of data. 
#' If NULL derived_code is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param random a named list which specifies which parameters to treat 
#' as random variables. If NULL random is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param length_out an integer element indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @return the input data frame with the median and 95% credibility intervals 
#' (or iterations) for
#' the derived parameter of interest
#' @seealso \code{\link{jaggernaut}}, \code{\link{model}} and \code{\link{analysis}}
#' @export 
derived <- function (object, parameter, data = NULL, base = FALSE, 
                     values = NULL, model = 1, derived_code = NULL, random = NULL, 
                     length_out = 50) {
  
  conf_int <- TRUE
  
  if (!is.jags_analysis(object))
    stop ("object should be class jags_analysis")  

  
  object <- subset(object, model = model)

 
  return (calc_expected(object, 
                         parameter = parameter, data = data, 
                         base = base, values = values, 
                        derived_model = derived_code, random = random, 
                         length.out = length_out, 
                         calc_estimates = conf_int))
  
}
