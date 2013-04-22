
#' @title JAGS analysis predictions
#'
#' @description
#' Calculate predictions with estimates for derived parameters 
#' in a JAGS analysis
#' 
#' @param object a jags_analysis object.
#' @param parm a character element naming the derived parameter for which 
#' the estimates should be calculated.
#' @param newdata a data.frame of the data values over which to calculate the
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
#' @param random_effects a named list which specifies which parameters to treat 
#' as random variables in the derived code. If NULL random_effects is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param length_out an integer element indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @param ... further arguments passed to or from other methods.
#' @return the input data frame with the median estimate and credibility intervals for
#' the derived parameter of interest
#' @seealso \code{\link{jags_model}}, \code{\link{jags_analysis}}
#' and \code{\link{jaggernaut}}
#' @method predict jags_analysis
#' @export 
predict.jags_analysis <- function (object, newdata = NULL, 
                                   parm = "prediction", base = FALSE, 
                                   values = NULL, model = 1, 
                                   derived_code = NULL, random_effects = NULL, 
                                   level = "current", length_out = 50, ...) {

  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
   if (!is.numeric(level)) {
     opts_jagr(mode = level)
     level <- opts_jagr("level")
     opts_jagr(old_opts)
   }
   opts_jagr(level = level)
  
  if (!is.jags_analysis(object))
    stop ("object should be class jags_analysis")  

  object <- subset(object, model = model)
 
  pred <- calc_expected(object, 
                         parameter = parm, data = newdata, 
                         base = base, values = values, 
                        derived_model = derived_code, random = random_effects, 
                         length.out = length_out, 
                         calc_estimates = T)
  rownames(pred) <- NULL
  
  return (pred)
  
}
