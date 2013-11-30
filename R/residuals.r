
#' @title JAGS analysis residuals
#'
#' @description
#' Calculate residuals with estimates using derived code 
#' in a JAGS analysis
#' 
#' @param object a jags_analysis object.
#' @param parm a character element naming the derived parameter for which 
#' the estimates should be calculated.
#' @param model_number an integer vector specifying the model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param derived_code a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameters for each row of data. 
#' If NULL derived_code is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param random_effects a named list which specifies which parameters to treat 
#' as random variables. If NULL random is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param estimate a character scalar specifying whether the point estimate should
#' be the "mean" or the "median" or a character scalar which mode the level should be #' taken from. By default the
#' estimate is as currently specified by \code{opts_jagr} in the global options.
#' @param data the dataset for which to calculate the residuals. By default newdata is NULL as the residuals are typically calculated on the original dataset.
#' @param ... further arguments passed to or from other methods.
#' @return the input data frame with the median estimate and credibility intervals for the residuals
#' @seealso \code{\link{jags_model}} and \code{\link{jags_analysis}}
#' @method residuals jags_analysis
#' @export 
residuals.jags_analysis <- function (object, 
                                   parm = "residual", model_number = 1, 
                                   derived_code = NULL, random_effects = NULL, 
                                   level = "current", estimate = "current",
                                     data = NULL, ...) {
  
  return (predict(object, newdata = data, parm = parm, model = model_number, 
                  derived_code = derived_code, random_effects = random_effects, 
                  level = level, estimate = estimate, ...))
}
