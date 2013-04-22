
#' @title Calculate parameter estimates
#'
#' @description
#' Calculates parameter estimates for a JAGS analysis
#' 
#' @param object a \code{jags_analysis} object
#' @param model an integer element specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param parm a character vector of the parameters to calculate the estimates
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr0} in the global options.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame of the parameter estimates with the median estimate and 
#' lower and upper credible limits as well as the percent relative error 
#' and significance 
#' @seealso \code{\link{jags_analysis}} and \code{\link{jaggernaut}}
#' @method coef jags_analysis
#' @export
coef.jags_analysis <- function (object, model = 1, parm = "fixed", level = "current", ...) {
 
  if(!is.jags_analysis(object))
    stop ("object should be class jags_analysis")
  
  old_opts <- opts_jagr0()
  on.exit(opts_jagr0(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr0(mode = level)
    level <- opts_jagr0("level")
    opts_jagr0(old_opts)
  }
  opts_jagr0(level = level)
  
  object <- subset(object, model = model)
  
  est <- calc_estimates(object,parameters = parm)
  
  est <- est[rownames(est) != "deviance",]

  return (est)
}
