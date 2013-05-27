
#' @title Calculate parameter estimates
#'
#' @description
#' Calculates parameter estimates for a JAGS analysis
#' 
#' @param object a \code{jags_analysis} object
#' @param model_number an integer element specifying the model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param parm a character vector of the parameters to calculate the estimates
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame of the parameter estimates with the median estimate and 
#' lower and upper credible limits as well as the percent relative error 
#' and significance 
#' @seealso \code{\link{jags_analysis}} and \code{\link{jaggernaut}}
#' @method coef jags_analysis
#' @export
coef.jags_analysis <- function (object, model_number = 1, parm = "fixed", as_list = FALSE, level = "current", ...) {
 
  if (!is.jags_analysis(object)) {
    stop ("object must be a jags_analysis")
  }
  
  if (is.character(parm)) {
    if (!length(parm)) {
      stop("parm must be at least length one")
    }
  } else {
    stop ("parm must be character vector")
  }
  
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  }
  
  object <- subset(object, model_number = model_number)
  
  parm <- get_parm (object, parm)
  
  est <- calc_estimates(object, parm = parm, level = level)
  
  est <- est[rownames(est) != "deviance",]
  
  if(as_list) {
    est <- extract_estimates(est)
  }

  return (est)
}
