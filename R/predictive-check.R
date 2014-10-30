#' @title JAGS analysis posterior predictive checks
#'
#' @description
#' Calculate posterior predictive check p-value for a JAGS analysis
#' 
#' @param object a jags_analysis object.
#' @param parm a character vector specifying the discrepancy 
#' derived parameters (default = "discrepancy").
#' @param model a count or string specifying the jags model to select.
#' @param derived_code a character scalar defining a block in the 
#' JAGS dialect of  the BUGS language that defines the discrepancy(s).
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param estimate a character scalar indicating whether the point estimate 
#' should be the "mean" or the "median". By default the estimate is as 
#' currently defined by \code{opts_jagr} in the global options.  
#' @param ... further arguments passed to or from other methods.
#' @return a coef table of the posterior predictive check p-values(s)
#' @seealso \code{\link{jags_model}}, \code{\link{jags_analysis}}
#' ,\code{\link{predict.jags_analysis}} and \code{\link{coef.jags_analysis}}
#' @export
predictive_check <- function (object, parm = "discrepancy",  model = 1, 
                              derived_code = NULL,
                              level = "current", estimate = "current",
                              ...) {
  
  assert_that(is.jags_analysis(object))
  assert_that(is.character(parm))
  assert_that(not_empty(parm))  
  assert_that(noNA(parm))
  assert_that(level != "no")
  
  x <- data.frame()
  for(pm in parm) {
    p <- predict(object, parm = pm, model = model, 
                 derived_code = derived_code, 
                 level = level, estimate = estimate, ...)
    p <- p[,c("estimate", "lower", "upper", "sd", "error", "significance")]
    p <- cbind(data.frame(parameter = pm), p)
    x <- rbind(x, p)
  }
  x$parameter <- as.character(x$parameter)
  x
}
