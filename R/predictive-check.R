#' @title JAGS analysis posterior predictive checks
#'
#' @description
#' Calculate posterior predictive check p-value for a JAGS analysis
#' 
#' @param object a jags_analysis object.
#' @param parm a character vector specifying the discrepancy 
#' derived parameters (default = "discrepancy").
#' @param model_id a count or string specifying the jags model to select.
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
#' @examples
#' 
#' \dontrun{
#' model1 <- jags_model("model {
#' alpha ~ dnorm(0, 50^-2) 
#' beta ~ dnorm(0, 10^-2)
#' sigma ~ dunif(0, 10)
#' 
#' for(i in 1:length(Volume)) { 
#'   eMu[i] <- alpha + beta * Girth[i]
#'   Volume[i] ~ dnorm(eMu[i], sigma^-2)
#' } 
#' }",
#' derived_code = "data {
#' for(i in 1:length(Volume)) { 
#'   prediction[i] <- alpha + beta * Girth[i]
#'   
#'   simulated[i] ~ dnorm(prediction[i], sigma^-2)
#'   
#'   D_observed[i] <- log(dnorm(Volume[i], prediction[i], sigma^-2))
#'   D_simulated[i] <- log(dnorm(simulated[i], prediction[i], sigma^-2))
#' }
#' discrepancy <- sum(D_observed) - sum(D_simulated)
#' 
#' }",
#' select_data = c("Volume", "Girth*"))
#' 
#' data(trees)
#' analysis1 <- jags_analysis(model1, data = trees)
#' predictive_check(analysis1)
#' }
#'
#' @export
predictive_check <- function (object, parm = "discrepancy",  
                              model_id = default_model_id(object),
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
    p <- predict(object, parm = pm, model_id = model_id, 
                 derived_code = derived_code, 
                 level = level, estimate = estimate, ...)
    p <- p[,c("estimate", "lower", "upper", "sd", "error", "significance")]
    p <- cbind(data.frame(parameter = pm), p)
    x <- rbind(x, p)
  }
  rownames(x) <- as.character(x$parameter)
  x$parameter <- NULL
  x
}
