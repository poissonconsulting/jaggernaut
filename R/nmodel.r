
#' @title Number of models in JAGS analysis
#'
#' @description 
#' Gets the number of models in a JAGS analysis.
#'   
#' @param x a jags_analysis object
#' @seealso \code{\link{jags_model}} and \code{\link{jags_analysis}} 
#' @export
nmodel <- function (x) {
  
  if (!is.jags_analysis(x))
    stop ("x must be class jags_analysis")
  
  return (x$n.model)
}
