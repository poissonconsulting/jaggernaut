#' @title Test Examples
#'
#' @description 
#' Test examples to be run manually by package maintainer.
#' @name test_examples
#' @seealso \code{\link{jaggernaut}}
#' @examples
#' \dontrun{
#' 
#' model1 <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0, 10^-2) 
#'  for (i in 1:length(x)) { 
#'    x[i] ~ dpois(bLambda) 
#'  } 
#'}")
#'
#' model2 <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0, 5^-2) 
#'  for (i in 1:length(x)) { 
#'    x[i] ~ dpois(bLambda) 
#'  } 
#'}")
#'
#' models <- combine(model1, model2)
#' data <- data.frame(x = rpois(100,1))
#' 
#' analysis <- jags_analysis (model1, data, mode = "demo")
#' analysis <- jags_analysis (model2, data, mode = "demo")
#' analysis <- jags_analysis (models, data, mode = "demo")
#' 
#' registerDoParallel(2) 
#' opts_jagr(parallel = TRUE)
#' 
#' analysis <- jags_analysis (model1, data, mode = "demo")
#' analysis <- jags_analysis (model2, data, mode = "demo")
#' analysis <- jags_analysis (models, data, mode = "demo")
#' 
#' #' # stop cluster if registered parallel backend in windows
#' if(.Platform$OS.type == "windows") stopImplicitCluster()
#' 
#' opts_jagr(parallel = FALSE)
#' registerDoSEQ()
#' 
#' registerDoParallel(4) 
#' opts_jagr(parallel = TRUE)
#' 
#' analysis <- jags_analysis (model1, data, mode = "demo")
#' analysis <- jags_analysis (model2, data, mode = "demo")
#' analysis <- jags_analysis (models, data, mode = "demo")
#' 
#' #' # stop cluster if registered parallel backend in windows
#' if(.Platform$OS.type == "windows") stopImplicitCluster()
#' 
#' opts_jagr(parallel = FALSE)
#' registerDoSEQ()
#' 
#' registerDoParallel(4) 
#' opts_jagr(parallel = TRUE)
#' 
#' demo(peregrine, ask= FALSE)
#' demo(hm, ask = FALSE)
#' demo(fritillary, ask= FALSE)
#' demo(pinna, ask = FALSE)
#' demo(tits, ask = FALSE)
#' demo(p610, ask = FALSE)
#' 
#' # stop cluster if registered parallel backend in windows
#' if(.Platform$OS.type == "windows") stopImplicitCluster()
#' 
#' opts_jagr(parallel = FALSE)
#' registerDoSEQ()
#' }
#'
NULL
