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
#'}",
#'model_id = "First Model")
#'
#' model2 <- jags_model("
#' model { 
#'  bLambda ~ dlnorm(0, 5^-2) 
#'  for (i in 1:length(x)) { 
#'    x[i] ~ dpois(bLambda) 
#'  } 
#'}",
#'model_id = "Second Model")
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
#'  
#' mod <- jags_model("
#'                   model {
#'                   alpha ~ dunif(-20, 20)
#'                   beta1 ~ dunif(-10, 10)
#'                   beta2 ~ dunif(-10, 10)
#'                   beta3 ~ dunif(-10, 10)
#'                   sigma ~ dunif(0, 5)
#'                  
#'                   for (i in 1:length(Year)) {
#'                   eps[i] ~ dnorm(0, sigma^-2)
#'                   eLogC[i] <- alpha + beta1 * Year[i]
#'                   + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'                   log(eC[i]) <- eLogC[i] + eps[i]
#'                   C[i] ~ dpois(eC[i])
#'                   }
#'                   }",
#'  derived_code = "data{
#'  for (i in 1:length(Year)) {
#'  log(prediction[i]) <- alpha + beta1 * Year[i]
#'  + beta2 * Year[i]^2 + beta3 * Year[i]^3
#'  }
#'  }",
#'   random_effects = list(eps = "Year"),
#'   select_data = c("C","Year*")
#'   )

#'  data(peregrine)
#'  dat <- peregrine

#'  dat$C <- dat$Pairs
#'  an <- jags_analysis (mod, dat, niters = 10^3)
#' 
#' # stop cluster if registered parallel backend in windows
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
