
#' @title Subset a JAGS analysis
#'
#' @description 
#' Subsets a JAGS analysis.
#' 
#' @param x a janalysis object to subset
#' @param model an integer vector specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param ... further arguments to pass to or from other methods.
#' @return a janalysis object
#' @export
#' @method subset janalysis
#' @examples
#' mod1 <- model("model { 
#'  bLambda ~ dunif(0,10) 
#'  for (i in 1:nrow) { 
#'    x[i] ~ dpois(bLambda) 
#'  } 
#'}")
#' mod2 <- model("model { 
#'  bLambda ~ dunif(0,10)
#'  r ~ dgamma(0.1, 0.1) 
#'  for (i in 1:nrow) { 
#'    u[i] ~ dgamma(r,r)
#'    x[i] ~ dpois(bLambda * u[i]) 
#'  } 
#'}")
#' mods <- list(mod1, mod2)
#' dat <- data.frame(x = rpois(100,1))
#' ana <- analysis (mods, dat)
#' summary(ana)
#' summary(subset(ana,model = 0))
#' summary(subset(ana,model = 2))
subset.janalysis <- function (x, model = 0, ...)
{ 
  model <- as.integer(model)
  
  if(length(model) != 1) {
    stop("model should be an integer vector of length 1")
  }
    
  if(!model %in% 0:x$n.model)
    stop("model values cannot be less than 0 or greater than n.model")
  
  if (x$n.model == 1)
    return (x)
  
  newObject <- list()
  newObject$analyses <- list()
  if(model == 0) {
    newObject$analyses[[1]] <- x$analyses[[rownames(x$dic)[1]]]
    
  } else {
    newObject$analyses[[1]] <- x$analyses[[model]]
  }
  newObject$dic <- x$dic[rownames(x$dic) == paste0("Model",model),,drop=T]
  newObject$n.model <- 1
  
  class(newObject) <- "janalysis"
  
  return (newObject)
}
