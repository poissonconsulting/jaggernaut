
#' @title Subset a JAGS analysis
#'
#' @description 
#' Subsets a JAGS analysis.
#' 
#' 
#' @param object a janalysis object to subset
#' @param model an integer vector specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @return a janalysis object
#' @export
#' @examples
#' model1 <- jmodel("model { 
#'  bLambda ~ dunif(0,10) 
#'  for (i in 1:nrow) { 
#'    x[i] ~ dpois(bLambda) 
#'  } 
#'}")
#' model2 <- jmodel("model { 
#'  bLambda ~ dunif(0,10)
#'  r ~ dgamma(0.1, 0.1) 
#'  for (i in 1:nrow) { 
#'    u[i] ~ dgamma(r,r)
#'    x[i] ~ dpois(bLambda * u[i]) 
#'  } 
#'}")#' models <- list(model1, model2)
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (models, data)
#' summary(analysis)
#' summary(subset(analysis,model = 0))
#' summary(subset(analysis,model = 2))
subset.janalysis <- function (object, model = 0)
{ 
  model <- as.integer(model)
  
  if(length(model) != 1) {
    stop("model should be an integer vector of length 1")
  }
    
  if(!model %in% 0:object$n.model)
    stop("model values cannot be less than 0 or greater than n.model")
  
  newObject <- list()
  newObject$analyses <- list()
  if(model == 0) {
    newObject$analyses[[1]] <- object$analyses[[rownames(analysis$dic)[1]]]
    
  } else {
    newObject$analyses[[1]] <- object$analyses[[model]]
  }
  newObject$dic <- object$dic[rownames(object$dic) == paste0("Model",model),]
  newObject$n.model <- 1
  
  class(newObject) <- "janalysis"
  
  return (newObject)
}
