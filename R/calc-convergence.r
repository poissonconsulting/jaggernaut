#' @export
calc_convergence <- function (object, ...) {
  UseMethod("calc_convergence", object)
}

calc_convergence.gsmcmc <- function (object, summarise = FALSE, pars = NULL) {

  nsim <- nsim(object)
  
  mcmc <- as.mcmc.list (object)
  
  vars<-varnames(mcmc)
  
  bol<-rep(F, length(vars))
  
  svars <- sapply(vars,strsplit,split='[',fixed=T)
  for (i in 1:length(vars)) {
    bol[i] <- svars[[i]][1] %in% pars
  }
  
  vars<-vars[bol]
  rhat <- numeric()
  ind <- numeric()
  for (i in seq(along = vars)) {
    rhat[i] <- gelman.diag(mcmc[,vars[i]])$psrf[1]
    ind[i] <- effectiveSize(mcmc[,vars[i]])[1] / nsim * 100
  }
  convergence <- data.frame (
    convergence = round(rhat,2), 
    independence = round(ind,0),
    row.names = vars
  )
  
  convergence <- subset(convergence,!(is.na(convergence) & independence == 0))
  if (!summarise)
    return (convergence)
  
  independence <- min(convergence$independence)
  convergence <- max(convergence$convergence)
  convergence<-c(convergence,independence)
  names(convergence) <- c('convergence','independence')
  return (convergence)
}

#' Calculate convergence values for JAGS analysis
#'
#' Calculate convergence values for JAGS analysis
#' 
#' @param object a janalysis object
#' @param summarise a boolean value indicating whether or not to summarise
#' @param type xx
#' @return xx
#' @method calc_convergence janalysis
#' @S3method calc_convergence janalysis
#' @export
#' @examples
#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")
#' data <- data.frame(x = rpois(100,1))
#' analysis <- janalysis (model, data)
#' calc_convergence(analysis)
calc_convergence.janalysis <- function (object, summarise = TRUE, type = "all") {
  pars <- get_pars (object$model, type = type)
  return (calc_convergence (object$mcmc, summarise = summarise, pars = pars))
}


