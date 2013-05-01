
convergence_gsmcmc <- function (object, parm, ...) {
  
  stopifnot(is.gsmcmc(object))
  stopifnot(is.character(parm))
  stopifnot(length(parm) >= 1)
  
  nsim <- nsim(object)
  
  mcmc <- as.mcmc.list (object)
  
  vars<-coda::varnames(mcmc)
  
  bol<-rep(F, length(vars))
  
  svars <- sapply(vars,strsplit,split="[",fixed=T)
  for (i in 1:length(vars)) {
    bol[i] <- svars[[i]][1] %in% parm
  }
  
  vars<-vars[bol]
  rhat <- numeric()
  ind <- numeric()
  for (i in seq(along = vars)) {
    rhat[i] <- coda::gelman.diag(mcmc[,vars[i]])$psrf[1]
    ind[i] <- coda::effectiveSize(mcmc[,vars[i]])[1] / nsim * 100
  }
  convergence <- data.frame (
    convergence = round(rhat,2), 
    independence = round(ind,0),
    row.names = vars
  )
  
  convergence <- convergence[!(is.na(convergence$convergence) & convergence$independence == 0),]
  
  return (convergence)
}

convergence_jagr_analysis <- function (object, parm, summarise = TRUE, ...) {
  
  stopifnot(is.jagr_analysis(object))
  stopifnot(is.character(parm))
  stopifnot(length(parm) >= 1)
  stopifnot(is.logical(summarise))
  stopifnot(is_scalar(summarise))
  
  convergence <- convergence_gsmcmc(as.gsmcmc(object), parm = parm)
  
  if(summarise) {
    independence <- min(convergence$independence)
    convergence <- max(convergence$convergence)
    convergence<-c(convergence,independence)
    names(convergence) <- c("convergence","independence")
  }
  
  return (convergence)
}

#' @title Calculate convergence values
#'
#' @description
#' Calculates convergence (R-hat) values for the parameters in a JAGS analysis
#' 
#' @param object a jags_analysis object
#' @param model_number an integer element specifying the model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param parm a character vector specifying the parameters for which to calculate the convergence
#' @param ... passed to and from other functions
#' @return a data.frame of the parameters with their convergence (R-hat) values
#' @seealso \code{\link{jaggernaut}} and \code{\link{jags_analysis}}
#' @export
convergence <- function (object, model_number = 1, parm = "fixed", ...) {
  if(!is.jags_analysis(object))
    stop ("object should be class jags_analysis")
  
  object <- subset(object, model_number)
  
  parm <- get_parm(object, parm = parm)
  
  con <- convergence_jagr_analysis(as.jagr_analysis(object), parm = parm, summarise = FALSE)
  
  if (!"all" %in% parm || !"deviance" %in% parm) {
    con <- con[rownames(con) != "deviance",]
  }
  
  return (con)
}

check_convergence <- function (object, ...) {
  stopifnot(is.jagr_analysis (object))
  
  parm <- get_parm(object, parm = "all")
  
  convergence <- convergence_jagr_analysis (object, parm = parm)
  
  convergence <- !is.na(convergence) && 
                  convergence[1]<= object$convergence && 
                  convergence[2] > object$independence
  
  return (convergence)
}
