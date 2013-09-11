


convergence_jagr_analysis <- function (object, parm = "all", summarise = TRUE, ...) {
  
  stopifnot(is.jagr_analysis(object))
  stopifnot(is.character(parm))
  stopifnot(length(parm) >= 1)
  stopifnot(is.logical(summarise))
  stopifnot(is_scalar(summarise))
  
  convergence <- convergence_jags_mcmc(as.jags_mcmc(object), parm = parm)
  
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
convergence_jags_analysis <- function (object, model_number = 1, parm = "fixed", ...) {
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

