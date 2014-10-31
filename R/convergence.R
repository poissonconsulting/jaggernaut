#' @title Get R-hat value(s)
#'
#' @description
#' Get R-hat value(s) for JAGS objects
#' 
#' @param object a JAGS object
#' @param parm a character vector indicating the parameters for which to calculate
#' the R-hat values. Either list the parmeters or use all, fixed or random.
#' @param combine a logical element indicating whether or not to summarise by 
#' the maximum R-hat value.
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of convergence values
#' @export
convergence <- function (object, parm = "all", combine = TRUE, ...) {
  UseMethod("convergence", object)
}

convergence.mcmc.list <- function (object, vars) {
  convergence <- round(gelman.diag(object[,vars], transform = TRUE, autoburnin = FALSE, multivariate = FALSE)$psrf[,"Point est.", drop = FALSE], 2)
  rownames(convergence) <- vars
  colnames(convergence) <- "convergence"
  as.data.frame(convergence)
}

convergence.jagr_chains <- function (object, parm = "all", combine = TRUE, ...) {
  
  assert_that(is.flag(combine) && noNA(combine))
  
  if (is.null(object$convergence)) {
    
    mcmc <- as.mcmc.list (object)
    
    vars <- sort(varnames(mcmc))
    nvars <- length(vars)
    
    nworkers <- ifelse(opts_jagr("parallel"), getDoParWorkers(), 1)
    
    if(getDoParWorkers() == 1) {
      object$convergence <- convergence(mcmc, vars = vars)
    } else {
      v <- NULL
      object$convergence <- foreach(v = isplitVector(vars, chunks = nworkers),
                                    .combine = rbind) %dopar% {
                                      convergence(mcmc, vars = v)
                                    } 
    }
  }
  convergence <- object$convergence
  
  convergence <- convergence[row.names(convergence) %in% parm,,drop = FALSE]
  
  if (combine)
    return (max(convergence$convergence, na.rm = TRUE))
  
  convergence
}

convergence.jagr_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  assert_that(is.flag(combine) && noNA(combine))
  
  parm <- expand_parm(object, parm)
  
  convergence(as.jagr_chains(object), parm = parm, combine = combine, ...)
}

convergence_jagr_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.jagr_analysis(object))
  convergence(object, parm = parm, combine = combine, ...)
}

#' @method convergence jags_analysis
#' @export 
convergence.jags_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  
  if(!is_character_vector(parm))
    stop("parm must be a character vector with no missing values")
  
  if(!is_logical_scalar(combine))
    stop("combine must be TRUE or FALSE")
  
  if(is_one_model(object))
    return (convergence(analysis(object), parm = parm, combine = combine, ...))
  
  lapply(analyses(object), convergence_jagr_analysis, parm = parm, combine = combine, ...)  
}

#' @method convergence jags_sample
#' @export 
convergence.jags_sample <- function (object, parm = "all", combine = TRUE, ...) {
  
  if(!is_character_vector(parm))
    stop("parm must be a character vector with no missing values")
  
  if(!is_logical_scalar(combine))
    stop("combine must be TRUE or FALSE")
  
  if(any(c("fixed", "random") %in% parm)) {
    warning("fixed or random not defined for jags_sample - replacing with all")
    parm <- "all"
  }
  
  convergence(as.jagr_chains(object), parm = parm, combine = combine, ...)  
}
