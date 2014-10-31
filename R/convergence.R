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

convergence.jagr_chains <- function (object, parm = "all", combine = TRUE, ...) {
  
  assert_that(is.flag(combine) && noNA(combine))
  
  mcmc <- as.mcmc.list (object)
  
  vars <- varnames(mcmc)
  
  vars <- sort(vars)
  
  if (is.null(object$convergence)) {
    if(nchains(object) > 1) {
      convergence <- numeric()
      for (i in seq(along = vars)) {
        convergence[i] <- round(gelman.diag(mcmc[,vars[i]], transform = TRUE, 
          autoburnin = FALSE)$psrf[1],2)
      }
    } else {
      convergence <- rep(NA,length(vars))
    }
    convergence <- data.frame(convergence = convergence, row.names = vars)
    convergence$convergence <- convergence
  } else
    convergence <- object$convergence
    
  convergence <- convergence[row.names(convergence) %in% parm,,drop = FALSE]
    
  if (combine)
    return (max(convergence$convergence, na.rm = TRUE))
  
  convergence
}

convergence.jagr_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  assert_that(is.flag(combine) && noNA(combine))
  
  parm <- expand_parm(object, parm, drop_suffixed = TRUE)

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
