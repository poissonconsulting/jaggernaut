
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
#' @return a vector, matrix or array of geweke values
#' @export
geweke <- function (object, parm = "all", combine = TRUE, ...) {
  UseMethod("geweke", object)
}

geweke.jagr_chains <- function (object, parm = "all", combine = TRUE, ...) {
  
  assert_that(is.string(parm) && noNA(parm))
  assert_that(is.flag(combine) && noNA(combine))
  
  parm <- unique(parm)
  
  mcmc <- as.mcmc.list (object)
  
  vars <- varnames(mcmc)
  
  vars <- sort(vars)
  
  if (is.null(object$geweke)) {
    if(nchains(object) > 1) {
      geweke <- numeric()
      for (i in seq(along = vars)) {
        geweke[i] <- abs(geweke.diag(mcmc[,vars[i]])[[1]][[1]])
      }
    } else {
      geweke <- rep(NA,length(vars))
    }
    geweke <- data.frame(geweke = geweke, row.names = vars)
    geweke$geweke <- geweke
  } else
    geweke <- object$geweke
  
  parm <- expand_parm(object, parm)
  
  geweke <- geweke[row.names(geweke) %in% parm,,drop = FALSE]
  
  if (combine)
    return (max(geweke$geweke, na.rm = TRUE))
  
  return (geweke)
}

geweke.jagr_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  return (geweke(as.jagr_chains(object), parm = parm, combine = combine, ...))
}

geweke_jagr_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.jagr_analysis(object))
  return (geweke(object, parm = parm, combine = combine, ...))
}

#' @method geweke jags_analysis
#' @export 
geweke.jags_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  
  if(!is_character_vector(parm))
    stop("parm must be a character vector with no missing values")
  
  if(!is_logical_scalar(combine))
    stop("combine must be TRUE or FALSE")
  
  if(is_one_model(object))
    return (geweke(analysis(object), 
      parm = parm, 
      combine = combine, ...))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, 
    geweke_jagr_analysis, 
    parm = parm, 
    combine = combine, ...)  
  analyses <- name_object(analyses, "model")
  return (analyses) 
}

#' @method geweke jags_sample
#' @export 
geweke.jags_sample <- function (object, parm = "all", combine = TRUE, ...) {
  
  if(!is_character_vector(parm))
    stop("parm must be a character vector with no missing values")
  
  if(!is_logical_scalar(combine))
    stop("combine must be TRUE or FALSE")
  
  if(any(c("fixed", "random") %in% parm)) {
    warning("fixed or random not defined for jags_sample - replacing with all")
    parm <- "all"
  }
  
  geweke(as.jagr_chains(object), parm = parm, combine = combine, ...)  
}
