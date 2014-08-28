
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
#' @return a vector, matrix or array of rhat values
#' @export
rhat <- function (object, parm = "all", combine = TRUE, ...) {
  UseMethod("rhat", object)
}

rhat.jagr_chains <- function (object, parm = "all", combine = TRUE, ...) {
  
  assert_that(is.string(parm) && noNA(parm))
  assert_that(is.flag(combine) && noNA(combine))
  
  parm <- unique(parm)
  
  mcmc <- as.mcmc.list (object)
  
  vars <- varnames(mcmc)
  
  vars <- sort(vars)
  
  if (is.null(object$rhat)) {
    if(nchains(object) > 1) {
      rhat <- numeric()
      for (i in seq(along = vars)) {
        rhat[i] <- round(gelman.diag(mcmc[,vars[i]], transform = TRUE, 
          autoburnin = FALSE)$psrf[1],2)
      }
    } else {
      rhat <- rep(NA,length(vars))
    }
    rhat <- data.frame(rhat = rhat, row.names = vars)
    rhat$rhat <- rhat
  } else
    rhat <- object$rhat
  
  parm <- expand_parm(object, parm)
  
  rhat <- rhat[row.names(rhat) %in% parm,,drop = FALSE]
    
  if (combine)
    return (max(rhat$rhat, na.rm = TRUE))
  
  return (rhat)
}

rhat.jagr_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  return (rhat(as.jagr_chains(object), parm = parm, combine = combine, ...))
}

rhat_jagr_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (rhat(object, parm = parm, combine = combine, ...))
}

rhat_jagr_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  stopifnot(is.jagr_analysis(object))
  return (rhat(object, parm = parm, combine = combine, ...))
}

#' @method rhat jags_analysis
#' @export 
rhat.jags_analysis <- function (object, parm = "all", combine = TRUE, ...) {
  
  if(!is_character_vector(parm))
    stop("parm must be a character vector with no missing values")
  
  if(!is_logical_scalar(combine))
    stop("combine must be TRUE or FALSE")
  
  if(is_one_model(object))
    return (rhat(analysis(object), 
                 parm = parm, 
                 combine = combine, ...))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, 
                     rhat_jagr_analysis, 
                     parm = parm, 
                     combine = combine, ...)  
  analyses <- name_object(analyses, "model")
  return (analyses) 
}

#' @method rhat jags_analysis
#' @export 
rhat.jags_sample <- function (object, parm = "all", combine = TRUE, ...) {
  
  if(!is_character_vector(parm))
    stop("parm must be a character vector with no missing values")
  
  if(!is_logical_scalar(combine))
    stop("combine must be TRUE or FALSE")
  
  if(any(c("fixed", "random") %in% parm)) {
    warning("fixed or random not defined for jags_sample - replacing with all")
    parm <- "all"
  }
    
  rhat(as.jagr_chains(object), parm = parm, combine = combine, ...)  
}

#' @method rhat jags_power_analysis
#' @export 
rhat.jags_power_analysis <- function (object, parm = "all", combine = TRUE, ...) {

  lapply_rhat_jagr_power_analysis <- function (object, parm, combine) {    
    return (lapply(object, rhat_jagr_power_analysis, 
                   parm = parm, 
                   combine = combine, ...))
  }
    
  analyses <- analyses(object)
    
  rhat <- lapply(analyses, lapply_rhat_jagr_power_analysis, 
                 parm = parm, 
                 combine = combine, ...)

  if(combine) {
    rhat <- matrixise(rhat)
    rhat <- name_object(t(rhat),c("replicate","value"))
  } else
    rhat <- name_object(rhat,c("value","replicate"))
  return (rhat)
}
