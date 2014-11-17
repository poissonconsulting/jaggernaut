as_list_coef <- function (object) {
  
  assert_that(is.data.frame(object))
  assert_that(all(colnames(object) %in% c("estimate", "lower", "upper", 
                                          "sd", "error", "significance")))
  assert_that(length(row.names(object)) > 0)
  
  object <- object[,c("estimate", "lower", "upper", 
                      "sd", "error", "significance"), drop = FALSE]
  
  est <- object
  rm(object)
  
  ss <- strsplit(rownames(est),"\\[|,|]")
  ss <- lapply(ss,function (x) return (x[x != ""]))
  
  pars <- sapply(ss,function (x) return (x[1]))
  ndims <- sapply(ss,function (x) return (length(x)-1))
  
  df <- data.frame(pars,ndims)
  df <- unique(df)
  df <- df[order(df$pars),]
  
  estimates <- list()
  estimates$estimate <- list()
  estimates$lower <- list()
  estimates$upper <- list()
  estimates$sd <- list()
  estimates$error <- list()
  estimates$significance <- list()
  
  for (i in 1:nrow(df)) {
    par <- as.character(df$pars[i])
    ndim <- df$ndims[i]
    if (ndim == 0) {
      estimates$estimate[[par]] <- est[par,"estimate"]
      estimates$lower[[par]] <- est[par,"lower"]
      estimates$upper[[par]] <- est[par,"upper"]
      estimates$sd[[par]] <- est[par,"sd"]
      estimates$error[[par]] <- est[par,"error"]
      estimates$significance[[par]] <- est[par,"significance"]
    } else {
      bol <- substr(rownames(est),1,nchar(par)+1) == paste0(par,"[")
      sss <- lapply(ss[bol],function (x) x[-1])
      estimates$estimate[[par]] <- array(est$estimate[bol], dim = sss[[length(sss)]])
      estimates$lower[[par]] <- array(est$lower[bol], dim = sss[[length(sss)]])
      estimates$upper[[par]] <- array(est$upper[bol], dim = sss[[length(sss)]])
      estimates$sd[[par]] <- array(est$sd[bol], dim = sss[[length(sss)]])
      estimates$error[[par]] <- array(est$error[bol], dim = sss[[length(sss)]])
      estimates$significance[[par]] <- array(est$significance[bol], dim = sss[[length(sss)]])
    }
  }
  estimates
}

#' @importFrom stats sd quantile
coef_matrix <- function(object, level, estimate, as_list) {
  
  stopifnot(is.matrix(object))
  stopifnot(is_numeric_scalar(level))
  stopifnot(is_bounded(level, 0.5, 1.0))
  stopifnot(estimate %in% c("mean","median"))
  
  estimates<-data.frame(t(apply(object,MARGIN=2,FUN = get_estimates, level = level, estimate = estimate)))
  rownames(estimates)<-colnames(object)
  colnames(estimates)<-c("estimate","lower","upper","sd","error","significance")
  
  if (as_list)
    return (as_list_coef(estimates))
  
  estimates
}

coef.jagr_chains <- function (object, parm, level, estimate, 
                              as_list, ...) {
  
  assert_that(is.number(level))
  
  mat <- as.matrix(object)
    
  mat <- mat[,colnames(mat) %in% parm,drop = FALSE]
  
  if (level > 0) {
  return (coef_matrix (mat, level = level, estimate = estimate, 
               as_list = as_list))
  }
  data <- data.frame(row = 1:ncol(mat))
  
  jags_sample(object, parm, data)
}

#' @title Coefficients
#'
#' @description
#' Calculates coefficients for a jags_sample object
#' 
#' @param object a jags_sample object from predict(...,level = "no")
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param estimate a character scalar specifying whether the point estimate should
#' be the "mean" or the "median" or a character scalar which mode the level should be #' taken from. By default the
#' estimate is as currently specified by \code{opts_jagr} in the global options.
#' @param as_list a logical scalar indicating whether the coefs should be in the
#' form of a list (as_list = TRUE) or the default (as_list = FALSE) a data.frame.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame of the parameter estimates with the point estimate and 
#' lower and upper credible limits as well as the standard deviation, percent
#' relative error and significance
#' @seealso \code{\link{opts_jagr}} 
#' and \code{\link{jaggernaut}}
#' @method coef jags_sample
#' @export
coef.jags_sample <- function (object, level = "current", estimate = "current",
                              as_list = FALSE, ...) {
  
  assert_that(is.flag(as_list) && noNA(as_list))
  
  if (!is.numeric(level) && level != "current") {
    old_opts <- opts_jagr(mode = level)
    on.exit(opts_jagr(old_opts))
  }
  
  if (!is.numeric(level)) {
    level <- opts_jagr("level")
  }
  
  if(!estimate %in% c("mean","median") && estimate != "current") {
    old_opts <- opts_jagr(mode = level)
    if(is.null(sys.on.exit()))
      on.exit(opts_jagr(old_opts))
  }
  
  if(!estimate %in% c("mean","median")) {
    estimate <- opts_jagr("estimate")
  }
  
  coef <- coef_matrix(t(samples(object)), level = level, estimate = estimate,
                      as_list = as_list)
  
  if(as_list)
    return (coef)
  
  cbind(dataset(object), coef)
}

coef.jagr_analysis <- function (object, parm, level, estimate, 
                                      as_list, ...) {
  
  parm <- expand_parm(object, parm = parm)
  
  coef(as.jagr_chains(object), parm = parm, 
       level = level, estimate = estimate, 
       as_list = as_list, ...)
}

coef_jagr_analysis <- function (object, parm, level, estimate, 
                                      as_list, ...) {
  stopifnot(is.jagr_analysis(object))
  coef(object, parm = parm, level = level, estimate = estimate,
       as_list = as_list, ...)
}

#' @title Calculate parameter estimates
#'
#' @description
#' Calculates parameter estimates for a JAGS analysis
#' 
#' @param object a \code{jags_analysis} object
#' @param parm a character vector of the parameters to calculate the estimates
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' If level = "no" then returns a jags_sample object.
#' @param estimate a character scalar specifying whether the point estimate should
#' be the "mean" or the "median" or a character scalar which mode the level should be 
#' taken from. By default the
#' estimate is as currently specified by \code{opts_jagr} in the global options.
#' @param as_list a logical scalar indicating whether the coefs should be in the
#' form of a list (as_list = TRUE) or the default (as_list = FALSE) a data.frame.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame of the parameter estimates with the point estimate and 
#' lower and upper credible limits as well as the standard deviation, percent
#' relative error and significance 
#' @seealso \code{\link{jags_analysis}}, \code{\link{opts_jagr}} and \code{\link{jaggernaut}}
#' @method coef jags_analysis
#' @export
coef.jags_analysis <- function (object, parm = "fixed", level = "current", 
                                estimate = "current", 
                                as_list = FALSE, ...) {
  
  if(!is_character_vector(parm))
    stop("parm must be a character vector")
  
  assert_that(is.flag(as_list) && noNA(as_list))
  
  if (!is.numeric(level) && level != "current") {
    if (level == "no") {
      level <- 0
    } else {
      old_opts <- opts_jagr(mode = level)
      on.exit(opts_jagr(old_opts))
    }
  }
  
  if (!is.numeric(level)) {
    level <- opts_jagr("level")
  }
  
  if(!estimate %in% c("mean","median") && estimate != "current") {
    old_opts <- opts_jagr(mode = level)
    if(is.null(sys.on.exit()))
      on.exit(opts_jagr(old_opts))
  }
  
  if(!estimate %in% c("mean","median")) {
    estimate <- opts_jagr("estimate")
  }
  
  if(is_one_model(object)) {
    return (coef(analysis(object), parm = parm, level = level, 
                 estimate = estimate, 
                 as_list = as_list, ...))
  }
  
  lapply(analyses(object), coef_jagr_analysis, 
                     parm = parm, level = level, estimate = estimate,
                     as_list = as_list, ...)
}
