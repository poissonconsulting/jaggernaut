
coef_matrix <- function(object, level) {
  
  stopifnot(is.matrix(object))
  stopifnot(is.numeric(level))
  stopifnot(is_scalar(level))
  stopifnot(is_defined(level))
  stopifnot(level > 0.5 & level < 1.0)
  
  est <- function (x, level) {
    
    p<-function (x) {
      x<-sum(as.integer(x>=0))/length(x)
      x<-round(x,4)
      return (min(x,1-x)*2)
    }
    
    lower <- (1 - level) / 2
    upper <- level + lower
    est <- quantile(x,c(0.5,lower,upper),na.rm=T)
    names (est) <- c("estimate","lower","upper") 
    
    pre <-round((est["upper"]-est["lower"]) / 2 / est["estimate"] * 100)
    pre <- abs(round(pre, 0))
    
    return (c(est, signif(sd(x),3), pre, p(x)))
  }
  
  estimates<-data.frame(t(apply(object,MARGIN=2,FUN = est, level = level)))
  rownames(estimates)<-colnames(object)
  colnames(estimates)<-c("estimate","lower","upper","sd","error","significance")
  return (estimates)
}

coef.jagr_chains <- function (object, parm = "all", level = 0.95, ...) {
  stopifnot(is.numeric(level))
  stopifnot(is_scalar(level))
  stopifnot(is_defined(level))
  stopifnot(level > 0.5 & level < 1.0)
  
  coef <- coef_matrix (as.matrix(object, parm), level = level)
  
  coef <- coef[rownames(coef) != "deviance",]
    
  return (coef)
}

coef.jagr_power_analysis <- function (object, parm = "all", level = 0.95, ...) {
  return (coef(as.jagr_chains(object), parm = parm, level = level, ...))
}

coef.jagr_analysis <- function (object, parm = "all", level = 0.95, ...) {

  parm <- expand_parm(object, parm = parm)
  
  return (coef(as.jagr_chains(object), parm = parm, level = level, ...))
}

coef_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (coef(object, ...))
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
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame of the parameter estimates with the median estimate and 
#' lower and upper credible limits as well as the percent relative error 
#' and significance 
#' @seealso \code{\link{jags_analysis}} and \code{\link{jaggernaut}}
#' @method coef jags_analysis
#' @export
coef.jags_analysis <- function (object, parm = "fixed", level = "current", ...) {
  if (!is.character(parm)) 
    stop ("parm must be character vector")
  if(!is_length(parm))
    stop("parm must be at least length one")
  if(!is_defined(parm))
    stop("parm must not contain missing values")
  
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  }
  
  if(is_one_model(object))
    return (coef(analysis(object), parm = parm, level = level, ...))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, coef_jagr_analysis, 
                     parm = parm, level = level, ...)
  analyses <- name_object(analyses, "Model")
  return (analyses) 
}

#' @method coef jags_power_analysis
#' @export
coef.jags_power_analysis <- function (object, parm = "fixed", level = "current", ...) {
  
  lapply_coef_jagr_power_analysis <- function (object, model,
                                               parm, ...) {    
    return (lapply(object, coef_jagr_power_analysis, 
                   model = model, parm = parm, ...))
  }
  
  analyses <- analyses(object)
  
  coef <- lapply(analyses, lapply_coef_jagr_power_analysis, 
                 model = model(model(object)), parm = parm, ...)
  
  coef <- name_object(coef,c("value","replicate"))
  return (coef)
}

#' @title Calculate estimates
#'
#' @description
#' Calculates estimates for a jags_sample object
#' 
#' @param object a data.frame or list of data.frames from predict(...,level = "no")
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @return a data frame with the median estimates and credibility intervals for
#' the derived parameter of interest
#' @seealso \code{\link{predict.jags_analysis}}
calc_estimates_jags_sample <- function (object, level = "current") {
  if (!inherits(object,"jags_sample"))
    stop("object must be class jags_sample")
  
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  } else {
    if (level < 0.75 || level > 0.99) {
      stop("level must lie between 0.75 and 0.99")
    }
  } 
  
  mat <- as.matrix(object[,grep("V[[:digit:]]", colnames(object))])
  est <- calc_estimates (t(mat), level = opts_jagr("level"))
  data <- object[,-grep("V[[:digit:]]", colnames(object)), drop=FALSE]
  est <- cbind(data, est)
  return (est)
}

