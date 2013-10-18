
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

coef.jagr_chains <- function (object, parm, level, ...) {
  stopifnot(is.numeric(level))
  stopifnot(is_scalar(level))
  stopifnot(is_defined(level))
  stopifnot(level > 0.5 & level < 1.0)
  
  mat <- as.matrix(object)

  parm <- expand_parm(object, parm = parm)
  
  mat <- mat[,colnames(mat) %in% parm,drop = FALSE]
    
  return (coef_matrix (mat, level = level))
}

coef.jagr_power_analysis <- function (object, parm, level, ...) {
  return (coef(as.jagr_chains(object), parm = parm, level = level, ...))
}

coef_jagr_power_analysis <- function (object, parm, level, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (coef(object, parm, level, ...))
}

coef_jagr_analysis <- function (object, parm, level, ...) {
  stopifnot(is.jagr_analysis(object))
  return (coef(object, parm, level, ...))
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
coef.jags_power_analysis <- function (object, parm = "fixed", combine = TRUE, level = "current", ...) {
  
  lapply_coef_jagr_power_analysis <- function (object,
                                               parm, level, ...) {    
    return (lapply(object, coef_jagr_power_analysis, 
                   parm = parm, level = level, ...))
  }
  
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  }
  
  power_level <- opts_jagr("power_level")
    
  analyses <- analyses(object)
  
  coef <- lapply(analyses, lapply_coef_jagr_power_analysis, parm = parm, level = level, ...)
  
  coef <- name_object(coef,c("value","replicate"))
  
  if(!combine)
    return (coef)
  
  melt_coef <- function (object) {
        
    object <- subset(object,select = c("estimate","lower","upper"))
    object$parameter <- rownames(object) 
    object <- reshape2::melt(object, id.vars = c("parameter"), variable.name = "statistic", value.name = "number")
    
    return (object)
  }
  
  ldply_analyses <- function (x, parm) {
    return (plyr::ldply(x, melt_coef))
  }
  
  coef <- plyr::ldply(coef, ldply_analyses)
  
  coef$replicate <- paste0("replicate",as.integer(substr(coef$.id,10,15)))
  coef$value <- paste0("value",rep(1:nvalues(object), each = nrow(coef)/nvalues(object)))
  coef$.id <- NULL
  
  coef <- reshape2::dcast(coef,value + parameter + statistic ~ replicate,
                          value.var = "number")
  
  get_estimates <- function (d, power_level) {
  
    estimate <- median(unlist(d[d$statistic == "estimate",
                         substr(colnames(d),1,9) == "replicate",drop = TRUE]))
    
    lower <- quantile(unlist(d[d$statistic == "lower",
                         substr(colnames(d),1,9) == "replicate",drop = TRUE]),
                      probs = (1 - power_level) / 2)   
    
    upper <- quantile(unlist(d[d$statistic == "upper",
                        substr(colnames(d),1,9) == "replicate",drop = TRUE]),
                      probs = power_level + ((1 - power_level) / 2))
    
    p <- t(d[d$statistic %in% c("lower","upper"),
                        substr(colnames(d),1,9) == "replicate"])
            
    p <- (p[,1,drop=TRUE] > 0 & p[,2,drop=TRUE] > 0) | (p[,1,drop=TRUE] < 0 & p[,2,drop=TRUE] < 0)
        
    significance <-  length(p[!p]) / length(p)
        
    significance <- round(significance, 4)
            
    return (data.frame(estimate = estimate, lower = lower, upper = upper,
                       significance = significance))
  }
  
  coef <- plyr::ddply(coef, plyr::.(value,parameter), get_estimates, power_level = power_level)
  
  values <- values(object)
  values <- cbind(data.frame(value = row.names(values)),values)
  
  coef <- merge(values, coef)
  
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

