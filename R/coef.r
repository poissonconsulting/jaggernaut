
coef_matrix <- function(object, level, estimate) {
  
  stopifnot(is.matrix(object))
  stopifnot(is_numeric_scalar(level))
  stopifnot(is_bounded(level, 0.5, 1.0))
  stopifnot(estimate %in% c("mean","median"))
  
  est <- function (x, level) {
    
    p<-function (x) {
      x<-sum(as.integer(x>=0))/length(x)
      x<-round(x,4)
      return (min(x,1-x)*2)
    }
    
    lower <- (1 - level) / 2
    upper <- level + lower
    ci <- quantile(x,c(lower,upper),na.rm=T)
    names (ci) <- c("lower","upper") 
    
    if (estimate == "mean") {
      estimate <- mean(x, na.rm = T)
    } else {
      estimate <- median(x, na.rm = T)      
    }
    
    pre <- (ci["upper"]-ci["lower"]) / 2 / abs(estimate)
    pre <- pre * 100
    
    return (c(estimate ,ci, signif(sd(x),5), round(pre), p(x)))
  }
  
  estimates<-data.frame(t(apply(object,MARGIN=2,FUN = est, level = level)))
  rownames(estimates)<-colnames(object)
  colnames(estimates)<-c("estimate","lower","upper","sd","error","significance")
  return (estimates)
}

coef.jagr_chains <- function (object, parm, level, estimate, ...) {
  stopifnot(is_numeric_scalar(level))
  stopifnot(is_bounded(level, 0.5, 1.0))
  
  mat <- as.matrix(object)

  parm <- expand_parm(object, parm = parm)
  
  mat <- mat[,colnames(mat) %in% parm,drop = FALSE]
    
  return (coef_matrix (mat, level = level, estimate = estimate))
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
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame of the parameter estimates with the point estimate and 
#' lower and upper credible limits as well as the standard deviation, percent
#' relative error and significance
#' @seealso \code{\link{opts_jagr}} 
#' and \code{\link{jaggernaut}}
#' @method coef jags_sample
#' @export
coef.jags_sample <- function (object, level = "current", estimate = "current", ...) {

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
    
  coef <- coef_matrix(t(samples(object)), level = level, estimate = estimate)
  
  coef <- cbind(data_jags(object), coef)
  
  return (coef)
}

coef.jagr_power_analysis <- function (object, parm, level, estimate, ...) {
  return (coef(as.jagr_chains(object), parm = parm, 
               level = level, estimate = estimate, ...))
}

coef_jagr_power_analysis <- function (object, parm, level, estimate, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (coef(object, parm = parm, level = level, estimate = estimate, ...))
}

coef_jagr_analysis <- function (object, parm, level, estimate, ...) {
  stopifnot(is.jagr_analysis(object))
  return (coef(object, parm = parm, level = level, estimate = estimate, ...))
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
#' @param estimate a character scalar specifying whether the point estimate should
#' be the "mean" or the "median" or a character scalar which mode the level should be #' taken from. By default the
#' estimate is as currently specified by \code{opts_jagr} in the global options.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame of the parameter estimates with the point estimate and 
#' lower and upper credible limits as well as the standard deviation, percent
#' relative error and significance 
#' @seealso \code{\link{jags_analysis}}, \code{\link{opts_jagr}} and \code{\link{jaggernaut}}
#' @method coef jags_analysis
#' @export
coef.jags_analysis <- function (object, parm = "fixed", level = "current", 
                                estimate = "current", ...) {
  if(!is_character_vector(parm))
    stop("parm must be a character vector")
  
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
    
  if(is_one_model(object))
    return (coef(analysis(object), parm = parm, level = level, estimate = estimate, ...))
  
  analyses <- analyses(object)
  analyses <- lapply(analyses, coef_jagr_analysis, 
                     parm = parm, level = level, estimate = estimate, ...)
  analyses <- name_object(analyses, "Model")
  return (analyses) 
}

#' @method coef jags_power_analysis
#' @export
coef.jags_power_analysis <- function (object, parm = "fixed", combine = TRUE, converged = TRUE, level = "current", power_level = "current", estimate = "current", ...) {
  
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

  if (!estimate %in% c("mean","median")) {
    estimate <- opts_jagr("estimate")
  }

  if (!is.numeric(power_level) && power_level != "current") {
    old_opts <- opts_jagr(mode = level)
    if(is.null(sys.on.exit()))
      on.exit(opts_jagr(old_opts))
  }

  if (!is.numeric(power_level)) {
    power_level <- opts_jagr("power_level")
  }
    
  rhat_threshold <- rhat_threshold(object)
  
  coef_jagr_power_analysis_converged <- function (object, parm, level, 
                                                  estimate, rhat_threshold,
                                                  converged, ...) {
    stopifnot(is.jagr_power_analysis(object))
    
    coef <- coef(object, parm = parm, level = level, estimate = estimate, ...)
        
    attr(coef,"converged") <- !converged ||
      is_converged(object, rhat_threshold = rhat_threshold)
      
    return (coef)
  }
      
  analyses <- analyses(object)
  
  coef <- llply_jg(analyses, coef_jagr_power_analysis_converged, parm = parm, 
                   level = level, estimate = estimate, 
                   rhat_threshold = rhat_threshold, 
                   converged = converged, ..., .recursive = 2)
  
  coef <- name_object(coef,c("value","replicate"))
  
  if(!combine)
    return (coef)
  
  melt_coef <- function (object) {
    
    converged <- attr(object,"converged")
        
    object <- subset(object,select = c("estimate","lower","upper","error"))
    object$parameter <- rownames(object) 
    melt <- reshape2::melt(object, id.vars = c("parameter"), variable.name = "statistic", value.name = "number")
    
    if(!converged)
      is.na(melt$number) <- TRUE
    
    return (melt)
  }
  
  coef <- ldply_jg(coef, melt_coef, .recursive = 2)
  
  coef$replicate <- paste0("replicate",as.integer(substr(coef$.id,10,15)))
  coef$value <- paste0("value",rep(1:nvalues(object), each = nrow(coef)/nvalues(object)))
  coef$.id <- NULL
  
  value <- parameter <- statistic <- replicate <- NULL
  
  coef <- reshape2::dcast(coef,value + parameter + statistic ~ replicate,
                          value.var = "number")
  
  get_estimates <- function (d, power_level, level, estimate, converged) {
        
    est <- d[d$statistic == "estimate",substr(colnames(d),1,9) == "replicate",drop=TRUE]
    
    niters <- length(est)
    conv <- round(length(est[!is.na(est)]) / length(est),2)
    if (!converged)
      is.na(conv) <- TRUE
    samples <- length(est[!is.na(est)])
    
    if (estimate == "median") {  
      est <- median(unlist(d[d$statistic == "estimate",
                         substr(colnames(d),1,9) == "replicate",drop = TRUE]),
                       na.rm = TRUE)
    } else if (estimate == "mean"){
      est <- mean(unlist(d[d$statistic == "estimate",
                                  substr(colnames(d),1,9) == "replicate",drop = TRUE]),
                         na.rm = TRUE)     
    } else
      stop()
    
    lower <- quantile(unlist(d[d$statistic == "lower",
                         substr(colnames(d),1,9) == "replicate",drop = TRUE]),
                      probs = (1 - power_level) / 2, na.rm = TRUE)   
    
    upper <- quantile(unlist(d[d$statistic == "upper",
                        substr(colnames(d),1,9) == "replicate",drop = TRUE]),
                      probs = power_level + ((1 - power_level) / 2), na.rm = TRUE)
    
    error <- (unlist(d[d$statistic == "upper",
                      substr(colnames(d),1,9) == "replicate",drop = TRUE]) -
      unlist(d[d$statistic == "lower",
               substr(colnames(d),1,9) == "replicate",drop = TRUE])) / 2 /
     abs(unlist(d[d$statistic == "estimate",
               substr(colnames(d),1,9) == "replicate",drop = TRUE]))
    
    error <- error * 100

    error <- quantile(error,
                      probs = c((1 - power_level) / 2, 0.5, 
                                power_level + ((1 - power_level) / 2)), na.rm = TRUE)
        
    error.lower <- error[1]
    error.upper <- error[3]
    error <- error[2]
    
    p <- t(d[d$statistic %in% c("lower","upper"),
                        substr(colnames(d),1,9) == "replicate"])
    
    p <- na.omit(p)
            
    p <- (p[,1,drop=TRUE] > 0 & p[,2,drop=TRUE] > 0) | (p[,1,drop=TRUE] < 0 & p[,2,drop=TRUE] < 0)
        
    significance <-  length(p[!p]) / length(p)
        
    significance <- round(significance, 4)
            
    data <- data.frame(niters = niters, converged = conv, samples = samples, 
                       estimate = est, lower = lower, upper = upper, 
                       error = error, error.lower = error.lower, 
                       error.upper = error.upper,
                       significance = significance)

    return (data)
  }
  
  coef <- ddply_jg(coef, plyr::.(value,parameter), get_estimates, power_level = power_level, level = level, estimate = estimate, converged = converged)
  
  values <- values(object)
  values <- cbind(data.frame(value = row.names(values)),values)
  
  coef <- merge(values, coef)
  
  return (coef)
}
