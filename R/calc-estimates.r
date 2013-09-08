
calc_estimates<- function (object, ...) {
  UseMethod("calc_estimates", object)
}

calc_estimates.matrix <- function(object, level) {
  
  stopifnot(is.matrix(object))
  stopifnot(is.numeric(level))
  stopifnot(length(level) == 1)
  stopifnot(level >= 0.75)
  stopifnot(level <= 0.99)
    
  est <- function (x, level) {
    lower <- (1 - level) / 2
    upper <- level + lower
    return (quantile(x,c(0.5,lower,upper),na.rm=T))
  }
  p<-function (x) {
    x<-sum(as.integer(x>=0))/length(x)
    x<-round(x,4)
    return (min(x,1-x)*2)
  }
  fun<-function (x, level) {

    est <- est(x, level)
    
    pre <-round((est[3]-est[1]) / 2 / est[2] * 100)
    pre <- abs(round(pre, 0))
    
    return (c(est, pre, p(x)))
  }
    
  estimates<-data.frame(t(apply(object,MARGIN=2,FUN=fun, level = level)))
  rownames(estimates)<-colnames(object)
  colnames(estimates)<-c('estimate','lower','upper','error','significance')
  return (estimates)
}

calc_estimates.jags_mcmc <- function (object, parm, level) {

  stopifnot(is.character(parm))
  stopifnot(is.numeric(level))
  stopifnot(length(level) == 1)
  stopifnot(level >= 0.75)
  stopifnot(level <= 0.99)
  
  return (calc_estimates (get_sims (object, parm), level = level))
}

calc_estimates.jagr_analysis <- function (object, parm, level) {
  
  stopifnot(is.jagr_analysis(object))
  stopifnot(is.character(parm))
  stopifnot(is.character(level) || is.numeric(level))
  stopifnot(length(level) == 1)

  return (calc_estimates (object$mcmc, parm = parm, level = level))
}

calc_estimates.jagr_simulation <- function (object) {
  
  stopifnot(is.jagr_simulation(object))
  
  return (calc_estimates (get_sims (object$mcmc, parm = NULL), level = 0.95))
}

calc_estimates.jags_analysis <- function (object, parm, level) {

  stopifnot(is.jags_analysis(object))
  stopifnot(nmodel(object) == 1)
  stopifnot(is.character(parm))
  stopifnot(is.numeric(level))
  stopifnot(length(level) == 1)
  stopifnot(level >= 0.75)
  stopifnot(level <= 0.99)
    
  return (calc_estimates(as.jagr_analysis(object), parm = parm, level = level))
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
#' @method calc_estimates jags_sample
#' @export 
calc_estimates.jags_sample <- function (object, level = "current") {
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
