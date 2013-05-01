
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
