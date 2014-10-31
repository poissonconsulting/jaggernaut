#' @title Get auto-correlation values (s)
#'
#' @description
#' Get auto-correlation value(s) for JAGS objects
#' 
#' @param object a JAGS object
#' @param parm a character vector indicating the parameters for which to calculate
#' the auto_corr values. Either list the parmeters or use all, fixed or random.
#' @param lags a vector of lags at which to calculate the autocorrelation
#' @param ... passed to and from other functions
#' @return a vector, matrix or array of autocorr values
#' @seealso \code{coda::autocorr}
#' @export
auto_corr <- function (object, parm = "all", lags = c(0, 1, 5, 10, 50), ...) {
  UseMethod("auto_corr", object)
}  
  
auto_corr.jagr_chains <- function (object, parm, lags, ...) {
  
  assert_that(is.character(parm) && noNA(parm) && not_empty(parm))  
  
  autocorr <- list()
  for(i in 1:nchains(object)) {
    x <- as.mcmc(as.mcmc.list(subset(object, chain = i)))
    
    acf <- autocorr(x, lags = lags)
    
    acf <- acf[, , dimnames(acf)[[3]] %in% parm, drop = FALSE]
    
    autocorr[[paste0("chain", i)]] <- acf
  }
  autocorr  
}

auto_corr.jagr_analysis <- function (object, parm, lags, ...) {
  
  parm <- unique(parm)
  
  parm <- expand_parm(object, parm)
  
  auto_corr(as.jagr_chains(object), parm = parm, lags = lags, ...)
}

auto_corr_jagr_analysis <- function (object, parm, lags, ...) {
  stopifnot(is.jagr_analysis(object))
  auto_corr(object, parm = parm, lags = lags, ...)
}

#' @method auto_corr jags_analysis
#' @export 
auto_corr.jags_analysis <- function (object, parm = "all", lags = c(0, 1, 5, 10, 50), ...) {
  
  if(is_one_model(object)) {
    return (auto_corr(analysis(object), 
                     parm = parm,
                     lags = lags,
                     ...))
  }
  lapply(analyses(object), auto_corr_jagr_analysis, parm = parm, lags = lags, ...)  
}

#' @method auto_corr jags_sample
#' @export 
auto_corr.jags_sample <- function (object, parm = "all", lags = c(0, 1, 5, 10, 50), ...) {
  
  assert_that(is.string(parm) && noNA(parm))  
  
  if(any(c("fixed", "random") %in% parm)) {
    warning("fixed or random not defined for jags_sample - replacing with all")
    parm <- "all"
  }
  
  auto_corr(as.jagr_chains(object), parm = parm, lags = lags, ...)  
}
