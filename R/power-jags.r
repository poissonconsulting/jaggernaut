                                         
#' @export
power_jags <- function (object, parm = c(fixed = 0), level = "current") {
  
  if(!is.jags_power_analysis(object))
    stop("object must be a jags_power_analysis")
  
  if(!is.numeric(parm) || !(is.character(parm) && is.character(names(parm))))
    stop("parm must be a character vector or a named numeric vector")
      
  if(is.character(parm)) {
    names <- parm
    parm <- rep(0, length(names))
    names(parm) <- names
  }
    
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  }
  
  if(!is.null(power(object))) {
    stopifnot(is.numeric(level(object)))
    if(level == level(object)) {
      
      
      return (power)
    }
  }
    && !is.null(leve)level == level(object)) {
    power <- power(object)
    # need to check that parm in power
    if (level == level(object))
      return (power(object))
  }
  
  quiet <- opts_jagr("quiet")
      
  if (!quiet)
    cat("\ncalculating coefficients\n")
  
  coef <- coef(object, parm = names(parm), level = level)
  
  return (power)
}
