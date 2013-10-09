                                         
#' @export
power_jags <- function (object, parm = c(fixed = 0), level = "current") {
  
  if(!is.jags_power_analysis(object))
    stop("object must be a jags_power_analysis")
  
  if(!(is.character(parm) || (is.numeric(parm) && is.character(names(parm)))))
    stop("parm must be a character vector or a named numeric vector")
    
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  }
  
  if(is.character(parm)) {
    names <- parm
    parm <- rep(0, length(names))
    names(parm) <- names
  }
  print(parm)
  
  parm <- "all"
  parm <- expand_parm(object, parm = parm)
  
  print(parm)
  stop()
  if(!is.null(power(object))) {
    stopifnot(is.numeric(level(object)))
    if(level == level(object)) {
      
      power <- power(object)
      return (power)
    }
  }
  
  quiet <- opts_jagr("quiet")
      
  if (!quiet)
    cat("\ncalculating coefficients\n")
  
  coef <- coef(object, parm = names(parm), level = level)
  
  power <- coef
  
  return (power)
}
