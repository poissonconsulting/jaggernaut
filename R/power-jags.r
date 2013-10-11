                                         
#' @export
power_jags <- function (object, parm = list("fixed" = c("p < 0.05")),  power_level = 0.8, coef_level = "current") {
  
  coefs_level <- coef_level
  rm(coef_level)

  powers_level <- power_level
  rm(power_level)
  
  if(!is.jags_power_analysis(object))
    stop("object must be a jags_power_analysis")
  
  if(!(is.list(parm) && is_named(parm)))
    stop("parm must be a named list")
     
  if(!all(sapply(parm, is_character)) || !all(sapply(parm, is_scalar)))
    stop("all the elements in parm must be character scalars")
     
  stopifnot(powers_level >= 0.5 && powers_level <= 0.99)
  
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(coefs_level)) {
    opts_jagr(mode = coefs_level)
    coefs_level <- opts_jagr("level")
  }
     
  quiet <- opts_jagr("quiet")
            
  if(!is.null(powers(object)) && powers_level == powers_level(object)) {
    powers <- powers(object)
  } else {
    if(!is.null(coefs(object)) && coefs_level == coefs_level(object)) {
      coefs <- object(coefs)
    } else {
      if (!quiet)
        cat("\ncalculating coefficients\n")
      
      coef <- coef(object, parm = "all", level = coefs_level)

      values <- values(object)
      values$value <- rownames(values)
      
      values <- subset(values, select = c("value",colnames(values(object))))

      statistic <- data.frame(statistic = colnames(coef[[1]][[1]]))
      parameter <- data.frame(parameter = rownames(coef[[1]][[1]]))
      
      coefs <- merge(values, statistic)
            
      coefs <- coefs[order(coefs$value),]
      
      coefs <- merge(coefs, parameter)
            
      melt_coef <- function (coef) {
        coef$parameter <- rownames(coef) 
        coef <- reshape2::melt(coef, id.vars = c("parameter"), variable.name = "statistic", value.name = "number")
        return (coef)
      }
      
      lapply_melt_coef <- function (coef) {   
        return (lapply(coef,melt_coef))
      }
    
      coef <- coef(power)
      coef <- lapply(coef, lapply_melt_coef) 
      print(coef)
      stop()
      
      
      
      coefs <- coef ## need to generate coefs from coef
      coefs_level(object) <- coefs_level
      coefs(object) <- coefs
    }

    powers <- coefs # need to generate power from coefs
    
    powers_level(object) <- powers_level
    powers(object) <- powers    
  }       

  ## nned to use parm to query out what need from powers  
  
  return (powers)
}
