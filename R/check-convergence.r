
check_convergence <- function (object) {
  if (!is.jagr_analysis (object))
    stop ("object should be class jagr_analysis")
  
  parm <- parm(object,parm = "all")
  
  convergence <- calc_convergence (object, summarise = T, parm = parm)
  
  convergence <- !is.na(convergence) && convergence[1]<= object$convergence && 
    convergence[2] > object$independence
  return (convergence)
}
