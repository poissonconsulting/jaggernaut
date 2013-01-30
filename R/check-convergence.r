
check_convergence <- function (object, type = "all") {
  if (!is.jagr_analysis (object))
    stop ("object should be class jagr_analysis")
  
  convergence <- calc_convergence (object, summarise = T, type = type)
  
  convergence <- !is.na(convergence) && convergence[1]<= object$convergence && 
    convergence[2] > object$independence
  return (convergence)
}
