
#' Get data from JAGS analysis
#'
#' Returns data from a JAGS analysis
#' 
#' @param object a JAGS analysis
#' @param data.frame of the data used in the JAGS analysis
#' @export
get_data <- function (object) {
  if(!is.janalysis (object)) 
    stop ("object must be of class janalysis")

  return (object$analyses[[1]]$data)  
}