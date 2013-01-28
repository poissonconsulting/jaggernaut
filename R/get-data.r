
get_data <- function (object) {
  if(!is.gsanalysis (object)) 
    stop ("object must be of class gsanalysis")

  return (object$data)  
}