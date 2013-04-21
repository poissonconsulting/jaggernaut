
# replace with dataset()
get_data <- function (object) {
  if(!is.jags_analysis (object)) 
    stop ("object must be of class jags_analysis")

  return (object$analyses[[1]]$data)  
}
