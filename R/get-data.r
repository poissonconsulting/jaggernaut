
# replace with dataset()
get_data <- function (object) {
  if(!is.janalysis (object)) 
    stop ("object must be of class janalysis")

  return (object$analyses[[1]]$data)  
}