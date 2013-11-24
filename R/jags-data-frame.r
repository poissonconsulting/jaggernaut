
jags_data_frame <- function (data) {
  
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  object <- data
  class(object) <- c("jags_data_frame","data.frame","jags_data_list")
  ntries(object) <- 1
      
  data <- as.list(data)
  
  if(length(data) > 0) {
    
    bol <- sapply(data, inherits, "logical")
    
    for (class in c("integer","numeric","factor","Date","POSIXt"))
      bol <- bol | sapply(data, inherits, class)
    
    if(!all(bol))
      stop("all elements in data must be class integer, numeric, factor, Date or 
         POSIXt")
  }
  
  return (object)
}
