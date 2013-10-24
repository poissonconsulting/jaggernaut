
jags_data_frame <- function (data) {
  
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
    
  object <- data
  class(object) <- c("jags_data_frame","data.frame","jags_data_list")
  
  data <- as.list(data)
  bol <- sapply(data,inherits,"logical")
  
  for (class in c("integer","numeric","factor","Date","POSIXt","matrix","array"))
    bol <- bol | sapply(data,inherits,class)
  
  if(!all(bol))
    stop("elements in data must be class integer, numeric, factor, Date, POSIXt, matrix,array")
  
  ntries(object) <- 1
  
  return (object)
}
