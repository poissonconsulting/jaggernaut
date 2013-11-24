
jags_data_list <- function (data) {
  
  if (!is.list(data)) {
    stop("data must be a list")
  }
  
  data <- as.list(data)
  
  if(length(data) > 1) {
    if(is.null(names(data)))
      stop("data must be a named list")
    if (any(names(data) == ""))
      stop("all elements must be named")
  }
  
  bol <- sapply(data,inherits,"logical")
  
  for (class in c("integer","numeric","factor","Date","POSIXt","matrix","array"))
    bol <- bol | sapply(data,inherits,class)
  
  if(!all(bol))
    stop("elements in data must be class integer, numeric, factor, Date, POSIXt, matrix,array")
  
  object <- data
  class(object) <- c("jags_data_list")
  ntries(object) <- 1
  
  return (object)
}