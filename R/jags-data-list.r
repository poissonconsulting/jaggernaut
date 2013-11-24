
jags_data_list <- function (data) {
  
  if (!is.list(data))
    stop("data must be a list")
  
  data <- as.list(data)
  
  if (length(data) == 0)
    stop("data must be a list with at least one element")
  
  if (is.null(names(data)) || any(names(data) == "" | duplicated(names(data))))
    stop("data must be a list of uniquely named elements")
    
  if (any(is.null(data)))
    stop("data must not contain NULL elements")
  
  bol <- sapply(data, inherits, "logical")
  
  for (class in c("integer","numeric","factor","Date","POSIXt","matrix","array"))
    bol <- bol | sapply(data, inherits, class)
  
  if(!all(bol))
    stop("elements in data must be class integer, numeric, factor, Date, 
         POSIXt, matrix or array")
  
  object <- data
  class(object) <- c("jags_data_list")
  ntries(object) <- 1
  
  return (object)
}
