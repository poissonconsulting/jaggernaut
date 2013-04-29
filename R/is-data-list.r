
is_data_list <- function (data) {
  if (!is.list(data)) {
    return (FALSE)
  }
  if(is.data.frame(data)) {
    return (FALSE)
  }
    
  names <- names(data)
  if(is.null(names)) {
    return (FALSE)
  }
  if (any(names == "")) {
    return (FALSE)
  }
  
  bol <- sapply(data,inherits,"logical")
  
  for (class in c("integer","numeric","factor","Date","POSIXt","matrix","array")) {
    bol <- bol | sapply(data,inherits,class)
  }
  return (all(bol))
}
