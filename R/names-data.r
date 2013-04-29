
names_data <- function (data) {
  
  if (!(is.data.frame(data) || is_data_list(data)))
    stop("data must be a data.frame or data list")
  
  if(is.data.frame(data)) {
    return (colnames(data))
  }
  return (names(data))
}
