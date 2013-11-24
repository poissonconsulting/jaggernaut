
jagr_data_frame <- function (data) {
  
  if (!is.data.frame(data))
    stop("data must be a data.frame")
  
  object <- data
  class(object) <- c("jagr_data_frame", "data.frame", "jagr_data_list")
  ntries(object) <- 1
  
  jagr_data_list(data)
  
  return (object)
}
