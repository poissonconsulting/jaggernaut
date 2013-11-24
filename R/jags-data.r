
jags_data <- function (x) {
  
  if(is.data.frame(x))
    return (jags_data_frame(x))
  
  return (jags_data_list(x))
}