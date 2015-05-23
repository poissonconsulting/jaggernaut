jags_sample_coef <- function (chains, parm, data) {
  
  stopifnot(is.jagr_chains(chains))
  stopifnot(is.character(parm))
  stopifnot(datalist::is_convertible_data(data))
    
  samples <- t(as.matrix(chains))
  samples <- samples[match(parm, rownames(samples)), , drop = FALSE]

  stopifnot(nrow(samples) >= 1)
  
  if(datalist::is_convertible_data_list(data)) {
    bol <- sapply(data, function (x, n) is.vector(x) && length(x) == n,
                  n = nrow(samples))
    if(any(bol)) {
      data <- data.frame(data[bol])
    } else
      data <- data.frame(row = 1:nrow(samples))
  }
  
  if(nrow(samples) == 1 && nrow(data) > 1) {
    data <- unique(delete_variable_variables(data))
    
    if(nrow(data) == 0)
      data <- data.frame(Parameter = parm)
  }
  
  stopifnot(nrow(samples) == nrow(data))
  
  object <- cbind(data, samples)

  class(object) <- c("data.frame", "jags_sample")
  
  return (object)
}

jags_sample <- function (chains, parm, data) {
  
  stopifnot(is.jagr_chains(chains))
  stopifnot(is.string(parm))
  stopifnot(parm %in% names(chains$samples))  
  stopifnot(datalist::is_convertible_data(data))
    
  samples <- t(as.matrix(chains))
  
  samples <- samples[substr(rownames(samples), 1, nchar(parm)) == parm, , drop = FALSE]
  
  stopifnot(nrow(samples) >= 1)
  
  if(datalist::is_convertible_data_list(data)) {
    bol <- sapply(data, function (x, n) is.vector(x) && length(x) == n,
                  n = nrow(samples))
    if(any(bol)) {
      data <- data.frame(data[bol])
    } else
      data <- data.frame(row = 1:nrow(samples))
  }
  
  if(nrow(samples) == 1 && nrow(data) > 1) {
    data <- unique(delete_variable_variables(data))
    
    if(nrow(data) == 0)
      data <- data.frame(Parameter = parm)
  }
  
  stopifnot(nrow(samples) == nrow(data))
  
  object <- cbind(data, samples)

  class(object) <- c("data.frame", "jags_sample")
  
  return (object)
}
