#' @title JAGS sample
#'
#' @description 
#' An object of class \code{jags_sample}.
#' 
#' @param chains \code{jagr_chains} object.
#' @param parm a character scalar.
#' @param data a data.frame.
#' @return A jags_sample object.
#' @seealso \code{\link{predict.jags_analysis}} and \code{\link{jaggernaut}}.
jags_sample <- function (chains, parm, data) {
  
  stopifnot(is.jagr_chains(chains))
  stopifnot(is_character_scalar(parm))
  stopifnot(parm %in% names(chains$samples))  
  stopifnot(is_convertible_data(data))
  
  parm <- expand_parm(chains, parm = parm)
  
  samples <- t(as.matrix(chains))
  samples <- samples[rownames(samples) %in% parm, , drop = FALSE]
  
  stopifnot(nrow(samples) >= 1)
  
  if(is_convertible_data_list(data)) {
    bol <- sapply(data, function (x, n) is.vector(x) && length(x) == n,
                  n = nrow(samples))
    if(any(bol)) {
      data <- data.frame(data[bol])
    } else
      data <- data.frame(row = 1:nrow(samples))
  }
  stopifnot(nrow(samples) == nrow(data))
  
  object <- cbind(data, samples)

  class(object) <- c("data.frame", "jags_sample")
  
  return (object)
}
