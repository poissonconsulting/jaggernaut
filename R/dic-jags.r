
#' @title DIC values
#'
#' @description
#' Gets Deviance Information Criterion values from a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return The DIC values as a data.frame.
#' @seealso \code{\link{jaggernaut}}  
#' @export
dic_jags <- function (object, ...) {
  UseMethod("dic_jags", object)
}

dic_jags.jagr_chains <- function (object, ...) {
  
  mat <- as.matrix(object)
  
  deviance <- mat[,colnames(mat)  == "deviance"]

  deviance <- as.vector(deviance)
  pD <- var(deviance) / 2
  Dbar <- mean(deviance)
  DIC <- Dbar + pD
  
  return (c(DIC = DIC, Dbar = Dbar, pD = pD))
}

dic_jags.jagr_power_analysis <- function (object, ...) {
  return (dic_jags(as.jagr_chains(object), ...))
}

dic_jags_jagr_power_analysis <- function (object, ...) {
  stopifnot(is.jagr_power_analysis(object))
  return (dic_jags(object, ...))
}

#' @title DIC values
#'
#' @description
#' Gets Deviance Information Criterion (DIC) values from a JAGS analysis.  
#' 
#' @param object a \code{jags_analysis}.
#' @param ... further arguments passed to or from other methods.
#' @return The DIC values as a data.frame.
#' @seealso \code{\link{jaggernaut}}  
#' @method dic_jags jags_analysis
#' @export 
dic_jags.jags_analysis <- function (object, ...) {
  
  if (is.null(object$dic)) {
    analyses <- analyses(object)
  
    dic <- t(sapply(analyses,dic_jags_jagr_power_analysis))  
    rownames(dic) <- paste0("Model",1:nrow(dic))  
    object$dic <- dic[order(dic[,"DIC",drop=T]),]
  }
  
  return (object$dic)
}
