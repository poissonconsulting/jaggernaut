
#' @title Calculate convergence values
#'
#' @description
#' Calculates convergence (R-hat) values for the parameters in a JAGS analysis
#' 
#' @param object a jags_analysis object
#' @param model_number an integer element specifying the model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param param a character vector specifying the parameters for which to calculate the convergence
#' @return a data.frame of the parameters with their convergence (R-hat) values
#' @seealso \code{\link{jaggernaut}} and \code{\link{jags_analysis}}
#' @export
convergence <- function (object, model_number = 1, param = "fixed") {
  if(!is.jags_analysis(object))
    stop ("object should be class jags_analysis")
  
  object <- subset(object, model = model_number)
  
  con <- calc_convergence.jags_analysis (object, summarise = FALSE, type = param)
  
  if (!"all" %in% param || !"deviance" %in% param) {
    con <- con[rownames(con) != "deviance",]
  }
  con$independence <- NULL
  
  return (con)
}
