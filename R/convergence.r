
#' @title Calculate convergence values
#'
#' @description
#' Calculates convergence (R-hat) values for the parameters in a JAGS analysis
#' 
#' @param object a jags_analysis object
#' @param model an integer element specifying the model to select. 
#' If model = 0 then it selects the model with the lowest DIC.
#' @param parameters a character vector specifying the parameters for which to calculate the convergence
#' @return a data.frame of the parameters with their convergence (R-hat) values
#' @seealso \code{\link{jaggernaut}} and \code{\link{analysis}}
#' @export
convergence <- function (object, model = 1, parameters = "fixed") {
  if(!is.jags_analysis(object))
    stop ("object should be class jags_analysis")
  
  object <- subset(object, model = model)
  
  con <- calc_convergence.jags_analysis (object, summarise = FALSE, type = parameters)
  
  if (!"all" %in% parameters || !"deviance" %in% parameters) {
    con <- con[rownames(con) != "deviance",]
  }
  con$independence <- NULL
  
  return (con)
}
