
#' @title JAGS analysis fitted values
#'
#' @description
#' Calculate fitted values with credible limits for a JAGS analysis
#' 
#' @param object a jags_analysis object.
#' @param parm a character scalar naming the derived parameter for which 
#' the estimates should be calculated (default = "prediction").
#' @param model_number an integer scalar specifying the model to select. 
#' @param derived_code a character scalar defining a block in the 
#' JAGS dialect of  the BUGS language that defines one or more derived
#' parameters for each row of data. 
#' If NULL derived_code is as defined by the JAGS model for which 
#' the JAGS analysis was performed. 
#' @param random_effects a named list which specifies which parameters to treat 
#' as random variables. If NULL random is as defined by the JAGS model for which
#' the JAGS analysis was performed. 
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param estimate a character scalar specifying whether the point estimate should
#' be the "mean" or the "median" or a character scalar indicating which mode the 
#' level should be taken from. By default the
#' estimate is as currently specified by \code{opts_jagr} in the global options.
#' @param data the dataset for which to calculate the residuals. By default data 
#' is NULL as the residuals are typically calculated on the original dataset.
#' @param ... further arguments passed to or from other methods.
#' @return the original data frame with the point estimate and lower and upper 
#' credibility limits for the derived parameter of interest
#' @seealso \code{\link{jags_model}}, \code{\link{jags_analysis}}
#' and \code{\link{predict.jags_analysis}}
#' @method fitted jags_analysis
#' @export 
fitted.jags_analysis <- function (object, parm = "prediction",  model_number = 1, 
                                  derived_code = NULL, random_effects = NULL, 
                                  level = "current", estimate = "current",
                                  data = NULL, ...) {
  
  if(!is_character_scalar(level) && !is_character_scalar(level))
    stop("level must be a character or numeric scalar")
  
  if(identical(as.character(level),"no"))
    stop("level must not be 'no'")
  
  if(!is_null(data) && !is_convertible_data(data))
    stop("data must be NULL or a data.frame or list of data")
  
  if(is_convertible_data_frame(dataset(object))) {
    if(is_convertible_data_list(data))
      stop("as original dataset is a data frame data must not be a data list")
  } else {
    if(!is_null(data) || !is_convertible_data_list(data))
      stop("as original dataset is a data list data must be NULL or a data list")
  }  
  
  return (predict(object, newdata = data, parm = parm, model = model_number, 
                  derived_code = derived_code, random_effects = random_effects, 
                  level = level, ...))
}
