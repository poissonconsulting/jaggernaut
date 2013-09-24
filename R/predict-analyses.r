
#' @title JAGS analyses predictions
#'
#' @description
#' Calculate predictions with estimates for derived parameters 
#' in multiple JAGS analyses
#' 
#' @param object a list of jags_analyses
#' @param newdata a data.frame or data list of the data values over which to calculate the
#' estimates of the derived parameter or a character vector specify the variable
#' or variable combination
#' for which to calculate the estimates of the derived parameter.
#' If NULL (the default) the derived parameter is calculated for each row in the original data set.
#' @param fun the function to apply to iterations.
#' @param parm a character element naming the derived parameter for which 
#' the estimates should be calculated.
#' @param base a boolean element indicating whether or not to express 
#' the expected value as a percent change of a base level or a data frame 
#' defining the base level.
#' @param values a data frame with a single row that defines the value of particular
#' variables. The variables in the arguments newdata and base are replaced by the corresponding values.
#' @param model_number an integer vector specifying the model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param derived_code a character element defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameters 
#' for each row of data. 
#' If NULL derived_code is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param random_effects a named list which specifies which parameters to treat 
#' as random variables in the derived code. If NULL random_effects is as defined by the JAGS model for which the JAGS analysis was performed. 
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @param length_out an integer element indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @param obs_comb an logical element indicating whether to only 
#' predict for observed factor combinations in the original data when 
#' specifying newdata by a character vector.
#' @return a data frame with the median estimates and credibility intervals for
#' the derived parameter of interest
#' @seealso \code{\link{predict.jags_analysis}}
predict_analyses <- function (object, newdata = NULL, fun = sum,
                                   parm = "prediction", base = FALSE, 
                                   values = NULL, model_number = 1, 
                                   derived_code = NULL, random_effects = NULL, 
                                   level = "current", length_out = 50, obs_comb = FALSE) {
  
  if(!is.list(object)) {
    stop("object must be a list of jags_analysis objects")
  }
  if (!is.function (fun)) {
    stop("fun must be a function")
  }
  
  if(!is.numeric(model_number)) {
    stop("model_number must be an integer")
  }
  nobject <- length(object)
  if(!length(model_number) %in% c(1,nobject)) {
    stop("model_number must be a single value or the same length as object")
  }
  
  x <- list()
  for (i in seq_along(object)) {
    
    quiet <- opts_jagr("quiet")
    
    if (!quiet) {
      cat(paste("\nAnalysis",i,"of",nobject,"\n"))
    }
    if (!is.jags_analysis(object[[i]])) {
      stop("object must be a list of jags_analysis objects")
    }
    mod_num <- model_number
    if (length(model_number) > 1) {
      mod_num <- model_number[i]
    }
    x[[i]] <- predict(object[[i]], newdata = newdata, parm = parm, 
                          base = base, values = values, 
                          model_number = mod_num, 
                          derived_code = derived_code, 
                          random_effects = random_effects, 
                          level = "no", length_out = 50, 
                          obs_comb = obs_comb)
  }

  if (is.character(newdata)) {
    x <- combine_jags_samples(x, by = newdata, fun = fun)
  } else {
    x <- combine_jags_samples(x, fun = fun)    
  }
  if (level == "no")
    return (x)
  return (coef(x, level = level))
}
