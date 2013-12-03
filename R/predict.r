#' @title Predict
#'
#' @description
#' Predict derived parameter from JAGS analysis. Level specifies the credibility
#' intervals with or if level = "no" the function 
#' returns an object of class jags_samples
#' 
#' @param object a jags_analysis object.
#' @param newdata a data.frame or data list of the data values over which to
#' calculate the estimates of the derived parameter or a character vector specify 
#' the variable or variable combination for which to calculate the estimates of 
#' the derived parameter. If NULL (the default) the derived parameter is 
#' calculated for each row in the original data set.
#' @param parm a character scalar indicating the derived parameter for which 
#' the estimates should be calculated by default = "prediction"
#' @param base a logical scalar indicating whether to express 
#' the expected value as a percent change of the base level or a data frame 
#' defining the base level.
#' @param values NULL or a data frame with a single row that defines the value 
#' of particular variables. The variables in the arguments newdata and base are
#' replaced by the corresponding values.
#' @param model_number an integer scalar specifying the jags model to select. 
#' If model_number = 0 then it selects the model with the lowest DIC.
#' @param modify_data_derived a function to modify the derived data set 
#' (after it has been converted to list form)
#' @param derived_code a character scalar defining a block in the JAGS dialect of 
#' the BUGS language that defines one or more derived parameters for each row 
#' of data. If NULL then derived_code is as defined by the JAGS model for which 
#' the JAGS analysis was performed. 
#' @param random_effects a named list which specifies which parameters to treat 
#' as random variables in the derived code. If NULL random_effects is as defined 
#' by the JAGS model for which the JAGS analysis was performed. 
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options. If
#' level = "no" then the function returns a \code{jags_sample} object of the
#' derived parameter.
#' @param estimate a character scalar indicating whether the point estimate 
#' should be the "mean" or the "median". By default the estimate is as 
#' currently defined by \code{opts_jagr} in the global options.
#' @param obs_by a logical scalar or a character vector indicating which factors 
#' to only predict for their observed factor combinations. If obs_by = TRUE then
#' newdata must be a character vector and the factors are taken to be those 
#' in newdata.
#' @param length_out an integer scalar indicating the number of values when 
#' creating a sequence of values across the range of a continuous variable.
#' @param ... further arguments passed to or from other methods.
#' @details
#' Its important to realise that if the original data set was a data list then 
#' newdata must be a data list or NULL, base must be FALSE and values must be NULL.
#' Otherwise if the original data set was a data frame then newdata cannot be a 
#' data list.
#' 
#' Its also important to realize that values always replaces the corresponding
#' values in base but only replaces the corresponding values in newdata if they
#' are unaltered, i.e., as they are in data_jags(object, base = TRUE).
#' 
#' @return the \code{coef} table for the derived parameter of interest 
#' or if level = "no" an object of class \code{jags_samples}
#' @seealso \code{\link{jags_model}}, \code{\link{jags_analysis}},
#' \code{\link{coef.jags_analysis}}, \code{\link{jags_sample}}
#'  and \code{\link{jaggernaut}}
#' @method predict jags_analysis
#' @export 
predict.jags_analysis <- function (object, newdata = NULL, 
                                   parm = "prediction", base = FALSE, 
                                   values = NULL, model_number = 1,
                                   modify_data_derived = NULL,
                                   derived_code = NULL, random_effects = NULL, 
                                   level = "current", estimate = "current", 
                                   obs_by = FALSE, length_out = 50, ...) {
  
  if(!is_null(newdata) && !is_data(newdata) &&
       !identical(newdata, "") && !is_character_vector(newdata))
    stop("newdata must be NULL or a data.frame or list of data or a character vector")

  if(!is_character_scalar(parm))
    stop("parm must be a character scalar")

  if(!is_logical_scalar(base) && !(is_data_frame(base) && nrow(base) == 1))
    stop("base must be an logical scalar or a data.frame with one row")

  if(!is_null(values) && !(is_data_frame(values) && nrow(values) == 1))
    stop("values must be NULL or a data.frame with a single row of data")

  if(!is_integer_scalar(model_number) && !is_bounded(model_number,0))
    stop("model_number must be an integer scalar of 0 or greater")

  if(!is_null(modify_data_derived) && !is_function(modify_data_derived))
    stop("modify_data_derived must be NULL or a function")
  
  if(!is_null(derived_code) && !is_character_scalar(derived_code))
    stop("derived_code must be NULL or a character scalar")

  if(!is_null(random_effects) && !is_named_list(random_effects))
    stop("random_effects must be NULL or a named list")

  if(!is_character_scalar(level) && !is_character_scalar(level))
    stop("level must be a character or numeric scalar")

  if(!is_character_scalar(estimate))
    stop("estimate must be a character vector")
  
  if(!is_integer_scalar(length_out) || !is_bounded(length_out, 10, 100))
    stop("length_out must be a integer between 10 and 100")

  if(!is_logical_scalar(obs_by) && !is_character_vector(obs_by))
    stop("obs_by must be a logical scalar or a character vector")
  
  if(is_TRUE(obs_by) && !is_character_vector(newdata))
    stop("if obs_by is TRUE newdata must be a character vector - otherwise
         specify the factors directly in obs_by")
  
  if (is.null(derived_code(object)) && is_null(derived_code))
    stop("derived_code must be defined if undefined in object")
  
  if (is_character_scalar(level)) {
    if (!level %in% c("current","no")) {
      old_opts <- opts_jagr(mode = level)
      on.exit(opts_jagr(old_opts))
    } 
    if (level == "no") {
      level <- 0
    } else
      level <- opts_jagr("level")
  } else if (!is_bounded(level, 0.75, 0.99))
      stop("level must lie between 0.75 and 0.99")
  
  if(!estimate %in% c("mean","median") && estimate != "current") {
    old_opts <- opts_jagr(mode = level)
    if(is.null(sys.on.exit()))
      on.exit(opts_jagr(old_opts))
  }
  
  if (!estimate %in% c("mean","median")) {
    estimate <- opts_jagr("estimate")
  }
  
  object <- subset_jags(object, model_number = model_number)
  
  if(!is_null(modify_data_derived))
    modify_data_derived(object) <- modify_data_derived
  
  if(!is.null(derived_code))
    derived_code(object) <- derived_code 
  
  if(!is_null(random_effects))
    random_effects(object) <- random_effects
  
  if(is_TRUE(obs_by))
    obs_by <- newdata
  
  data <- data_jags(object)
  
  if(is.jags_data_frame(data)) {
    if(is_data_list(newdata))
      stop("as original dataset is a data frame newdata must not be a data list")
    if (!is_FALSE(obs_by) && !all(obs_by %in% colnames(data)))
      stop("all obs_by must be in data")
  } else {
    if(!is_null(newdata) && !is_data_list(newdata))
      stop("as original dataset is a data list newdata must be NULL or a data list")
    if(!is_FALSE(base))
      stop("as original dataset is a data list base must be FALSE")    
    if(!is_null(values))
      stop("as original dataset is a data list values must be NULL") 
    if(!is_FALSE(obs_by))
      stop("as original dataset is a data list obs_by must be FALSE") 
  }
  
  if (is.null(newdata)) {
    newdata <- data
  } else if (is_character_vector(newdata))
    newdata <- generate_data (data, range = newdata, length_out = length_out)
  
  if (is_TRUE(base)) {
    base <- generate_data(data)
  } else if(is_FALSE(base))
    base <- NULL
  
  if (is.jags_data_list(data)) {
    bol <- !names(data) %in% names(newdata)
    if (any(bol)) {
      newdata <- c(newdata, data[bol])
    }
    newdata <- newdata[names(data)]
  } else {
    dat <- generate_data(data)
    bol <- !names(dat) %in% names(newdata)
    if (any(bol)) {
      newdata <- merge(newdata, dat[bol])
    }
    newdata <- newdata[names(data)]
    
    if (is_data_frame(base)) {
      bol <- !names(dat) %in% names(base)
      if (any(bol)) {
        base <- cbind(base, dat[bol])
      }
      base <- base[names(data)]
    }
    
    if (is_data_frame(values)) {
      for (name in names(values)) {
        if (name %in% names(newdata)) {
          x <- newdata[[name]]
          if (all(x == dat[[name]])) {
            newdata[[name]] <- values[[name]]
          }
          if (is_data_frame(base)) {
            base[[name]] <- values[[name]]
          }
        }
      }
    }
  }  

  if(is_character_vector(obs_by)) {
    dat <- unique(data[, colnames %in% obs_by])
    newdata <- merge(newdata, dat, by = colnames(dat))
  }
  
  newdata <- as.jags_data(newdata)
  if(!is_null(base))
    base <- as.jags_data(base)
  
  pred <- predict_jagr(object, parm = parm, data = newdata, base = base, 
                           level = level, estimate = estimate, ...)
      
  rownames(pred) <- NULL
  
  return (pred)
}
