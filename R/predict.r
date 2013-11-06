
#' @title JAGS analysis predictions
#'
#' @description
#' Calculate predictions with estimates for derived parameters 
#' in a JAGS analysis or if level = "no" returns an object of class jags_samples
#' 
#' @param object a jags_analysis.
#' @param newdata a data.frame or data list of the data values over which to calculate the
#' estimates of the derived parameter or a character vector specify the variable
#' or variable combination
#' for which to calculate the estimates of the derived parameter.
#' If NULL (the default) the derived parameter is calculated for each row in the original data set.
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
#' @param obs_by a character vector indicating which factors to only predict 
#' for their observed factor combinations.
#' @param ... further arguments passed to or from other methods.
#' @details
#' Its important to realise that if the original data set was a data list then 
#' newdata must be a data list or NULL, base must be FALSE and values must be NULL.
#' Otherwise if the original data set was a data frame then newdata can be NULL, a
#' character vector of the variables to generate data for or a data.frame and
#' base and values can be data.frames. 
#' 
#' Its also important to realize that values always replaces the corresponding
#' values in base but only replaces the corresponding values in newdata if they
#' are unaltered, i.e., as they are in data_jags(object,base = TRUE).
#' 
#' @return a data frame with the median estimates and credibility intervals for
#' the derived parameter of interest or if level = "no" an object of 
#' class jags_samples
#' @seealso \code{\link{jags_model}}, \code{\link{jags_analysis}}
#' and \code{\link{jaggernaut}}
#' @method predict jags_analysis
#' @export 
predict.jags_analysis <- function (object, newdata = NULL, 
                                   parm = "prediction", base = FALSE, 
                                   values = NULL, model_number = 1,
                                   modify_data_derived = NULL,
                                   derived_code = NULL, random_effects = NULL, 
                                   level = "current", length_out = 50, 
                                   obs_by = NULL, ...) {
  
  if (is.numeric(length_out)) {
    if (length(length_out)  != 1) {
      stop("length_out must be length one")
    }
    if (length_out < 10 || length_out > 100) {
      stop("length_out must lie between 10 and 100")
    }
  } else {
    stop("length_out must be an integer")
  }

  dataset <- data_jags(object)
    
  if(!is.null(obs_by)) {
    if(!is.character(obs_by))
      stop("obs_by must be NULL or a character vector")
    if (!is.data.frame(dataset))
      stop("obs_by is only available when the original data is a data.frame")
  }
  
  if (is.null(newdata)) {
    newdata <- dataset
  } else if (is.character(newdata)) {
    newdata <- generate_data (dataset, range = newdata, length_out = length_out)
  }
  
  if(is.logical(base)) {
    if (length(base) != 1) {
      stop("if logical base must be TRUE or FALSE")
    }
  } else {
    if (!is.data.frame(base)) {
      stop("base must be logical or a data.frame")
    }
    if (nrow(base) != 1) {
      stop("base must contain a single row of data")
    }
    if (ncol(base) == 0) {
      stop("base must contain at least one column of data")
    }    
  }
  
  if (!is.null(values)) {
    if (!is.data.frame(values)) {
      stop("values must be NULL or a data.frame")
    }
    if (nrow(values) != 1) {
      stop("values must contain a single row of data")
    }
    if (ncol(values) == 0) {
      stop("if a data.frame values must contain at least one column of data")
    }
  }
    
  if (identical(base,TRUE)) {
    base <- generate_data(data_jags(object))
  } else if (identical(base,FALSE)) {
    base <- NULL
  }
    
  if (is.data.frame (newdata)) {
    if (is_data_list(dataset)) {
      stop("if original data is a data list newdata must not be a data.frame")
    }
  } else {
    if (!is_data_list(newdata)) {
      stop("newdata must be NULL, character, data.frame or a data list")
    }
    if (is.data.frame(dataset)) {
      stop("if original data is a data.frame newdata must not be a data list")
    }
    if (is.data.frame(base)) {
      stop("if original data is a data list base must be FALSE")
    }
    if (is.data.frame(values)) {
      stop("if original data is a data list values must be NULL")
    } 
  }
  
  if (is_data_list(dataset)) {
    bol <- !names_data(dataset) %in% names_data(newdata)
    if (any(bol)) {
      dat <- dataset[bol]
      newdata <- c(newdata,dat)
    }
    newdata <- newdata[names(dataset)]
  } else {
    dat <- generate_data(data_jags(object))
    bol <- !names_data(dat) %in% names_data(newdata)
    if (any(bol)) {
      dat <- dat[bol]
      newdata <- merge(newdata,dat)
    }
    newdata <- newdata[names(dataset)]
    if (is.data.frame(base)) {
      dat <- generate_data(data_jags(object))
      bol <- !names_data(dat) %in% names_data(base)
      if (any(bol)) {
        dat <- dat[bol]
        base <- cbind(base,dat)
      }
      base <- base[names(dataset)]
    }
    if (is.data.frame(values)) {
      dat <- generate_data(data_jags(object))
      for (name in names_data(values)) {
        if (name %in% names_data(newdata)) {
          x <- newdata[[name]]
          if (all(x == dat[[name]])) {
            newdata[[name]] <- values[[name]]
          }
          if (is.data.frame(base)) {
            base[[name]] <- values[[name]]
          }
        }
      }
    }
  }  
  
  if (is.character(parm)) {
    if (length(parm) != 1) {
      stop("parm must be length one")
    }
  } else {
    stop("parm must be a character vector")
  }
  
  if (!is.numeric(level)) {
    if (!level %in% c("current","no")) {
      old_opts <- opts_jagr(mode = level)
      on.exit(opts_jagr(old_opts))
    }
    if (level == "no") {
      level <- 0
    } else
      level <- opts_jagr("level")
  } else {
    if (level < 0.75 || level > 0.99) {
      stop("level must lie between 0.75 and 0.99")
    }
  }
  
  object <- subset_jags(object, model_number = model_number)
  
  if(!is.null(modify_data_derived)) {
    modify_data_derived(object) <- modify_data_derived
  }
  
  if(!is.null(derived_code)) {
    derived_code(object) <- derived_code
  } else if (is.null(derived_code(object))) {
    stop("derived_code is undefined")
  }  
  
  if(!is.null(random_effects)) {
    random_effects(object) <- random_effects
  }

  if(is.character(obs_by)) {
    if (!all(obs_by %in% colnames(newdata)))
      stop("all obs_by must be in newdata")
  }
  
  if(!is.null(obs_by)) {
    dat <- unique(subset(dataset,select = obs_by))
    newdata <- merge(newdata, dat)
    stopifnot(nrow(newdata) > 0)
  }
  
  pred <- predict_internal(object, parm = parm, 
                           data = newdata, 
                           base = base, level = level, ...)
      
  rownames(pred) <- NULL
  
  return (pred)
}
