#' @title Get dataset(s) from a JAGS object
#'
#' @description
#' Gets the dataset(s) from a JAGS object.  
#' 
#' @param object a JAGS object.
#' @param ... further arguments passed to or from other methods.
#' @return a data.frame or list(s) of the data
#' @seealso \code{\link{dataset.jags_data_model}}  
#' @export
dataset <- function (object, ...) {
  UseMethod("dataset", object)
}

"dataset<-" <- function (object, value) {
  UseMethod("dataset<-", object)
}

#' @title Get dataset
#'
#' @description
#' Gets the dataset from a \code{jags_sample} object.  
#' 
#' @param object a \code{jags_sample} object.
#' @param ... further arguments passed to or from other methods.
#' @return The dataset.
#' @method dataset jags_sample
#' @export
dataset.jags_sample <- function (object, ...) {
  object <- object[,-grep("[[:digit:]]", colnames(object)), drop = FALSE]
  return (object)
}

#' @title Get dataset from a JAGS data model
#'
#' @description
#' Simulates a dataset from a \code{jags_data_model} object.  
#' 
#' @param object a \code{jags_data_model} object.
#' @param values a data.frame with a single row of data indicating the values 
#' for the simulation.
#' @param ... further arguments passed to or from other methods.
#' @return the simulated dataset in list form (unless modified by extract_data function).
#' @seealso \code{\link{dataset}} and \code{\link{jags_data_model}}
#' @method dataset jags_data_model
#' @export
dataset.jags_data_model <- function (object, values, ...) { 
  if (!is.data.frame(values) || nrow(values) != 1)
    stop("values must be a data.frame with a single row")
  
  if (options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb))
  }
  
  check_modules()
  
  values <- translate_data(select(object), values)
  
  data <- values
  
  if (is.function(modify_data(object))) 
    data <- modify_data(object)(data)
  
  if (is.function(gen_inits(object))) {
    inits <- list()
    inits[[1]] <- gen_inits(object)(data)
  } else
    inits <- NULL
  
  file <- tempfile(fileext=".bug")
  cat(paste(model_code(object),"model { deviance <- 1}"), file=file)
  
  chains <- jags_analysis_internal (
    data = data, file = file, monitor = monitor(object), 
    inits = inits
  )
  
  est <- extract_estimates(chains)[["estimate"]]
  
  est$deviance <- NULL
  
  data <- clist(data, est)
  
  values <- values[!names(values) %in% names(data)]
  
  if (length(values))
    data <- clist(values, data)
  
  if(is.function(extract_data(object)))
    data <- extract_data(object)(data)
  
  data <- data[order(names(data))]
  
  return (data)
}

#' @title Get dataset from a JAGS analysis object
#'
#' @description
#' Returns the original dataset from a \code{jags_analysis} object.  
#' 
#' @param object a \code{jags_analysis} object.
#' @param ... further arguments passed to or from other methods.
#' @return The original dataset.
#' @seealso \code{\link{dataset}}, \code{\link{jags_analysis}}
#' and \code{\link{jaggernaut}}
#' @method dataset jags_analysis
#' @export
dataset.jags_analysis <- function (object, ...) {
  return (object$data)
}

#' @title Get datasets from a JAGS simulation
#'
#' @description
#' Extracts datasets from a \code{jags_simulation} object.  
#' 
#' @param object a \code{jags_simulation} object.
#' @param ... further arguments passed to or from other methods.
#' @return The datasets as a list of lists.
#' @seealso \code{\link{dataset}} and \code{\link{jags_simulation}}
#' @method dataset jags_simulation
#' @export
dataset.jags_simulation <- function (object, ...) {  
  data <- object$data
  data <- name_object(data,c("value","replicate"))
  
  return (data)
}

"dataset<-.jags_analysis" <- function (object, value) {
  stopifnot(is_convertible_data(value))
  
  object$data <- value
  
  return (object)
}

"dataset<-.jags_simulation" <- function (object, value) {  
  stopifnot(is_list_list(value))
  stopifnot(is_scalar(unique(sapply(value,length))))
  stopifnot(length(value) == nvalues(object))
  
  object$data <- value
  
  return (object)
}
