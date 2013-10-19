
#' @title Get dataset(s) from a JAGS object
#'
#' @description
#' Gets the dataset(s) from a JAGS object.  
#' 
#' @param object a object.
#' @param ... other arguments.
#' @return a data.frame or list(s) of the data
#' @seealso \code{\link{data_jags.jags_data_model}}  
#' @export
data_jags <- function (object, ...) {
  UseMethod("data_jags", object)
}

"data_jags<-" <- function (object, ...) {
  UseMethod("data_jags<-", object)
}

#' @title Get dataset from a JAGS data model
#'
#' @description
#' Simulates a dataset from a JAGS data model.  
#' 
#' @param object a JAGS data model.
#' @param values a data.frame with a single row of data indicating the values for the simulation.
#' @param ... other arguments passed to generic function.
#' @return the simulated dataset in list form (unless modified by extract_data function).
#' @seealso \code{\link{data_jags}} and \code{\link{jags_data_model}}
#' @method data_jags jags_data_model
#' @export
data_jags.jags_data_model <- function (object, values, ...) { 
  if (!is.data.frame(values) || nrow(values) != 1)
    stop("values must be a data.frame with a single row")

  if (options()$jags.pb != "none") {
    jags.pb <- options()$jags.pb
    options(jags.pb = "none")
    on.exit(options("jags.pb" = jags.pb))
  }
  
  values <- translate_data(select(object), values)
  
  data <- values
  
  if (is.function(modify_data(object))) 
    data <- modify_data(object)(data)
  
  if (is.function(gen_inits(object))) {
    inits <- list()
    inits[[i]] <- gen_inits(object)(data)
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

#' @method data_jags jags_analysis
#' @export
data_jags.jags_analysis <- function (object, ...) 
  object$data

#' @method data_jags jags_simulation
#' @export
data_jags.jags_simulation <- function (object, ...) {  
  data <- object$data
  data <- name_object(data,c("value","replicate"))
  
  return (data)
}

"data_jags<-.jags_analysis" <- function (object, value, ...) {

  if (!is.data.frame (value)) {
    if (!is.list(value)) {
      stop("value must be a data.frame or a data list")
    }
    names <- names(value)
    if(is.null(names)) {
      stop("variables in value must be named")
    }
    classes <- c("logical","integer","numeric","factor",
                 "Date","POSIXt","matrix","array")
    bol <- sapply(value, inherits,classes[1])
    for (class in classes[-1]) {
      bol <- bol | sapply(value,inherits,class)
    }
    if (!all(bol)) {
      stop(paste("variables in data list must be class",classes))
    }
    if (!is_data_list (value)) {
      stop("value must be a data.frame or a data list")
    }
  } else {
    if (!nrow(value)) {
      stop("value must include at least one row")
    }
    if (!ncol(value)) {
      stop("value must include at least one column")
    }
  }
  
  object$data <- value
  
  return (object)
}

"data_jags<-.jags_simulation" <- function (object, value, ...) {  
  if (!is.list(value) || !is.list(value)[[1]])
      stop("value must be a list of lists")
  
  stopifnot(is_scalar(unique(sapply(value,length))))
  stopifnot(length(value) == nvalues(object))
  
  object$data <- value
  
  return (object)
}
