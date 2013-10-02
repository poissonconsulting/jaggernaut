
select <- function (object, ...) {
  UseMethod("select", object)
}

"select<-" <- function (object, value, ...) {
  UseMethod("select<-", object)
}

select.jagr_model <- function (object, ...) {
  return (object$select)
}

select_jagr_model <- function (object, ...) {
  stopifnot(is.jagr_model(object))
  return (select(object, ...))
}

select.jags_model <- function (object, ...) {
  
  object <- as.jagr_model(object)
  
  if(is.jagr_model(object))
    return (select(object, ...))
  
  object <- lapply(object, select_jagr_model, ...)
  
  object <- delist(object)
  if (length(object) == 0)
    return (NULL)
  return (object)  
}

select.jags_data_model <- function (object, ...) {
  return (select(as.jags_model(object), ...))
}

select.jagr_analysis <- function (object, ...) {
  return (select(as.jagr_model(object, ...)))
}

select_jagr_analysis <- function (object, ...) {
  stopifnot(is.jagr_analysis(object))
  return (select(object, ...))
}

select.jags_analysis <- function (object, ...) {
  return (select(as.jags_model(object), ...))
}

"select<-.jagr_model" <- function (object, value, ...) {
  
  if (!is.null(value)) {
    if (!is.character(value)) {
      stop ("select must be NULL or class character")
    }
    if (!length(value)) {
      stop ("select must be NULL or define at least one variable to include")
    }
    names <- names_select(value)
    if (any(duplicated(names))) {
      stop ("variables to select must be unique")
    }
  }
  
  object$select <- value
  
  return (object)
}


"select<-.jags_model" <- function (object, value, ...) {
  
  for (i in 1:nmodel(object))
    select(object$models[[i]], ...) <- value
  return (object)
}

"select<-.jags_data_model" <- function (object, value, ...) {
  
  for (i in 1:nmodel(object))
    select(object$models[[i]], ...) <- value
  return (object)
}

