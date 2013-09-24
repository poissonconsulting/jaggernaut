
select <- function (object, ...) {
  UseMethod("select", object)
}

"select<-" <- function (object, value, ...) {
  UseMethod("select<-", object)
}

select.jags_model <- function (object, ...) {
  x <- list()
  for (i in 1:length(object$models))
    x[[i]] <- object$models[[i]]$select
  x <- delist(x)
  if (length(x) == 0)
    return (NULL)
  return (x)
}

"select<-.jags_model" <- function (object, value, ...) {
  
  if(is.null(value))
    return (object)

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
  
  for (i in 1:length(object$models))
    object$models[[i]]$select <- value
  return (object)
}

select.jagr_analysis <- function (object, ...) {
  return (select(as.jags_model(object, ...)))
}

