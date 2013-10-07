
zero_random<- function (object, ...) {
  UseMethod("zero_random", object)
}

zero_random.mcarray <- function (object, ...) {
  object[] <- 0  
  return (object)
}

zero_random.jagr_chains <- function (object, random_variable, ...) {
  stopifnot(is.character(random_variable))
  
  random_variable <- random_variable[random_variable %in% names(object$mcmc)]
  
  for (variable in random_variable)
    object$mcmc[[variable]] <- zero_random(object$mcmc[[variable]], ...)
  
  return (object)
}

zero_random.jagr_power_analysis <- function (object, data) {
  stopifnot(is.list(data))
    
  for (name in names_data(data)) {
    var <- data[[name]]
    va <- unique(var)
    if (!(paste0("n",name) %in% names_data(data) && length(va) == 1 && va == 1))
      data[[name]] <- NULL
  }
  
  random_effects <- random_effects(object)
      
  for (ran in names(random_effects))
    if (!any(random_effects[[ran]] %in% names_data(data)))
      random_effects[[ran]] <- NULL
      
  if (length(random_effects))
    return (zero_random (chains(object), names_random = names(random_effects)))
  
  return (chains(object))
}

zero_random.jags_analysis <- function (object, data, ...)
  return (zero_random (analysis(object), data, ...))

