
zero_random<- function (object, ...) {
  UseMethod("zero_random", object)
}

zero_random.mcarray <- function (object) {
  object[] <- 0  
  return (object)
}

zero_random.jags_mcmc <- function (object, random) {
  if (!is.list(random)) {
    stop ("random must be a list")
  }
  
  unknown <- names(random)[!names(random) %in% names(object$mcmc)]
  if (length (unknown))
    warning (paste0("the following variables are in random but not object:",
                    unknown))
  
  random <- random[names(random) %in% names(object$mcmc)]
  
  for (ran in names(random)) {
    object$mcmc[[ran]] <- zero_random(object$mcmc[[ran]])
  }
  return (object)
}

zero_random.jagr_analysis <- function (object, data) {
  
  dat <- convert_data(object$data, standardise=object$model$standardise,
                      dat = data)
  
    for (name in names_data(data)) {
      var <- data[[name]] 
      va <- unique(dat[[name]])
      if (!(is.factor(var) && length(va) == 1 && va == 1))
        data[[name]] <- NULL
    }
  
  random <- object$model$random
    
  for (ran in names(random))
    if (!any(random[[ran]] %in% names_data(data)))
      random[[ran]] <- NULL
    
  if (length(random))
    object$mcmc <- zero_random (object$mcmc, random = random)
  
  return (object)
}
