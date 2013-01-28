
zero_random<- function (object, ...) {
  UseMethod("zero_random", object)
}

zero_random.mcarray <- function (object) {
  object[] <- 0  
  return (object)
}

zero_random.gsmcmc <- function (object, random) {
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

zero_random.janalysis <- function (object, data) {
  data <- as.data.frame (data)
  
  dat <- convert_data(object$data, standardise=object$model$standardise,
                      dat = data)
  
    for (colname in colnames(data)) {
      var <- data[,colname,drop=T] 
      va <- unique(dat[,colname,drop=T])
      if (!(is.factor(var) && length(va) == 1 && va == 1))
        data[,colname] <- NULL
    }
  
  random <- object$model$random
  
  for (ran in names(random))
    if (!any(random[[ran]] %in% colnames(data)))
      random[[ran]] <- NULL
  
  if (length(random))
    object$mcmc <- zero_random (object$mcmc, random = random)
  
  return (object)
}