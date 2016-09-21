variable <- function(x) {
  UseMethod("variable", x)
}

variable.logical <- function (x) {
  x <- as.logical (x)
  
  levels <- c(FALSE, TRUE)
  
  obs <- sort(unique(x[!is.na(x)]))
  
  object <- list(levels = levels, obs = obs)    
  class(object) <- c("vlogical", "categorical", "variable")
  
  return (object)
}

variable.integer <- function (x) {
  x <- as.integer(x)
    
  obs <- sort(unique(x[!is.na(x)]))
  
  mean <- as.integer(round(mean(x, na.rm = TRUE)))
  sd <- sd(x, na.rm = TRUE)
  
  object <- list(mean = mean, sd = sd, obs = obs)    
  class(object) <- c("vinteger", "continuous", "variable")
  
  return (object)
}

variable.numeric <- function (x) {
    x <- as.numeric(x)
    
    obs <- sort(unique(x[!is.na(x)]))

    mean <- mean(x, na.rm = TRUE)
    sd <- sd(x, na.rm = TRUE)
    
    object <- list(mean = mean, sd = sd, obs = obs)    
    class(object) <- c("vnumeric", "continuous", "variable")
    
    return (object)
}

variable.character <- function (x) {  
    x <- as.factor (x)
    variable(x)
}

variable.factor <- function (x) {
    x <- as.factor (x)
    
    levels <- factor(levels(x), levels = levels(x))
    
    obs <- levels[levels %in% x]
    
    object <- list(levels = levels, obs = obs)    
    class(object) <- c("vfactor", "categorical", "variable")
    
    return (object)
}

variable.Date <- function (x) {
  x <- as.Date (x)
  
  obs <- sort(unique(x[!is.na(x)]))
  
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  
  object <- list(mean = mean, sd = sd, obs = obs)    
  class(object) <- c("vdate", "continuous", "variable")
  
  return (object)
}

variable.POSIXt <- function (x) {
  x <- as.POSIXct (x)
  
  obs <- sort(unique(x[!is.na(x)]))

  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  
  object <- list(mean = mean, sd = sd, obs = obs)    
  class(object) <- c("vposixt", "continuous", "variable")
  
  return (object)
}

variable.matrix <- function (x) {
    
  smode <- storage.mode(x)
  ndim <- 2
  
  obs <- sort(unique(x[!is.na(x)]))
  
  if (smode == "logical") {
    
    levels <- c(FALSE, TRUE)
    
    object <- list(levels = levels, obs = obs, ndim = ndim)    
    class(object)<-c("mlogical", "categorical", "variable")
  } else if (smode == "integer") {
    
    mean <- as.integer(round(mean(x, na.rm = TRUE)))
    sd <- sd(x, na.rm = TRUE)
        
    object <- list(mean = mean, sd = sd, obs = obs, ndim = ndim)    
    class(object) <- c("minteger", "continuous", "variable")
  } else if (smode == "double") {
    
    mean <- mean(x, na.rm = TRUE)
    sd <- sd(x, na.rm = TRUE)    
    
    object <- list(mean = mean, sd = sd, obs = obs, ndim = ndim)    
    class(object) <- c("mdouble", "continuous", "variable")
  } else
    stop(paste("storage mode", smode, "not recognised"))
  
  return (object)
}

variable.array <- function (x) {
    
  smode <- storage.mode(x)
  ndim <- length(dim(x))
  
  obs <- sort(unique(x[!is.na(x)]))
  
  if (smode == "logical") {
    
    levels <- c(FALSE, TRUE)
    
    object <- list(levels = levels, obs = obs, ndim = ndim)    
    class(object)<-c("alogical", "categorical", "variable")
  } else if (smode == "integer") {
    
    mean <- as.integer(round(mean(x, na.rm = TRUE)))
    sd <- sd(x, na.rm = TRUE)
    
    object <- list(mean = mean, sd = sd, obs = obs, ndim = ndim)    
    class(object) <- c("ainteger", "continuous", "variable")
  } else if (smode == "double") {
    
    mean <- mean(x, na.rm = TRUE)
    sd <- sd(x, na.rm = TRUE)    
    
    object <- list(mean = mean, sd = sd, obs = obs, ndim = ndim)    
    class(object) <- c("adouble", "continuous", "variable")
  } else
    stop(paste("storage mode", smode, "not recognised"))
  
  return (object)
}
