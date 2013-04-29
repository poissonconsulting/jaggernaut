
dvariable <- function(x) {
  UseMethod("dvariable", x)
}

dvariable.logical <- function (x) {
  x <- as.logical (x)
  
  min = FALSE
  mean = NA
  max = TRUE
  sd = NA
  object <- list(min = min, mean = mean, max = max, sd = sd)    
  class(object)<-c("vlogical","dvariable")
  
  return (object)
}

dvariable.integer <- function (x) {
  x <- as.integer(x)
  
  min = as.integer(min(x, na.rm=T))
  mean = as.integer(round(mean(x, na.rm=T)))
  max = as.integer(max(x, na.rm=T))
  sd = sd(x, na.rm=T)
  
  object <- list(min = min, mean = mean, max = max, sd = sd)    
  class(object)<-c("vinteger","dvariable")
  
  return (object)
}

dvariable.numeric <- function (x) {
    x <- as.numeric(x)
    
    min = min(x, na.rm=T)
    mean = mean(x, na.rm=T)
    max = max(x, na.rm=T)
    sd = sd(x, na.rm=T)
    
    object <- list(min = min, mean = mean, max = max, sd = sd)    
    class(object)<-c("vnumeric","dvariable")
    
    return (object)
}

dvariable.factor <- function (x) {
    x <- as.factor (x)
    
    levels<-factor(levels(x),levels=levels(x))
    min = levels[1]
    mean = NA
    max = levels[nlevels(x)]
    sd = NA
    object <- list(min = min, mean = mean, max = max, sd = sd, levels = levels)    
    class(object)<-c("vfactor","dvariable")
    
    return (object)
}

dvariable.Date <- function (x) {
  x <- as.Date (x)
  
  min = min(x, na.rm=T)
  mean = mean(x, na.rm=T)
  max = max(x, na.rm=T)
  sd = sd(x, na.rm=T)
  object <- list(min = min, mean = mean, max = max, sd = sd)    
  class(object)<-c("vdate","dvariable")
  
  return (object)
}

dvariable.POSIXt <- function (x) {
  x <- as.POSIXct (x)
  
  min = min(x, na.rm=T)
  mean = mean(x, na.rm=T)
  max = max(x, na.rm=T)
  sd = sd(x, na.rm=T)
  object <- list(min = min, mean = mean, max = max, sd = sd)    
  class(object)<-c("vposixt","dvariable")
  
  return (object)
}

dvariable.matrix <- function (x) {
  
  smode <- storage.mode(x)
  ndim <- 2
  if (smode == "logical") {
  
    min = FALSE
    mean = NA
    max = TRUE
    sd = NA
    object <- list(min = min, mean = mean, max = max, sd = sd, ndim = ndim)    
    class(object)<-c("mlogical","dvariable")
  } else if (smode == "integer") {
    
    min = as.integer(min(x, na.rm=T))
    mean = as.integer(round(mean(x, na.rm=T)))
    max = as.integer(max(x, na.rm=T))
    sd = sd(x, na.rm=T)
    
    object <- list(min = min, mean = mean, max = max, sd = sd, ndim = ndim)    
    class(object)<-c("minteger","dvariable")
  } else if (smode == "double") {
    
    min = min(x, na.rm=T)
    mean = mean(x, na.rm=T)
    max = max(x, na.rm=T)
    sd = sd(x, na.rm=T)    
    object <- list(min = min, mean = mean, max = max, sd = sd, ndim = ndim)    
    class(object)<-c("mdouble","dvariable")
  } else {
    stop(paste("storage mode",smode,"not recognised"))
  } 
  return (object)
}

dvariable.array <- function (x) {
  
  smode <- storage.mode(x)
  ndim <- length(dim(x))
  if (smode == "logical") {
    
    min = FALSE
    mean = NA
    max = TRUE
    sd = NA
    object <- list(min = min, mean = mean, max = max, sd = sd, ndim = ndim)    
    class(object)<-c("alogical","dvariable")
  } else if (smode == "integer") {
    
    min = as.integer(min(x, na.rm=T))
    mean = as.integer(round(mean(x, na.rm=T)))
    max = as.integer(max(x, na.rm=T))
    sd = sd(x, na.rm=T)
    
    object <- list(min = min, mean = mean, max = max, sd = sd, ndim = ndim)    
    class(object)<-c("ainteger","dvariable")
  } else if (smode == "double") {
    
    min = min(x, na.rm=T)
    mean = mean(x, na.rm=T)
    max = max(x, na.rm=T)
    sd = sd(x, na.rm=T)    
    object <- list(min = min, mean = mean, max = max, sd = sd, ndim = ndim)    
    class(object)<-c("adouble","dvariable")
  } else {
    stop(paste("storage mode",smode,"not recognised"))
  } 
  return (object)
}
