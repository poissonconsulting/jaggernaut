
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
  class(object)<-c("dlogical","dvariable")
  
  return (object)
}

dvariable.numeric <- function (x) {
    x <- as.numeric(x)
    
    min = min(x, na.rm=T)
    mean = mean(x, na.rm=T)
    max = max(x, na.rm=T)
    sd = sd(x, na.rm=T)
    
    object <- list(min = min, mean = mean, max = max, sd = sd)    
    class(object)<-c("dnumeric","dvariable")
    
    return (object)
}

dvariable.integer <- function (x) {
  x <- as.integer(x)
  
  min = as.integer(min(x, na.rm=T))
  mean = as.integer(round(mean(x, na.rm=T)))
  max = as.integer(max(x, na.rm=T))
  sd = sd(x, na.rm=T)
  
  object <- list(min = min, mean = mean, max = max, sd = sd)    
  class(object)<-c("dinteger","dvariable")
    
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
    class(object)<-c("dfactor","dvariable")
    
    return (object)
}

dvariable.Date <- function (x) {
  x <- as.Date (x)
  
  min = min(x, na.rm=T)
  mean = mean(x, na.rm=T)
  max = max(x, na.rm=T)
  sd = sd(x, na.rm=T)
  object <- list(min = min, mean = mean, max = max, sd = sd)    
  class(object)<-c("ddate","dvariable")
  
  return (object)
}

dvariable.POSIXt <- function (x) {
  x <- as.POSIXct (x)
  
  min = min(x, na.rm=T)
  mean = mean(x, na.rm=T)
  max = max(x, na.rm=T)
  sd = sd(x, na.rm=T)
  object <- list(min = min, mean = mean, max = max, sd = sd)    
  class(object)<-c("dposixt","dvariable")
  
  return (object)
}
