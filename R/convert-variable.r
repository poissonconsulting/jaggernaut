
convert_variable <- function(object, x, numericise = T, centre = F, standardise = F) {
  UseMethod("convert_variable", object)
}

convert_variable.dlogical <- function(object, x, numericise = T, centre = F, 
                                     standardise = F) {
  
  x <- as.logical(x)
  
  if (numericise || centre || standardise)
    x <- as.integer(x)
  
  return (x)
}

convert_variable.dnumeric <- function(object, x, numericise = T, centre = F, standardise = F) {

  x <- as.numeric (x)
  
  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre)
    x <- x - object$mean
  
  return (x)
}

convert_variable.dinteger <- function(object, x, numericise = T, centre = F, standardise = F) {
  
  x <- as.integer(x)

  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre)
    x <- x - object$mean

  return (x)
}

convert_variable.dfactor <- function(object, x, numericise = T, centre = F, standardise = F) {
    
  x <- factor(as.character (x), levels = object$levels)
    
  if (numericise || centre || standardise)
    x <- as.integer(x)
  
  return (x)
}

convert_variable.ddate <- function(object, x, numericise = T, centre = F, standardise = F) {
  
  x <- as.Date(x)

  if (numericise || centre || standardise)
    x <- as.integer(x) - as.integer(as.Date('1999-12-31'))
  
  if(standardise) {
    x <- (x - (as.integer(object$mean) - as.integer(as.Date('1999-12-31')))) / object$sd
  } else if (centre)
    x <- x - (as.integer(object$mean) - as.integer(as.Date('1999-12-31')))
  
  return (x)
}

convert_variable.dposixt <- function(object, x, numericise = T, centre = F, standardise = F) {
  
  x <- as.POSIXct(x)

  if (numericise || centre || standardise)
    x <- as.integer(x) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "GMT"))  
  
  if(standardise) {
    x <- (x - (as.integer(object$mean) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "GMT")))) / object$sd
  } else if (centre)
    x <- x - (as.integer(object$mean) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "GMT")))
  
  return (x)
}
