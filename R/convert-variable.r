
convert_variable <- function(object, x, numericise = T, centre = F, standardise = F) {
  UseMethod("convert_variable", object)
}

convert_variable.vlogical <- function(object, x, numericise = T, centre = F, 
                                     standardise = F) {
  if (!is.vector(x)) {
    stop("x must be a vector")
  }
  if(storage.mode(x) != "logical") {
    stop("storage mode of x must be logical")
  }

  if (numericise || centre || standardise) {
    storage.mode(x) <- "integer"
  }
  
  return (x)
}

convert_variable.vinteger <- function(object, x, numericise = T, centre = F, standardise = F) {

  if (!is.vector(x)) {
    stop("x must be a vector")
  }
  if(storage.mode(x) != "integer") {
    stop("storage mode of x must be integer")
  }
  
  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre) {
    x <- x - object$mean
  }
  
  return (x)
}

convert_variable.vnumeric <- function(object, x, numericise = T, centre = F, standardise = F) {

  if (!is.vector(x)) {
    stop("x must be a vector")
  }
  if(storage.mode(x) != "double") {
    stop("storage mode of x must be double")
  }
  
  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre) {
    x <- x - object$mean
  }
  
  return (x)
}

convert_variable.vfactor <- function(object, x, numericise = T, centre = F, standardise = F) {
  
  if(!is.factor(x)) {
    stop("x must be class factor")
  }
    
  x <- factor(as.character (x), levels = object$levels)
    
  if (numericise || centre || standardise) {
    x <- as.integer(x)
  }
  
  return (x)
}

convert_variable.vdate <- function(object, x, numericise = T, centre = F, standardise = F) {
  
  if(!inherits(x,"Date")) {
    stop("x must be class Date")
  }
  
  if (numericise || centre || standardise) {
    x <- as.integer(x) - as.integer(as.Date('1999-12-31'))
  }
  
  if(standardise) {
    x <- (x - (as.integer(object$mean) - as.integer(as.Date('1999-12-31')))) / object$sd
  } else if (centre) {
    x <- x - (as.integer(object$mean) - as.integer(as.Date('1999-12-31')))
  }
  
  return (x)
}

convert_variable.vposixt <- function(object, x, numericise = T, centre = F, standardise = F) {
  
  if(!inherits(x,"POSIXt")) {
    stop("x must be class POSIXt")
  }
  
  x <- as.POSIXct(x)

  if (numericise || centre || standardise) {
    x <- as.integer(x) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "GMT"))  
  }
  
  if(standardise) {
    x <- (x - (as.integer(object$mean) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "GMT")))) / object$sd
  } else if (centre) {
    x <- x - (as.integer(object$mean) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "GMT")))
  }
  return (x)
}

convert_variable.mlogical <- function(object, x, numericise = T, centre = F, 
                                      standardise = F) {
  if (!is.matrix(x)) {
    stop("x must be a matrix")
  }
    
  if(storage.mode(x) != "logical") {
    stop("storage mode of x must be logical")
  }
  
  if (numericise || centre || standardise) {
    storage.mode(x) <- "integer"
  }
  
  return (x)
}

convert_variable.minteger <- function(object, x, numericise = T, centre = F, 
                                      standardise = F) {
  if (!is.matrix(x)) {
    stop("x must be a matrix")
  }
  
  if(storage.mode(x) != "integer") {
    stop("storage mode of x must be integer")
  }
  
  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre) {
    x <- x - object$mean
  }
  
  return (x)
}

convert_variable.mdouble <- function(object, x, numericise = T, centre = F, 
                                      standardise = F) {
  if (!is.matrix(x)) {
    stop("x must be a matrix")
  }
  
  if(storage.mode(x) != "double") {
    stop("storage mode of x must be double")
  }
  
  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre) {
    x <- x - object$mean
  }
  
  return (x)
}


convert_variable.alogical <- function(object, x, numericise = T, centre = F, 
                                      standardise = F) {
  if (length(dim(x)) != length(dim(x))) {
    stop("x must be an array of the same dimensionality as object")
  }
  
  if(storage.mode(x) != "logical") {
    stop("storage mode of x must be logical")
  }
  
  if (numericise || centre || standardise) {
    storage.mode(x) <- "integer"
  }
  
  return (x)
}

convert_variable.ainteger <- function(object, x, numericise = T, centre = F, 
                                      standardise = F) {
  if (length(dim(x)) != length(dim(x))) {
    stop("x must be an array of the same dimensionality as object")
  }
  
  if(storage.mode(x) != "integer") {
    stop("storage mode of x must be integer")
  }
  
  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre) {
    x <- x - object$mean
  }
  
  return (x)
}

convert_variable.adouble <- function(object, x, numericise = T, centre = F, 
                                     standardise = F) {
  if (length(dim(x)) != length(dim(x))) {
    stop("x must be an array of the same dimensionality as object")
  }
  
  if(storage.mode(x) != "double") {
    stop("storage mode of x must be double")
  }
  
  if(standardise) {
    x <- (x - object$mean) / object$sd
  } else if (centre) {
    x <- x - object$mean
  }
  
  return (x)
}
