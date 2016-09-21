convert_variable <- function(object, x, numericise = TRUE, 
                             centre = FALSE, standardise = FALSE) {
  UseMethod("convert_variable", object)
}

convert_variable.vlogical <- function(object, x, numericise = TRUE, 
                                      centre = FALSE, standardise = FALSE) {

  assert_that(is.vector(x))
  assert_that(storage.mode(x) == "logical")
  
  if (numericise || centre || standardise)
    storage.mode(x) <- "integer"
  
  return (x)
}

convert_variable.vinteger <- function(object, x, numericise = TRUE, 
                                      centre = FALSE, standardise = FALSE) {

  assert_that(is.vector(x))
  assert_that(storage.mode(x) == "integer")
  
  if(standardise) {
    x <- (x - get_mean(object)) / get_sd(object)
  } else if (centre)
    x <- x - get_mean(object)
  
  return (x)
}

convert_variable.vnumeric <- function(object, x, numericise = TRUE, 
                                      centre = FALSE, standardise = FALSE) {

  assert_that(is.vector(x))
  assert_that(storage.mode(x) == "double")
  
  if(standardise) {
    x <- (x - get_mean(object)) / get_sd(object)
  } else if (centre)
    x <- x - get_mean(object)
  
  return (x)
}

convert_variable.vfactor <- function(object, x, numericise = TRUE, 
                                     centre = FALSE, standardise = FALSE) {

  assert_that(is.factor(x))
    
  x <- factor(as.character (x), levels = object$levels)
    
  if (numericise || centre || standardise)
    x <- as.integer(x)
  
  return (x)
}

convert_variable.vdate <- function(object, x, numericise = TRUE, 
                                   centre = FALSE, standardise = FALSE) {
  assert_that(is.date(x))
  
  if(standardise) {
    x <- (date2integer(x) - date2integer(get_mean(object))) / get_sd(object)
  } else if (centre) {
    x <- date2integer(x) - date2integer(get_mean(object))
  } else if (numericise)
    x <- date2integer(x)
  
  return (x)
}

convert_variable.vposixt <- function(object, x, numericise = TRUE, 
                                     centre = FALSE, standardise = FALSE) {
  
  assert_that(is.POSIXt(x))
  
  x <- as.POSIXct(x)

  if (numericise || centre || standardise)
    x <- as.integer(x) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "UTC"))  
  
  if(standardise) {
    x <- (x - (as.integer(get_mean(object)) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "UTC")))) / get_sd(object)
  } else if (centre)
    x <- x - (as.integer(get_mean(object)) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "UTC")))

  return (x)
}

convert_variable.mlogical <- function(object, x, numericise = TRUE, 
                                      centre = FALSE, standardise = FALSE) {
  assert_that(is.matrix(x))
  assert_that(storage.mode(x) == "logical")
  
  if (numericise || centre || standardise)
    storage.mode(x) <- "integer"
  
  return (x)
}

convert_variable.minteger <- function(object, x, numericise = TRUE, 
                                      centre = FALSE, standardise = FALSE) {
  assert_that(is.matrix(x))
  assert_that(storage.mode(x) == "integer")
  
  if(standardise) {
    x <- (x - get_mean(object)) / get_sd(object)
  } else if (centre)
    x <- x - get_mean(object)
  
  return (x)
}

convert_variable.mdouble <- function(object, x, numericise = TRUE, 
                                     centre = FALSE, standardise = FALSE) {
  assert_that(is.matrix(x))
  assert_that(storage.mode(x) == "double")
  
  if(standardise) {
    x <- (x - get_mean(object)) / get_sd(object)
  } else if (centre)
    x <- x - get_mean(object)
  
  return (x)
}


convert_variable.alogical <- function(object, x, numericise = TRUE, 
                                      centre = FALSE, standardise = FALSE) {
  
  assert_that(length(dim(x)) == length(dim(x)))
  assert_that(storage.mode(x) == "logical")
  
  if (numericise || centre || standardise)
    storage.mode(x) <- "integer"
  
  return (x)
}

convert_variable.ainteger <- function(object, x, numericise = TRUE, 
                                      centre = FALSE, standardise = FALSE) {

  assert_that(length(dim(x)) == length(dim(x)))
  assert_that(storage.mode(x) == "integer")
  
  if(standardise) {
    x <- (x - get_mean(object)) / get_sd(object)
  } else if (centre)
    x <- x - get_mean(object)
  
  return (x)
}

convert_variable.adouble <- function(object, x, numericise = TRUE, 
                                     centre = FALSE, standardise = FALSE) {

  assert_that(length(dim(x)) == length(dim(x)))
  assert_that(storage.mode(x) == "double")
  
  if(standardise) {
    x <- (x - get_mean(object)) / get_sd(object)
  } else if (centre)
    x <- x - get_mean(object)
  
  return (x)
}
