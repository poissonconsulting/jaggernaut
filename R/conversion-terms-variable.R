conversion_terms_variable <- function(object, x, centre = FALSE, standardise = FALSE) {
  UseMethod("conversion_terms_variable", object)
}

conversion_terms_variable.vlogical <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(is.vector(x))
  assert_that(storage.mode(x) == "logical")
  
  mean <- NA_real_
  sd <- NA_real_
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.vinteger <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(is.vector(x))
  assert_that(storage.mode(x) == "integer")
  
  mean <- NA_real_
  sd <- NA_real_
  
  if(standardise) {
    mean <- get_mean(object)
    sd <-  get_sd(object)
  } else if (centre)
    mean <- get_mean(object)
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.vnumeric <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(is.vector(x))
  assert_that(storage.mode(x) == "double")
  
  mean <- NA_real_
  sd <- NA_real_
  
  if(standardise) {
    mean <- get_mean(object)
    sd <-  get_sd(object)
  } else if (centre)
    mean <- get_mean(object)
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.vfactor <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(is.factor(x))
  
  mean <- NA_real_
  sd <- NA_real_
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.vdate <- function(object, x, centre = FALSE, standardise = FALSE) {
  assert_that(is.date(x))
  
  mean <- NA_real_
  sd <- NA_real_
  
  if(standardise) {
    mean <- date2integer(get_mean(object))
    sd <- get_sd(object)    
  } else if (centre)
    mean <- date2integer(get_mean(object))
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.vposixt <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(is.POSIXt(x))
  
  x <- as.POSIXct(x)
  
  mean <- NA_real_
  sd <- NA_real_
    
  if(standardise) {
    mean <- get_mean(object)
    sd <- get_sd(object)    
  } else if (centre)
    mean <- get_mean(object)
  
  mean <- as.integer(mean) - as.integer(as.POSIXct('1999-12-31 23:59:59',tz = "UTC"))
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.mlogical <- function(object, x, centre = FALSE, standardise = FALSE) {
  assert_that(is.matrix(x))
  assert_that(storage.mode(x) == "logical")
  
  mean <- NA_real_
  sd <- NA_real_
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.minteger <- function(object, x, centre = FALSE, standardise = FALSE) {
  assert_that(is.matrix(x))
  assert_that(storage.mode(x) == "integer")
  
  mean <- NA_real_
  sd <- NA_real_
  
  if(standardise) {
    mean <- get_mean(object)
    sd <-  get_sd(object)
  } else if (centre)
    mean <- get_mean(object)
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.mdouble <- function(object, x, centre = FALSE, standardise = FALSE) {
  assert_that(is.matrix(x))
  assert_that(storage.mode(x) == "double")
  
  mean <- NA_real_
  sd <- NA_real_
  
  if(standardise) {
    mean <- get_mean(object)
    sd <-  get_sd(object)
  } else if (centre)
    mean <- get_mean(object)
  
  c(mean = mean, sd = sd)
}


conversion_terms_variable.alogical <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(length(dim(x)) == length(dim(x)))
  assert_that(storage.mode(x) == "logical")
  
  mean <- NA_real_
  sd <- NA_real_
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.ainteger <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(length(dim(x)) == length(dim(x)))
  assert_that(storage.mode(x) == "integer")
  
  mean <- NA_real_
  sd <- NA_real_
  
  if(standardise) {
    mean <- get_mean(object)
    sd <-  get_sd(object)
  } else if (centre)
    mean <- get_mean(object)
  
  c(mean = mean, sd = sd)
}

conversion_terms_variable.adouble <- function(object, x, centre = FALSE, standardise = FALSE) {
  
  assert_that(length(dim(x)) == length(dim(x)))
  assert_that(storage.mode(x) == "double")
  
  mean <- NA_real_
  sd <- NA_real_
  
  if(standardise) {
    mean <- get_mean(object)
    sd <-  get_sd(object)
  } else if (centre)
    mean <- get_mean(object)
  
  c(mean = mean, sd = sd)
}
