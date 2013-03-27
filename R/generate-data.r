
#' @title Generate dummy data
#'
#' @description
#' Generates dummy data frame based on the variables in data.
#' If a variable is specified in range then it is represented
#' by a sequence of evenly spaced values from the minimum to the maximum 
#' of the observed values (the range) of length equal to length.out. 
#' If it is not specified in range then it is represented by the mean of the 
#' observed values. The returned data frame includes all combinations of the
#' generated values.
#' 
#' @param data the data frame of variables used to generate the dummy data frame
#' @param range a character vector of the variables in data to 
#' represent by a sequence of values
#' @param length.out the the number of values in a sequence
#' @return the dummy data frame
#' @details
#' data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10, 
#'    factor = factor(1:10), date = as.Date("2000-01-01") + 1:10,
#'    posixt = ISOdate(2000,1,1) + 1:10)
#'    
#' generate_data (data)
#' generate_data (data,range='numeric')
#' generate_data (data,range=c('date','factor'))
#' @export
generate_data <- function (data, range = NULL, length.out = 30)
{
  if (!is.data.frame(data))
    stop ("data should be class data.frame")
  if (!(is.null(range) || is.character(range)))
    stop ("range should be NULL or class character")
  if (length(length.out) != 1)
    stop ("length.out should be vector of length 1")
  if (length.out < 1)
    stop ("length.out should be greater than 0")

  range <- as.character(range)
  length.out <- as.integer(length.out)
  
  x <- range[!range %in% c(colnames (data),"")]
  if (length(x))
    message (paste("the following variables are in range but not data:", x))  
  
  dat<-list()
  for (colname in colnames(data)) {
    variable <- dvariable(data[, colname, drop=T])    
    if (colname %in% range) {
        dat[[colname]] <- generate_range(variable, length.out=length.out)        
    } else {
      dat[[colname]] <- get_mean(variable)                
    }
  }
  data <- expand.grid (dat)
  
  return (data)
}
