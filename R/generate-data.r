#' @title Generate data
#'
#' @description
#' Generates a data.frame that can then be passed to a model to predict the 
#' effects of particular variables with the other variables held constant.
#' 
#' @details
#' Unless a variable is named in range it is fixed at its base value. 
#' A continuous variable's base value is its mean value
#' of the same class, i.e., an integer variable's base value is its rounded mean.
#' A logical variable's base value is FALSE while a factor's is
#' its first level. 
#' 
#' Alternatively if a variable is named in range then a sorted sequence of 
#' unique values of the same class 
#' from the minimum to the maximum of the observed values with a 
#' length of up to length_out (by default 30) is generated. If length_out is less
#' than the number of possible values then in the case of a continous variable the
#' values are as evenly distributed as possible while in the case of a factor only
#' the first length_out levels are selected.
#' 
#' @param data data.frame of variables from which the data.frame will be generated
#' @param range character vector of the variables in data to 
#' represent by a sequence of values or NULL (the default)
#' @param length_out integer scalar of the maximum number of values in a sequence
#' @return The generated data.frame which can then be passed to a model for the
#' purpose of estimating the effects of particular variables.
#' @examples
#' data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10, 
#'    factor = factor(1:10), date = as.Date("2000-01-01") + 1:10,
#'    posixt = ISOdate(2000,1,1) + 1:10)
#'    
#' generate_data (data)
#' generate_data (data, range = "numeric")
#' generate_data (data, range = c("date", "factor"))
#' generate_data (data, range = c("numeric"))
#' 
#' @export
generate_data <- function (data, range = NULL, length_out = 30) {
  
  warning("generate_data is deprecated in datalist 0.3 for new_data")
  
  return (new_data(data, sequence = range, length_out = length_out))
}
