#' @title New data
#'
#' @description
#' Generates a data.frame that can then be passed to a model to predict the 
#' effects of particular variables with the other variables held constant.
#' 
#' @details
#' Unless a variable is named in sequence it is fixed at its base value. 
#' A continuous variable's base value is its mean value
#' of the same class, i.e., an integer variable's base value is its rounded mean.
#' A logical variable's base value is FALSE while a factor's is
#' its first level. The only exception to this if a variable is named in observed 
#' in which case its observed base is its closest observed value to its base value.
#' 
#' Alternatively if a variable is named in sequence then a sorted sequence of 
#' unique values of the same class 
#' from the minimum to the maximum of the observed values with a 
#' length of up to length_out (by default 30) is generated. If length_out is less
#' than the number of possible values then in the case of a continous variable the
#' values are as evenly distributed as possible while in the case of a factor only
#' the first length_out levels are selected. If a variable is also
#' named in observed then only observed values are permitted in the sequence and
#' continuous variables may no longer be as evenly spaced as possible.
#' 
#' @param data data.frame of variables from which the data.frame will be generated
#' @param sequence character vector of the variables in data to 
#' represent by a sequence of values or NULL (the default)
#' @param observed character vector of the variables in data to 
#' represent by a sequence of observed values or NULL (the default)
#' @param length_out integer scalar of the maximum number of values in a sequence
#' @return The generated data.frame which can then be passed to a model for the
#' purpose of estimating the effects of particular variables.
#' @examples
#' data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10, 
#'    factor = factor(1:10), date = as.Date("2000-01-01") + 1:10,
#'    posixt = ISOdate(2000,1,1) + 1:10)
#'    
#' new_data(data)
#' new_data(data, observed = c("numeric"))
#' new_data(data, sequence = "numeric")
#' new_data(data, sequence = c("date", "factor"))
#' new_data(data, sequence = c("numeric"), observed = c("numeric"))
#' 
#' @export
new_data <- function (data, sequence = NULL, observed = NULL, length_out = 30) {
  
  assert_that(is.data.frame(data))
  assert_that(is.null(sequence) || is.character(sequence))
  assert_that(is.null(observed) || is.character(observed))
  assert_that(is.count(length_out) && noNA(length_out))
  
  x <- sequence[!sequence %in% colnames (data)]
  if (length(x))
    warning("the following variables are in sequence but not data: ", x)
  
  x <- observed[!observed %in% colnames (data)]
  if (length(x))
    warning("the following variables are in observed but not data: ", x)
  
  dat<-list()
  for (colname in colnames(data)) {
    variable <- variable(data[[colname]])
    dat[[colname]] <- get_sequence(variable, 
                                   observed = colname %in% observed,
                                  length_out = ifelse(colname %in% sequence, 
                                                      length_out, 1))
  }
  data <- expand.grid (dat)
  
  return (data)
}
