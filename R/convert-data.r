#' Convert Data
#'
#' Convert (by numericising, centring or standardising) the variables of a 
#' data.frame or elements of a list into a list of integer or numeric values 
#' ready for analysis by 
#' JAGS, OpenBUGS or WinBUGS.
#' 
#' Numericising, centring and standardising represent increasing conversion of a
#' variable. A variable is first converted into a numeric value (numericising).
#' Next the mean is subtracted from all the values (centring). Finally all the 
#' values are divided by the standard deviation (standardising).
#' 
#' Numericising has no effect on numeric and integer values. Factors are 
#' numericized
#' by converting to an integer.
#' Date and POSIXt variables are numericised by converting to integer values
#' and subtracting the integer value of 1999-12-31 or 1999-12-31 23:59:59 UTC, 
#' respectively.
#' 
#' Centring has no affect on factors while integer, Date and POSIXt values are subtracted 
#' from the rounded mean. 
#' Similarly, standardising has no affect on factors while centred integer, 
#' Date and POSIXt values are divided by their respective standard deviation.
#'
#' @param data the data frame or list of data on which the conversion is based
#' @param numericise a logical scalar or a character vector of the variables to 
#' numericise
#' @param centre a logical scalar or a character vector of the variables 
#' to centre
#' @param standardise a logical scalar or a character vector of the variables
#' to standardise
#' @param dat the data frame or data list to convert. If dat is NULL the dataset 
#' passed as the data argument is converted
#' @return The converted data frame
#' @examples
#' data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10, 
#' factor = factor(1:10), date = as.Date("2000-01-01") + 1:10,
#' posixt = ISOdate(2000,1,1) + 1:10)
#' convert_data(data)
#' convert_data(data, centre = TRUE)
#' convert_data(data, standardise = TRUE)
#' convert_data(data, numericise = FALSE, standardise = "date")
#' convert_data(data, numericise = FALSE, centre = "date", standardise = "date")
#' @export
convert_data <- function (data, numericise = TRUE, centre = FALSE, 
                          standardise = FALSE, dat = NULL) {
  
  assert_that(is_convertible_data_frame(data) || is_convertible_data_list(data))
  assert_that(is.null(dat) || is_convertible_data_frame(dat) ||
                is_convertible_data_list(dat))
  assert_that(is.null(dat) || (is_convertible_data_frame(data) && 
                                 is_convertible_data_frame(dat)) ||
                (is_convertible_data_list(data) && is_convertible_data_list(dat)))
  
  assert_that(is.flag(numericise) || is.character(numericise) || is.null(numericise))
  assert_that(is.flag(centre) || is.character(centre) || is.null(centre))
  assert_that(is.flag(standardise) || is.character(standardise) || is.null(standardise))

  
  if (is.null(dat))
    dat <- data
  
  names_data <- names(data)
  names_dat <- names(dat)
  
  if (is.logical(numericise)) {
    if (numericise) {
      numericise <- names_data
    } else
      numericise <- NULL
  }
  if (is.logical(centre)) {
    if (centre) {
      centre <- names_data
    } else
      centre <- NULL
  }
  if (is.logical(standardise)) {
    if (standardise) {
      standardise <- names_data
    } else
      standardise <- NULL
  }  
  
  numericise <- numericise[!numericise %in% c(centre, standardise)]
  centre <- centre[!centre %in% standardise]
  
  all <- c(numericise, centre, standardise)
  
  x <- all[!all %in% names_data]
  if(length(x))
    message(paste(c("the following variables are not in data:", x),collapse = " "))
  
  x <- names_dat[!names_dat %in% names_data]
  if (length(x))
    message(paste(c("the following variables are in dat but not data: ", x), collapse = " "))
  
  con_terms <- list()
  
  for(name in names_data) {
    if (name %in% names_dat) {

      variable <- variable(data[[name]])       
      
      dat[[name]] <- convert_variable(
        variable, 
        dat[[name]], 
        numericise = name %in% numericise,
        centre = name %in% centre,
        standardise = name %in% standardise
      )
    }
  }
  return (dat)
}
