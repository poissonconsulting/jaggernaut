
#' @title Convert data
#'
#' @description
#' Convert (by numericising, centring or standardising) the variables in a 
#' data frame into integer or numeric values ready for analysis by 
#' JAGS, OpenBUGS or WinBUGS.
#' 
#' Numericising, centring and standardising represent increasing conversion of a
#' variable. A variable is first converted into a integer value (numericising).
#' Next the mean is substracted from all the values (centring). Finally all the values
#' are divided by the standard deviation (standardising).
#' 
#' Numericising has no effect on numeric and integer values 
#' and Date and POSIXt variables are numericised by converting to integer values, 
#' and subtracting the integer value of 1999-12-31 or 1999-12-31 23:59:59 GMT, respectively.
#' 
#' Centring has no affect on factors and Date and POSIXt values are subtracted from
#' the rounded mean. Similarly, standardising has no affect on factors and centred 
#' Date and POSIXt values are divided by their respective standard deviation.
#'
#'
#' @param data the data frame or data list on which the conversion is based
#' @param numericise the variables to numericise
#' @param centre the variables to centre
#' @param standardise the variables to standardise
#' @param dat the data frame or data list to convert. If dat is NULL data is 
#' converted
#' @return the converted data frame
#' @details
#' data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10, 
#' factor = factor(1:10), date = as.Date("2000-01-01") + 1:10,
#' posixt = ISOdate(2000,1,1) + 1:10)
#' convert_data(data)
#' convert_data(data, centre = TRUE)
#' convert_data(data, standardise = TRUE)
#' convert_data(data, numericise = FALSE, standardise = "date")
#' convert_data(data, numericise = FALSE, centre = "date", standardise = "date")
convert_data <- function (data, numericise = TRUE, centre = FALSE, standardise = FALSE, dat = NULL)
{
  if (!(is.data.frame(data) || is_data_list(data))) 
    stop ("object must be class data.frame or a list of vectors, matrices and arrays")
  if (!(is.logical(numericise) || is.character(numericise) || is.null(numericise)))
    stop ("numericise must be class logical, character or NULL") 
  if (!(is.logical(centre) || is.character(centre) || is.null(centre)))
    stop ("centre must be class logical, character or NULL") 
  if (!(is.logical(standardise) || is.character(standardise) || is.null(standardise)))
    stop ("standardise must be class logical, character or NULL") 
  if (!(is.null(dat) || (is.data.frame(data) && is.data.frame(dat)) || (is_data_list(data) && is_data_list(dat))))
    stop ("dat must be NULL or class data.frame or a data list")
  
  if (is.null(dat)) {
    dat <- data
  }
  
  names_data <- names_data(data)
  names_dat <- names_data(dat)
  
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
    message(paste("the following variables are not in data:", x))
  
  x <- names_dat[!names_dat %in% names_data]
  if (length(x))
    message(paste("the following variables are in dat but not data: ", x))
  
  for(name in names_data) {
      variable <- dvariable(data[[name]])      
    if (name %in% names_dat) {
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


