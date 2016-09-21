#' Conversion Terms
#' 
#' Get the mean and standard deviation for centred and standardised
#' variables. 
#'
#' @param data the data frame or list of data
#' @param centre a logical scalar or a character vector of the variables 
#' to get the mean for
#' @param standardise a logical scalar or a character vector of the variables
#' to get the mean and standard deviation for
#' @param dat the data frame or data list to convert. If dat is NULL the dataset 
#' passed as the data argument is converted
#' @return A matrix of the means and standard deviations for 
#' the centred and standardised variables
#' @seealso \code{\link{convert_data}}
#' @examples
#' data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10, 
#' factor = factor(1:10), date = as.Date("2000-01-01") + 1:10,
#' posixt = ISOdate(2000,1,1) + 1:10)
#' conversion_terms(data)
#' conversion_terms(data, centre = TRUE)
#' conversion_terms(data, standardise = TRUE)
#' conversion_terms(data, standardise = "date")
#' conversion_terms(data, centre = "date", standardise = "date")
#' @export
conversion_terms <- function (data, centre = FALSE, standardise = FALSE, dat = NULL) {
  
  assert_that(is_convertible_data_frame(data) || is_convertible_data_list(data))
  assert_that(is.null(dat) || is_convertible_data_frame(dat) ||
                is_convertible_data_list(dat))
  assert_that(is.null(dat) || (is_convertible_data_frame(data) && 
                                 is_convertible_data_frame(dat)) ||
                (is_convertible_data_list(data) && is_convertible_data_list(dat)))
  
  assert_that(is.flag(centre) || is.character(centre) || is.null(centre))
  assert_that(is.flag(standardise) || is.character(standardise) || is.null(standardise))
  
  if (is.null(dat))
    dat <- data
  
  names_data <- names(data)
  names_dat <- names(dat)
  
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
  
  centre <- centre[!centre %in% standardise]
  
  all <- c(centre, standardise)
  
  x <- all[!all %in% names_data]
  if(length(x))
    message(paste(c("the following variables are not in data:", x),collapse = " "))
  
  x <- names_dat[!names_dat %in% names_data]
  if (length(x))
    message(paste(c("the following variables are in dat but not data: ", x), collapse = " "))
  
  cterms <- list()
  
  for(name in names_data) {
    if (name %in% names_dat) {
        
      variable <- variable(data[[name]])      
      cterms[[name]] <- conversion_terms_variable(
        variable, 
        dat[[name]], 
        centre = name %in% centre,
        standardise = name %in% standardise
      )
    }
  }
  t(as.matrix(as.data.frame(cterms)))
}
