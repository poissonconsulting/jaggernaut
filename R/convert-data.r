
# ' @title Convert data
# '
# ' @description
# ' Convert (by numericising, centring or standardising) the variables in a 
# ' data frame into integer or numeric values ready for analysis by 
# ' JAGS, OpenBUGS or WinBUGS.
# ' 
# ' Numericising, centring and standardising represent increasing conversion of a
# ' variable. A variable is first converted into a integer value (numericising).
# ' Next the mean is substracted from all the values (centring). Finally all the values
# ' are divided by the standard deviation (standardising).
# ' 
# ' Numericising has no effect on numeric and integer values 
# ' and Date and POSIXt variables are numericised by converting to integer values, 
# ' and subtracting the integer value of 1999-12-31 or 1999-12-31 23:59:59 GMT, respectively.
# ' 
# ' Centring has no affect on factors and Date and POSIXt values are subtracted from
# ' the rounded mean. Similarly, standardising has no affect on factors and centred 
# ' Date and POSIXt values are divided by their respective standard deviation.
# '
# '
# ' @param data the data frame on which the conversion is based
# ' @param numericise the variables to numericise
# ' @param centre the variables to centre
# ' @param standardise the variables to standardise
# ' @param dat the data frame to convert. If dat is NULL data is 
# ' converted
# ' @return the converted data frame
# ' @details
# ' data <- data.frame(numeric = 1:10 + 0.1, integer = 1:10, 
# ' factor = factor(1:10), date = as.Date("2000-01-01") + 1:10,
# ' posixt = ISOdate(2000,1,1) + 1:10)
# ' convert_data(data)
# ' convert_data(data, centre = TRUE)
# ' convert_data(data, standardise = TRUE)
# ' convert_data(data, numericise = FALSE, standardise = "date")
# ' convert_data(data, numericise = FALSE, centre = "date", standardise = "date")
convert_data <- function (data, numericise = TRUE, centre = FALSE, standardise = FALSE, dat = NULL)
{
  if (!is.data.frame(data)) 
    stop ("object must be class data.frame")
  if (!(is.logical(numericise) || is.character(numericise) || is.null(numericise)))
    stop ("numericise must be class logical, character or NULL") 
  if (!(is.logical(centre) || is.character(centre) || is.null(centre)))
    stop ("centre mmust be class logical, character or NULL") 
  if (!(is.logical(standardise) || is.character(standardise) || is.null(standardise)))
    stop ("standardise must be class logical, character or NULL") 
  if (!(is.null(dat) || is.data.frame(dat)))
    stop ("dat must be NULL or class data.frame")
  
  if (is.logical(numericise)) {
    if (numericise) {
      numericise <- colnames(data)
    } else
      numericise <- NULL
  }
  if (is.logical(centre)) {
    if (centre) {
      centre <- colnames(data)
    } else
      centre <- NULL
  }
  if (is.logical(standardise)) {
    if (standardise) {
      standardise <- colnames(data)
    } else
      standardise <- NULL
  }  
  
  numericise <- numericise[!numericise %in% c(centre, standardise)]
  centre <- centre[!centre %in% standardise]
  
  all <- c(numericise, centre, standardise)

  if (is.null(dat))
    dat <- data
  
  x <- all[!all %in% colnames (data)]
  if(length(x))
    message(paste("the following variables are not in data:", x))
  
  x <- colnames(dat)[!colnames(dat) %in% colnames (data)]
  if (length(x))
    message(paste("the following variables are in dat but not data: ", x))
  
  for(colname in colnames(data)) {
    variable <- dvariable(data[, colname, drop=T])
    if (colname %in% colnames(dat)) {
      dat[,colname] <- convert_variable(
        variable, 
        dat[,colname], 
        numericise = colname %in% numericise,
        centre = colname %in% centre,
        standardise = colname %in% standardise
      )
    }
  }
  return (dat)
}


