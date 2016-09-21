#' Extend JAGS model code
#' 
#' Extends JAGS model code, i.e., converts it from
#' code in the JAGS dialect of the BUGS language to 
#' code in the JAGS dialect of the extended BUGS language.
#' In addition to the data and model blocks 
#' the extended BUGS languge allows a predict and/or aggregate block.
#' The predict block is used to estimate new expected 
#' values while the aggregate block is used to estimate metrics for the entire
#' dataset. The former is useful for exploring the models predictions and 
#' calculation of residuals while the latter is useful for posterior predictive
#' checking among other things. The advantages of separating the predictive
#' and aggregative code from the model code are 
#' 1) the model code runs quicker; 
#' 2) the predictive code can be run with user provided datasets to generate
#' predictions under unobserved combinations of data;
#' 3) the predictive or aggregative code can be changed without requiring the
#' model code to be rerun.
#' 
#' In order for the current function to extend some JAGS code it
#' requires the predictive and/or aggregative code segments to be identified
#' line by line using a #' comment followed by one or more of the characters
#' MPAmpa to indicate to which 
#' blocks the line of code belongs.
#' 
#' The syntax is as follows: the 
#' first line of code, i.e. model \{ is always #' M (it doesn't require setting)
#' which indicates that it and 
#' all subsequent lines should only be included in the model block.
#' This remains the case until the mode changes to for example #' MP which 
#' indicates that the current line and all subsequent lines should be included
#' in the model and predictive blocks unless the mode changes again to for example
#' #' A to indicate inclusion in the aggregative code block only.

#'  The only exception
#' is if a line of code contains one or more of the lower case characters
#' m, p and/or a. 
#' In this situation the mode is temporarily ignored and just the lower case
#' characters are used to determine block inclusion for the current line only. 
#' It is worth noting that the block mode can be set and temporarily ignored in
#' the same line. Thus #' Pm indicates change mode to predictive but only
#' include the current line in the model block. The order of the characters 
#' doesn't matter but they must form the first word after #' 
#' which must be the first hash character on the line. Duplicates are ignored
#' and a warning is issued for any characters other than MPAmpa - the 
#' extraneous characters are ignored. 
#' 
#' @param x string of JAGS model code
#' @return String of model code in JAGS dialect of extended BUGS language
#' or FALSE if fails (in which case also issues an error).
#' @export
jg_extend <- function (x) {
  
}
