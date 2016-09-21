#' Remove comments in JAGS model code
#' 
#' Removes comments in JAGS model code.
#' A comment is everything from a # to the end
#' of a line of the end of the string.
#' 
#' @param x string of JAGS model code
#' @return JAGS model code stripped of comments as a string.
#' @export
jg_rm_comments <- function (x) {
  x <- check_string(x)
  
  gsub("#[^\n]*(?=($|[\n]))", "", x, perl = TRUE)
}
