#' Fix JAGS model code
#' 
#' Fixes JAGS model code. Currently only
#' drops blocks with invalid names and reorders blocks 
#' (JAGS requires blocks to be ordered).
#' 
#' @param x string of JAGS model code
#' @param extended flag of whether to allow extended BUGS language.
#' @return If successful a string of the fixed code. 
#' If fails issues warning and returns FALSE.
#' @export
jg_fix <- function (x, extended = FALSE) {
  
  assert_that(is.flag(extended) && noNA(extended))
  x <- jg_rm_comments(x)
  
  if(!extended) {
    anames <- c("data", "model")
  } else
    anames <- c("data", "model", "predict", "aggregate")
  
  blocks <- try(jg_blocks(x))
  if (inherits(blocks, "try-error")){
    warning("unbalanced brackets")
    return (FALSE)
  } 
  
  if (!"model" %in% names(blocks)) {
    warning("no model block")
    return (FALSE)
  }
  
  blocks <- blocks[names(blocks) %in% anames]
  fnames <- as.integer(factor(names(blocks), levels = anames))
  blocks <- blocks[order(fnames)]
  paste_blocks(blocks)
}
  
