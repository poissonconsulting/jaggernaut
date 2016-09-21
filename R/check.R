check_string <- function (x) {
  if(!is.character(x))
    stop("x must be class character")
  
  if(!is.string(x)) {
    message("collapsing x into string")
    x <- paste0(x, collapse = "\n")
  }
  x
}

#' Check JAGS model code
#' 
#' Checks JAGS model code
#' 
#' @param x string of JAGS model code
#' @param extended flag of whether to allow extended BUGS language
#' @return Invisible flag of whether JAGS model code passes certain checks.
#' In addition, a unique warning is issued for each failed check.
#' @export
jg_check <- function (x, extended = FALSE) {
  
  assert_that(is.flag(extended) && noNA(extended))
  x <- jg_rm_comments(x)
  
  flag <- TRUE
  
  bnames <- try(jg_block_names(x))
  if (inherits(bnames, "try-error")){
    warning("unbalanced brackets")
    return (FALSE)
  } 
  
  if (!"model" %in% bnames) {
    warning("no model block")
    return (FALSE)
  }
  
  if (any(duplicated(bnames))) {
    warning("duplicated block names: ", paste_names(bnames[duplicated(bnames)], TRUE))
    flag <- FALSE
  }
  
  if(!extended) {
    anames <- c("data", "model")
  } else
    anames <- c("data", "model", "predict", "aggregate")
  
  if(any(!bnames %in% anames)) {
    warning("invalid block names: ", paste_names(bnames[!bnames %in% anames], TRUE))
    flag <- FALSE
  } else {
    fnames <- as.integer(factor(bnames, anames))
    if(is.unsorted(fnames)) {
      warning("block order must be: ", paste_names(anames, TRUE))
      flag <- FALSE
    }
  }
  nodes <- jg_vnodes(x, indices = FALSE)
  if(identical(nodes, "character(0)")) {
    warning("no nodes")
    flag <- FALSE
  } else if(any(nodes %in% jags_reserved_words())) {
    nodes <- nodes[nodes %in% jags_reserved_words()]
    warning("invalid node names '", paste(nodes, collapse = "', '"), "'")    
    flag <- FALSE
  }
  
  dists <- jg_dists(x)
  dists <- dists[!dists %in% jags_distributions()]
  if(!identical(dists, character(0))) {
    warning("invalid distributions '", paste(dists, collapse = "', '"), "'")    
    flag <- FALSE
  }
  
  funcs <- jg_funcs(x)
  funcs <- funcs[!funcs %in% jags_functions()]
  if(!identical(funcs, character(0))) {
    warning("invalid functions '", paste(funcs, collapse = "', '"), "'")    
    flag <- FALSE
  }
  
  return(invisible(flag))
}
