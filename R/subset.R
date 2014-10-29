subset.mcarray <- function (x, sample = NULL, chain = NULL, ...) {
  
  if(is.null(sample) & is.null(chain))
    return (x)
  
  if(!(is.null(sample))) {
    if(!is.numeric(sample))
      stop("sample must be class integer")
    if(length(sample) == 0)
      stop("sample must at least one value")
    if(any(is.na(sample)))
      stop("sample must not contain missing values")
    if(max(sample) > nsamples(x) /nchains(x))
      stop("sample must not exceed the number of MCMC samples per chain")
    
    sample <- as.integer(sample)
    sample <- sort(unique(sample))
  } else {
    sample <- 1:(nsamples(x)/nchains(x))
  }
  
  if(!(is.null(chain))) {
    if(!is.numeric(chain))
      stop("chain must be class integer")
    if(length(chain) == 0)
      stop("chain must at least one value")
    if(any(is.na(chain)))
      stop("chain must not contain missing values")
    if(max(chain) > nchains(x))
      stop("chain must not exceed the number of chains")
    
    chain <- as.integer(chain)
    chain <- sort(unique(chain))
  } else {
    chain <- 1:nchains(x)
  }
  
  ndim <- length (dim(x))
  dnames <- dimnames(dim(x))
  
  commas <- paste0(rep(',',ndim - 2),collapse = "")   
  sample <- paste0("c(",paste0(sample,collapse = ","),")")
  chain <- paste0("c(",paste0(chain,collapse = ","),")")
  cmd <- paste0('x<-x[', commas ,sample, ',' ,chain, ',drop=F]')
  eval(parse(text = cmd))
  
  dimnames(x) <- dnames
  class(x) <- 'mcarray'
  
  return (x)
}

subset_mcarray <- function (x, sample = NULL, chain = NULL, ...) {
  stopifnot(is.mcarray(x))
  return (subset(x, sample = sample, chain = chain, ...))
}

subset.jagr_chains <- function (x, sample = NULL, chain = NULL) {
  
  samples(x) <- lapply(samples(x), FUN = subset_mcarray, sample=sample, chain=chain)
  
  jags(x) <- jags(x)[chain]
  
  return (x)
}
#' @title Subset a JAGS model
#'
#' @description
#' Subset a JAGS model x.  
#' 
#' @param x a jags_model x.
#' @param model an integer or character vector specifying the jags models to select. 
#' @param ... other arguments passed to generic function.
#' @return the subsetted jags_analysis x
#' @seealso \code{\link{subset}} and \code{\link{jags_analysis}}
#' @method subset jags_model
#' @export 
subset.jags_model <- function (x, model = 1:nmodels(x), ...) {   
    
  assert_that((is.numeric(model) || is.character(model)) && noNA(model) && not_empty(model))
  
  if(is.numeric(model)) {    
    assert_that(min(model) >= 1 && max(model) <= nmodels(x))    
    
    models(x) <- models(x)[sort(unique(floor(model)))]
    return (x)
  }
  
  assert_that(all(model %in% model_name(x, reference = TRUE)))
  
  models(x) <- models(x)[model_name(x, reference = TRUE) %in% model]
  x  
}

#' @title Subset a JAGS analysis
#'
#' @description
#' Subset a JAGS analysis x.  
#' 
#' @param x a jags_analysis x.
#' @param model an integer or character vector specifying the jags models to select. 
#' @param ... other arguments passed to generic function.
#' @return the subsetted jags_analysis x
#' @seealso \code{\link{subset}} and \code{\link{jags_analysis}} 
#' @method subset jags_analysis
#' @export 
subset.jags_analysis <- function (x, model = 1:nmodel(x), ...) {   
  
  assert_that((is.numeric(model) || is.character(model)) && noNA(model) && not_empty(model))
  
  if(is.numeric(model)) {    
    assert_that(min(model) >= 1 && max(model) <= nmodels(x))    
    
    analyses(x) <- analyses(x)[sort(unique(floor(model)))]
    return (x)
  }
  
  assert_that(all(model %in% model_name(x, reference = TRUE)))
  
  analyses(x) <- analyses(x)[model_name(x, reference = TRUE) %in% model]
  x 
}
