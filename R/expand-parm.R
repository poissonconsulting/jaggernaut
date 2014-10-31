expand_parm <- function (object, parm, drop_suffixed = FALSE) {
  
  assert_that(is.jagr_analysis(object))
  assert_that(is.character(parm) && noNA(parm) && not_empty(parm))
  assert_that(is.flag(drop_suffixed) && noNA(drop_suffixed))
    
  parm <- sort(unique(parm))
    
  mcmc <- as.mcmc.list (as.jagr_chains(object))
  
  vars <- varnames(mcmc)
  
  vars <- sort(vars)
  
  get_svars <- function (x) {
    x <- strsplit(x, split = "[", fixed = T)
    x <- delist(x)[1]
    return (x)
  }
  
  svars <- sapply(vars, get_svars)
  
  all <- NULL
  fixed <- NULL
  random <- NULL
  
  if("all" %in% parm) {
    all <- svars
  }
  if ("fixed" %in% parm) {
    fixed <- svars[!svars %in% random_effects(object, names = TRUE)]
  } 
  if ("random" %in% parm) {
    random <- svars[svars %in% random_effects(object, names = TRUE)]
  }
  pars <- svars[svars %in% parm]
  
  pars <- sort(unique(c(all,fixed,random,pars)))
  
  vars <- vars[svars %in% pars]
  
  if(drop_suffixed && length(monitor(object)) > 2)
    vars <- vars[svars %in% monitor(object, drop_suffixed = TRUE)]
  
  vars
}
