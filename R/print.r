print.mcarray <- function (x, ...) {  ndim <- length(dim(x))    xx <- x  class(xx) <- 'array'  xx <- apply(xx, 1, mean)  print(xx, ...)    invisible(x)}print.gsmcmc <- function (x, ...) {  cat("\nparameters\n")      print_list <- function (x) {    for (par in names(x)) {      print(par)      print(x[[par]])    }  }    print_list(x$mcmc)      cat("\nchains\n")  print(nchain(x))  cat("\nsimulations\n")  print(nsim(x))    invisible(x)}#' @method print jags_model#' @exportprint.jags_model <- function (x, ...) {  cat("\nmodel\n")  cat(x$model)  cat("\n")    if(!is.null(x$derived)) {    cat("\nderived\n")    cat(x$derived)    cat("\n")  }  if(!is.null(x$monitor)) {    cat("\nmonitor\n")    print(x$monitor)  }    if(!is.null(x$gen_inits)) {    cat("\ngen_inits\n")    print(x$gen_inits)     }  if(!is.null(x$select)) {    cat("\nselect\n")    print(x$select)     }    if(!is.null(x$random)) {    cat("\nrandom\n")    print(x$random)  }  invisible(x)}#' @method print jags_analysis#' @exportprint.jags_analysis <- function (x, ...) {    x <- top_model(x)  print(x$model)   cat("\ndata\n")  print(head(x$data))   cat("\ninits\n")  print(x$inits)  cat("\niterations\n")  print(x$iterations)  cat("\ntime\n")  print(x$time)  print(x$mcmc)    invisible(x)}print.summary_jagr_analysis <- function (x, ...) {  for (name in names(x)) {    cat(paste0("\n",name,":\n"))    print(x[[name]])  }}#' @method print summary_jags_analysis#' @exportprint.summary_jags_analysis <- function (x, ...) {    for (name in names(x)) {    cat(paste0("\n",name,":\n"))    print(x[[name]])  }}