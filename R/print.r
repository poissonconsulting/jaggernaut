print.mcarray <- function (x) {  ndim <- length(dim(x))    xx <- x  class(xx) <- 'array'  xx <- apply(xx, 1, mean)  print(xx)    invisible(x)}print.gsmcmc <- function (x) {  cat("\nparameters\n")      print_list <- function (x) {    for (par in names(x)) {      print(par)      print(x[[par]])    }  }    print_list(x$mcmc)      cat("\nchains\n")  print(nchain(x))  cat("\nsimulations\n")  print(nsim(x))    invisible(x)}#' @title Print a JAGS model#'#' @description #' Prints a JAGS model#'   #' @param x a jmodel object to print#' @export#' @examples#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")#' print(model)print.jmodel <- function (x) {  cat("\nmodel\n")  cat(x$model)  cat("\n")    if(!is.null(x$derived)) {    cat("\nderived\n")    cat(x$derived)    cat("\n")  }  if(!is.null(x$monitor)) {    cat("\nmonitor\n")    print(x$monitor)  }    if(!is.null(x$gen_inits)) {    cat("\ngen_inits\n")    print(x$gen_inits)     }  if(!is.null(x$select)) {    cat("\nselect\n")    print(x$select)     }    if(!is.null(x$random)) {    cat("\nrandom\n")    print(x$random)  }  invisible(x)}#' @title Print a JAGS analysis#'#' @description #' Prints a JAGS analysis#'   #' @param x a janalysis object to print#' @export#' @examples#' model <- jmodel("model { bLambda ~ dunif(0,10) for (i in 1:nrow) { x[i]~dpois(bLambda) } }")#' data <- data.frame(x = rpois(100,1))#' analysis <- janalysis (model, data)#' print(analysis)print.janalysis <- function (x) {    x <- top_model(x)  print(x$model)   cat("\ndata\n")  print(head(x$data))   cat("\ninits\n")  print(x$inits)  cat("\niterations\n")  print(x$iterations)  cat("\ntime\n")  print(x$time)  print(x$mcmc)    invisible(x)}print.summary_jagr_analysis <- function (x) {  for (name in names(x)) {    cat(paste0("\n",name,":\n"))    print(x[[name]])  }}#' @title Print summary of JAGS analysis (janalysis) object#'#' @description #' Prints a summary of a JAGS analysis (janalysis) object#'   #' @param x the summary of a JAGS analysis (janalysis) object#' @exportprint.summary_janalysis <- function (x) {    for (name in names(x)) {    cat(paste0("\n",name,":\n"))    print(x[[name]])  }}