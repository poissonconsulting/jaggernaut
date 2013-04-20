
.opts_jagr0_def <- list(
  convergence = 1.1, 
  n_chains = 3,
  n_resamples = 3,
  parallel_chains = .Platform$OS.type!="windows", 
  parallel_models = FALSE,
  quiet = FALSE
)

#' @title Get and set jaggernaut options
#'
#' @description
#' Queries and sets options for JAGS analyses.
#' @param ... options can be defined using \code{name = value} or by passing a list
#' of such tagged values.
#' @details
#' The function \code{options_jaggernaut()}, which can aso be invoked using the alias
#' \code{opts_jagr0()}, behaves just like the \code{options()}
#'  function in the 
#' base library, with the additional feature that 
#' \code{opts_jagr0(default=TRUE)}
#'  will 
#' reset all options to the default values.
#' 
#' Available options are
#' 
#' \describe{
#' \item{convergence}{a numeric element of the R-hat threshold for convergence (default = 1.1)}
#' \item{n_chains}{a integer element indciating the number of MCMC chains (default = 3)}
#' \item{n_resamples}{an integer element of the number of times to resample 
#' until convergence is achieved (default = 3)}
#' #' \item{parallel_chains}{a boolean element indicating whether the chains should
#' be run on separate processes (default is platform dependennt)}
#' \item{parallel_models}{a boolean element indicating whether the models should
#' be run on separate processes (default = FALSE)}
#' \item{quiet}{a boolean element indicating whether or not to suppress messages (default = FALSE)}
#' }
#' 
#' By default a JAGS analysis will retain a minumum of 1,000 MCMC samples 
#' thinned from the second halves of three chains. For example 
#' if \code{n_iters = 1000} in the analysis then by default 334 samples will be 
#' thinned from the last 500 iterations of each chain.  
#'  
#' Convergence is considered to have been achieved when all the monitored
#' parameters have an R-hat less than the value of the \code{convergence} option
#' which
#' by default is 1.1 (Kery & Schaub 2011). If the initial number of iterations (\code{n_iters})
#' are performed and the convergence target has not been achieved and
#' the \code{n_resamples} option is greater than 0
#' then the value of \code{n_iters} is doubled, the MCMC sampling to date is 
#' considered the burn in period, the saved MCMC samples are discarded and 
#' MCMC sampling continues.  This process is continued until the convergence target is 
#' achieved or resampling exceeds the value of the \code{n_resamples} option.
#' 
#' Currently parallel processing is only available for unix-based systems. For such systems
#' the \code{parallel_chains} option is by default \code{TRUE} otherwise its \code{FALSE}.
#' 
#' @return For \code{opts_jagr0()} a list of all jaggernaut options values
#' sorted by name. For \code{opts_jagr0(name)} a list of length one of the 
#' option value. When setting one or more options a list with the previous values of
#' the options unchanged (returned invisibly).
#' @seealso \code{\link{analysis}} and \code{\link{options}}
#' @usage options_jaggernaut(...) 
#' 
#' opts_jagr0(...)
#' @examples
#' opts_jagr0()
#' opts_jagr0(n_chains = 4)
#' opts_jagr0("n_chains")
#' opts_jagr0(default = TRUE)
#' options_jaggernaut("n_chains")
#' @export 
#' @aliases opts_jagr0
options_jaggernaut <- function (...) {
  single <- FALSE
  opts <- if (exists(".opts_jagr0", frame = 1)) {
      get(".opts_jagr0", pos = 1)
  } else {
    .opts_jagr0_def
  }
  if (nargs() == 0) {
    return(opts)
  }
  args <- list(...)
  if(length(args) == 1) {
    if(is.list(args[[1]])) {
      args <- args[[1]]
    } else if (is.null(names(args))) {
      single <- TRUE
    }
  }
  if(is.null(names(args))) {
    args <- unlist(args)
    value <- vector("list", length(args))
    names(value) <- args
    for (v in args) {
      if (v %in% names(opts)) {
        value[v] <- opts[v]
      } 
    }
    if(single) {
      return (value[[1]])
    }
    return (value)
  }
  old <- vector("list", length(args))
  names(old) <- names(args)
  if ("default" %in% names(args) && args$default == TRUE) {
    opts <- .opts_jagr0_def
  }
  for (v in names(args)) {
    if (v %in% names(opts)) {
      old[v] <- opts[v]
      if (is.null(args[[v]])) {
        opts[v] <- list(NULL)
      } else if (mode(opts[[v]]) == mode(args[[v]])) {
        opts[v] <- args[v]
      }
    }
  }
  assign_opts_jagr0(opts)
  invisible(old)
}

#' @export 
opts_jagr0 <- function (...) {
  return (options_jaggernaut(...))
}

assign_opts_jagr0 <- function (opts) {
  
  for (v in names(.opts_jagr0_def)) {
    if (!v %in% names(opts))
      stop(paste("option",v,"is unspecified"))
  }
  for (v in names(opts)) {
    if (!v %in% names(.opts_jagr0_def))
      stop(paste("option",v,"is unknown"))
  }  

  if (length(opts$convergence) != 1) {
    stop("option convergence must be length 1")
  }
  if (length(opts$n_chains) != 1) {
    stop("option n_chains must be length 1")
  }
  if (length(opts$parallel_chains) != 1) {
    stop("option n_chains must be length 1")
  }  
  if (length(opts$parallel_models) != 1) {
    stop("option n_chains must be length 1")
  }  
  if (length(opts$quiet) != 1) {
    stop("option quiet must be length 1")
  }  
  if (length(opts$n_resamples) != 1) {
    stop("option n_resamples must be length 1")
  }  
  
  opts$n_chains <- as.integer(opts$n_chains)
  opts$n_resamples <- as.integer(opts$n_resamples)
  
  if (!(opts$convergence >= 1 &&  opts$convergence <= 2)) {
    stop("option convergence must lie between 1 and 2")
  }  
  if (!opts$n_chains %in% 2:6) {
    stop("option n_chains must be greater than 1 and less than 7")
  }  
  if (!opts$n_resamples %in% 0:3) {
    stop("option n_resamples must lie between 0 and 3")
  } 
  if (opts$parallel_chains && .Platform$OS.type=="windows") {
    stop("option parallel_chains == TRUE not yet implemented for windows")
  } 
  if (opts$parallel_models && .Platform$OS.type=="windows") {
    stop("option parallel_models == TRUE not yet implemented for windows")
  } 
  
  assign(".opts_jagr0", opts, pos = 1)
  invisible(opts)
}

