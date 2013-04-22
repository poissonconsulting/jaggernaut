
.opts_jagr_debug <- list(
  level = 0.80,
  mode = "debug",
  nchains = 2,
  nresample = 0,
  nsims = 100,
  parallel_chains = FALSE, 
  parallel_models = FALSE,
  quiet = FALSE,
  rhat = 2
)

.opts_jagr_test<- list(
  level = 0.90,
  mode = "explore",
  nchains = 2,
  nresample = 2,
  nsims = 500,
  parallel_chains = .Platform$OS.type!="windows", 
  parallel_models = FALSE,
  quiet = TRUE,
  rhat = 1.5
)

.opts_jagr_explore<- list(
  level = 0.90,
  mode = "explore",
  nchains = 2,
  nresample = 2,
  nsims = 500,
  parallel_chains = .Platform$OS.type!="windows", 
  parallel_models = FALSE,
  quiet = FALSE,
  rhat = 1.5
)

.opts_jagr_report <- list(
  level = 0.95,
  mode = "report",
  nchains = 3,
  nresample = 3,
  nsims = 1000,
  parallel_chains = .Platform$OS.type!="windows", 
  parallel_models = FALSE,
  quiet = FALSE,
  rhat = 1.1
)

.opts_jagr_paper <- list(
  level = 0.95,
  mode = "paper",
  nchains = 4,
  nresample = 4,
  nsims = 2000,
  parallel_chains = .Platform$OS.type!="windows", 
  parallel_models = FALSE,
  quiet = FALSE,
  rhat = 1.05
)

.opts_jagr_def <- .opts_jagr_report

#' @title Get and set jaggernaut options
#'
#' @description
#' Queries and sets options for JAGS analyses.
#' @param ... options can be defined using \code{name = value} or by passing a list
#' of such tagged values.
#' @details
#' The function \code{opts_jagr()}, which can aso be invoked using its alias
#' \code{options_jaggernaut()}, behaves just like the \code{options()}
#'  function in the 
#' base library, with the additional feature that 
#' \code{opts_jagr(mode="default")}
#'  will 
#' reset all options to the values for the default mode. 
#' There are four available modes: debug, explore,
#' report and paper which are characterized by increasing accuracy. 
#' In summary the debug model should be used when first trying to code models in the
#' JAGS dialect of the BUGS language. Once the JAGS code is running without errors
#' is now time to switch to the explore model to look at model adequacy. Once you are content
#' that the model is adequate you can now switch to report model to extract the results
#' for presentation in a report or paper model if you are going to be sending the results to 
#' a peer-reviewed journal. The default mode is report mode.
#' 
#' Available options are
#' \describe{
#' \item{level}{the credible interval level (default = 0.95)}
#' \item{nchains}{the number of MCMC chains (default = 3)}
#' \item{nresample}{the number of times to resample 
#' until convergence is achieved (default = 3)}
#' \item{nsims}{the total number of MCMC samples to thin from the second halves of the MCMC chains (default = 1000)}
#' \item{parallel_chains}{whether the chains should
#' be run on separate processes (default is platform dependennt)}
#' \item{parallel_models}{whether the models should
#' be run on separate processes (default = FALSE)}
#' \item{quiet}{whether to suppress messages (default = FALSE)}
#' \item{rhat}{the R-hat threshold for convergence (default = 1.1)}
#' }
#' 
#' By default a JAGS analysis will retain a minumum of 1,000 MCMC samples 
#' thinned from the second halves of three chains. For example 
#' if \code{niter = 1000} in the analysis then by default 334 samples will be 
#' thinned from the last 500 iterations of each chain.  
#'  
#' Convergence is considered to have been achieved when all the monitored
#' parameters have an R-hat less than the value of the \code{rhat} option
#' which
#' by default is 1.1 (Kery & Schaub 2011). If the initial number of iterations
#' are performed and the convergence target has not been achieved and
#' \code{nresample > 0}
#' then the value of \code{niter} is doubled, the MCMC sampling to date is 
#' considered the burn in period, the saved MCMC samples are discarded and 
#' MCMC sampling continues.  This process is repeated until the convergence target is 
#' achieved or resampling would exceed the value of the \code{nresample} argument.
#' 
#' Currently parallel processing is only available for unix-based systems. For such systems
#' the \code{parallel_chains} option is by default \code{TRUE} otherwise its \code{FALSE}.
#' 
#' @return For \code{opts_jagr()} a list of all jaggernaut options values
#' sorted by name. For \code{opts_jagr(name)} a list of length one of the 
#' option value. When setting one or more options a list with the previous values of
#' the options unchanged (returned invisibly).
#' @seealso \code{\link{jags_analysis}} and \code{\link{options}}
#' @usage opts_jagr(...)
#' 
#' options_jaggernaut(...) 
#' @examples
#' opts_jagr()
#' opts_jagr(mode = "debug")
#' options_jaggernaut()
#' opts_jagr("nchains","mode")
#' opts_jagr(nchains = 4)
#' opts_jagr("nchains","mode")
#' old <- opts_jagr(mode = "default") 
#' opts_jagr()
#' opts_jagr(old)
#' opts_jagr("nchains","mode") 
#' @export 
#' @aliases options_jaggernaut
opts_jagr <- function (...) {
  single <- FALSE
  opts <- if (exists(".opts_jagr", frame = 1)) {
      get(".opts_jagr", pos = 1)
  } else {
    .opts_jagr_def
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
  old <- opts
  if ("mode" %in% names(args)) {
    if (args$mode == "debug") {
      opts <- .opts_jagr_debug
    } else if (args$mode == "test") {
      opts <- .opts_jagr_test
    }else if (args$mode == "explore") {
      opts <- .opts_jagr_explore
    } else if (args$mode == "report") {
      opts <- .opts_jagr_report
    } else if (args$mode == "paper") {
      opts <- .opts_jagr_paper
    } else if (args$mode == "default") {
      opts <- .opts_jagr_def
    } else if (args$mode == "custom") {
      opts$mode <- "custom"
    } else if (args$mode != "current") {
      stop(paste("mode",args$mode,"not recognized"))
    }
  }
  for (v in names(args)) {
    if (v %in% names(opts) && v != "mode") {
      opts["mode"] <- "custom"
      if (is.null(args[[v]])) {
        opts[v] <- list(NULL)
      } else if (mode(opts[[v]]) == mode(args[[v]])) {
        opts[v] <- args[v]
      }
    }
  }
  assign_opts_jagr(opts)
  invisible(old)
}

#' @export 
options_jaggernaut <- function (...) {
  return (opts_jagr(...))
}

assign_opts_jagr <- function (opts) {
  
  for (v in names(.opts_jagr_def)) {
    if (!v %in% names(opts))
      stop(paste("option",v,"is unspecified"))
  }
  for (v in names(opts)) {
    if (!v %in% names(.opts_jagr_def))
      stop(paste("option",v,"is unknown"))
  }  

  if (length(opts$level) != 1) {
    stop("option level must be length 1")
  }
  if (length(opts$nchains) != 1) {
    stop("option nchains must be length 1")
  }
  if (length(opts$nresample) != 1) {
    stop("option nresample must be length 1")
  }  
  if (length(opts$nsims) != 1) {
    stop("option nsims must be length 1")
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
  if (length(opts$rhat) != 1) {
    stop("option convergence must be length 1")
  }
  
  opts$nchains <- as.integer(opts$nchains)
  opts$nresample <- as.integer(opts$nresample)
  opts$nsims <- as.integer(opts$nsims)
   
  if (!(opts$level >= 0.8 && opts$level <= 0.99)) {
    stop("option level must lie between 0.8 and 0.99")
  }  
  if (!opts$nchains %in% 2:4) {
    stop("option nchains must lie between 2 and 4")
  }  
  if (!opts$nresample %in% 0:4) {
    stop("option nresample must lie between 0 and 4")
  } 
  if (!(opts$nsims >= 100 &&  opts$nsims <= 2000)) {
    stop("option nsims must lie between 100 and 2000")
  } 
  if (opts$parallel_chains && .Platform$OS.type=="windows") {
    stop("option parallel_chains == TRUE not yet implemented for windows")
  } 
  if (opts$parallel_models && .Platform$OS.type=="windows") {
    stop("option parallel_models == TRUE not yet implemented for windows")
  } 
  if (!(opts$rhat >= 1 &&  opts$rhat <= 2)) {
    stop("option convergence must lie between 1 and 2")
  } 
  
  assign(".opts_jagr", opts, pos = 1)
  invisible(opts)
}

