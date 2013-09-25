
print.mcarray <- function (x, ...) {
  
  ndim <- length(dim(x))  
  xx <- x
  class(xx) <- 'array'
  xx <- apply(xx, 1, mean)
  
  print(xx, ...)
  
  invisible(x)
}

print.jags_mcmc <- function (x, ...) {
  cat("\nparameters\n")
  
  
  print_list <- function (x) {
    for (par in names(x)) {
      print(par)
      print(x[[par]])
    }
  }
  
  print(coef(x))
  
  cat("\nchains\n")
  print(nchain(x))
  
  cat("\nsimulations\n")
  print(nsim(x))
  
  invisible(x)
}

#' @method print jags_model
#' @export
print.jags_model <- function (x, ...) {
  
  cat("\nnumber of models: ")
  cat(nmodel(x))
  cat("\n")
  
  for (i in 1:nmodel(x)) {
    
    y <- subset_jags(x, model = i)
    
    cat("\nmodel number: ")
    cat(i)
    cat("\n")
    
    cat("\nmodel code:\n")
    cat(model_code(y))
    cat("\n")
    
    if(!is.null(derived_code(y))) {
      cat("\nderived code\n")
      cat(derived_code(y))
      cat("\n")
    }
    
    if(!is.null(random_effects(y))) {
      cat("\nrandom effects\n")
      print(random_effects(y))
    } 
    
    if(!is.null(select(y))) {
      cat("\nselect\n")
      print(select(y))   
    }
    
    if(!is.null(modify_data(y))) {
      cat("\nmodify data function\n")
      cat(modify_data(y))
      cat("\n")
    }
    
    if(!is.null(gen_inits(y))) {
      cat("\ngenerate initial values function\n")
      print(gen_inits(y))   
    }
    
    if(!is.null(monitor(y))) {
      cat("\nmonitor\n")
      print(monitor(y))
    }   
  }
  invisible(x)
}

#' @method print jagr_analysis
#' @export
print.jagr_analysis <- function (x, ...) {
  
  print(as.jags_model(x)) 
  cat("\ninits\n")
  print(x$inits)
  cat("\niterations\n")
  print(x$iterations)
  cat("\ntime\n")
  print(x$time)
  cat("\nmcmc\n")
  print(as.jags_mcmc(x))
  
  invisible(x)
}

#' @method print jags_analysis
#' @export
print.jags_analysis <- function (x, ...) {
  
  cat("\ndata\n")
  print(head(x$data))
  
  lapply(x$analyses, print)
  
  invisible(x)
}

#' @method print summary_jagr_analysis
print.summary_jagr_analysis <- function (x, ...) {
  
  for (name in names(x)) {
    cat(paste0("\n",name,":\n"))
    print(x[[name]])
  }
}


#' @method print summary_jags_analysis
#' @export
print.summary_jags_analysis <- function (x, ...) {
  
  for (name in names(x)) {
    cat(paste0("\n",name,":\n"))
    print(x[[name]])
  }
}
