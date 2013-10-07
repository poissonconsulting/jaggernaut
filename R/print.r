
print.mcarray <- function (x, ...) {
  
  x1 <- x
  
  class(x) <- "array"
  x <- apply(x, 1, mean)
  print(x, ...)
  
  invisible(x1)
}

print.jagr_chains <- function (x, ...) {
  
  cat(paste("\nchains:",nchains(x),"\n"))
  cat(paste("\nsimulations:",nsims(x),"\n"))
  
  invisible(x)
}

print.jagr_model <- function (x, ...) {
      
  cat("\nmodel code: ")
  cat(model_code(x))
  cat("\n")
  
  if(!is.null(monitor(x))) {
    cat("\nmonitor: ")
    cat(monitor(x))
    cat("\n")
  } 
  
  if(!is.null(select(x))) {
    cat("\nselect: ")
    cat(select(x))   
    cat("\n")
  }
  
  if(!is.null(modify_data(x))) {
    cat("\nmodify data: ")
    print(modify_data(x))
  }
  
  if(!is.null(gen_inits(x))) {
    cat("\ngen inits: ")
    print(gen_inits(x))
  }
  
  invisible(x)
}

#' @method print jags_data_model
#' @export
print.jags_data_model <- function (x, ...) {
  
  print(as.jagr_model(x, ...))
  
  if(!is.null(extract_data(x))) {
    cat("\nextract data: ")
    cat(extract_data(x))
    cat("\n")
  } 
  invisible(x)
}

print.jagr_analysis_model <- function (x, ...) {
  
  print(as.jagr_model(x), ...)

  if(!is.null(modify_data_derived(x))) {
    cat("\nmodify data derived: ")
    cat(modify_data_derived(x))
    cat("\n")
  } 
  
  if(!is.null(derived_code(x))) {
    cat("\nderived code: ")
    cat(derived_code(x))
    cat("\n")
  } 
  
  if(!is.null(random_effects(x))) {
    cat("\nrandom effects: ")
    cat(random_effects(x))   
    cat("\n")
  }
  invisible(x)
}

#' @method print jags_model
#' @export
print.jags_model <- function (x, ...) {

  models <- models(x)
    
  for (i in 1:nmodels(x)) {
    cat(paste0("\n",paste0("Model",i),":\n"))
    print(models[[i]], ...)
  }
  invisible(x)
}

#' @method print jagr_analysis
#' @export
print.jagr_power_analysis <- function (x, ...) {
    
  cat(paste("iterations:",niters(x),"\n"))
  print(as.jagr_chains(x), ...)
  
  invisible(x)
}

#' @method print jagr_analysis
#' @export
print.jagr_analysis <- function (x, ...) {
  
  print(as.jagr_analysis_model(x), ...)
  print(as.jagr_power_analysis(x), ...)
  
  invisible(x)
}

#' @method print jags_analysis
#' @export
print.jags_analysis <- function (x, ...) {

  cat("\ndata:\n")
  print(head(data_jags(x)))
  
  analyses <- analyses(x)
    
  for (i in 1:nmodels(x)) {
    cat(paste0("\n",paste0("Analysis",i),":\n"))
    print(analyses[[i]], ...)
  }
  
  cat(paste0("\nrhat threshold: ", rhat_threshold(x),"\n"))
  
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
