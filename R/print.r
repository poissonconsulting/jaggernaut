
print.mcarray <- function (x, ...) {
  
  x1 <- x
  
  class(x) <- "array"
  x <- apply(x, 1, mean)
  print(x, ...)
  
  return(invisible(x1))
}

print.jagr_chains <- function (x, ...) {
  
  cat(paste("\nchains:",nchains(x),"\n"))
  cat(paste("\nsimulations:",nsims(x),"\n"))
  cat(paste("\nrhat:",rhat(x, parm = "all", combine = TRUE),"\n"))
  
  return(invisible(x))
}

print.jagr_model <- function (x, ...) {
      
  cat("\nmodel code: ")
  cat(model_code(x))
  cat("\n")
  
  return(invisible(x))
}

#' @method print jags_model
#' @export
print.jags_model <- function (x, ...) {
  
  if(is_one_model(x)) {
    print(model(x), ...)    
    return (invisible(x))
  }

  models <- models(x)
    
  for (i in 1:nmodels(x)) {
    cat(paste0("\n",paste0("Model",i),":\n"))
    print(models[[i]], ...)
  }
  return(invisible(x))
}

print.jagr_power_analysis <- function (x, ...) {
    
  print(as.jagr_chains(x), ...)
  cat(paste("\niterations:",niters(x),"\n"))
  
  return(invisible(x))
}

print.jagr_analysis <- function (x, ...) {
  
  print(as.jagr_analysis_model(x), ...)
  print(as.jagr_power_analysis(x), ...)
  
  return(invisible(x))
}

#' @method print jags_analysis
#' @export
print.jags_analysis <- function (x, ...) {

  cat("\ndata head:\n")
  print(head(dataset(x)))
  
  if (is_one_model(x)) {
    print(analysis(x), ...)
  } else {
    analyses <- analyses(x)
    
    for (i in 1:nmodels(x)) {
      cat(paste0("\n",paste0("Analysis",i),":\n"))
      print(analyses[[i]], ...)
    }
  }
  cat(paste0("\nrhat threshold: ", rhat_threshold(x),"\n"))
  
  return(invisible(x))
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
