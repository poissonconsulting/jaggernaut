.onAttach <- function(libname, pkgname) {
  
  if (getDoParWorkers() == 1) {
    registerDoParallel(3)
    opts_jagr(parallel = TRUE)
  }
  
  opts_jagr(mode = "report")
  
  invisible()
}
