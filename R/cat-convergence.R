
cat_convergence <- function (object) {
  stopifnot(is.jagr_analysis(object))
  
  cat (' (rhat:')
  cat (convergence(object, parm = "all"))
  cat (')\n')
}
