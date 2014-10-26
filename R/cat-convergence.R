
cat_convergence <- function (object) {
  stopifnot(is.jagr_analysis(object))
  
  cat (' (convergence:')
  cat (convergence(object, parm = "all"))
  cat (')\n')
}
