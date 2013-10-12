
cat_convergence <- function (object) {
  stopifnot(is.jagr_power_analysis(object))
  
  cat (' (Rhat:')
  cat (rhat(object, parm = "all"))
  cat (')\n')
}
