
cat_convergence <- function (object) {
  cat (' (Rhat:')
  cat (rhat(object))
  cat (')\n')
}
