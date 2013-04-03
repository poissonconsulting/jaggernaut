
is_scalar <- function (x) {
  return (is.vector(x) && length(x) == 1)
}