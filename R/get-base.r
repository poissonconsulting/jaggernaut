#' @export
get_base <- function (object) {
  base <- generate_data (object$data)
  if (!is.null(object$block$select))
    base <- subset (base, select = process_select(object$block$select))
  
  return (base)
}
