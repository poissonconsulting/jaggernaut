
#replace with dataset(,base=TRUE)
base_values <- function (object) {
  if(!is.janalysis(object))
    stop("object should be of class janalysis")
  
  object <- top_model(object)
  base <- generate_data (object$data)
  if (!is.null(object$block$select))
    base <- subset (base, select = process_select(object$block$select))
  
  return (base)
}
