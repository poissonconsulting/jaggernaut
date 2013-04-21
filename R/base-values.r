
#replace with dataset(,base=TRUE)
base_values <- function (object) {
  if(!is.jags_analysis(object))
    stop("object should be of class jags_analysis")
  
  object <- top_model(object)
  base <- generate_data (object$data)
  if (!is.null(object$block$select))
    base <- subset (base, select = process_select(object$block$select))
  
  return (base)
}
