
#' @export
combine_expected <- function (x, fun = sum, calc_estimates = TRUE) {

  if (!is.list(x))
    stop ("x must be a list")
      
  colnames <- lapply(x, colnames)
  
  colnames <- colnames(x[[1]])
  for (i in 2:length(x)) {
    colnames <- colnames[colnames %in% colnames(x[[i]])]
  }
  colnames <- colnames[grepl("V[[:digit:]]", colnames)]
      
  array <- as.matrix((x[[1]][,colnames]))
  for (i in 2:length(x)) {
    mat <- as.matrix((x[[1]][,colnames]))
    array <- abind(array, mat, along = 3)
  }  
  mat <- apply(array, MARGIN=c(1,2), fun)

  if(calc_estimates) {
    mat <- calc_estimates (t(mat))
  }
  data <- x[[1]]
  data <- data[,!colnames(data) %in% colnames]
  data <- cbind(data, mat)
  return (data)
}
