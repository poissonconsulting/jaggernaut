
#' @title Combine predictions
#'
#' @description
#' Calculates estimates for derived parameters for data.frames from
#' multiple JAGS analyses
#' 
#' @param object a list of data.frames from predict(...,level = "no")
#' @param fun the function to apply to iterations.
#' @param level a numeric scalar specifying the significance level or a character
#' scalar specifying which mode the level should be taken from. By default the
#' level is as currently specified by \code{opts_jagr} in the global options.
#' @return a data frame with the median estimates and credibility intervals for
#' the derived parameter of interest
#' @seealso \code{\link{predict.jags_analysis}}
#' @export 
combine_predictions <- function (object, fun = sum, level = "current") {
  
  if(!is.list(object)) {
    stop("object must be a list of data.frames")
  }
  if (!is.function (fun)) {
    stop("fun must be a function")
  }
  
  old_opts <- opts_jagr()
  on.exit(opts_jagr(old_opts))
  
  if (!is.numeric(level)) {
    opts_jagr(mode = level)
    level <- opts_jagr("level")
  } else {
    if (level < 0.75 || level > 0.99) {
      stop("level must lie between 0.75 and 0.99")
    }
  } 
   x <- object
  
  colnames <- lapply(x, colnames)
  
  colnames <- colnames(x[[1]])
  for (i in 2:length(x)) {
    colnames <- colnames[colnames %in% colnames(x[[i]])]
  }
  colnames <- colnames[grepl("V[[:digit:]]", colnames)]
  
  array <- as.matrix((x[[1]][,colnames]))
  for (i in 2:length(x)) {
    mat <- as.matrix((x[[i]][,colnames]))
    array <- abind(array, mat, along = 3)
  }  
  mat <- apply(array, MARGIN=c(1,2), fun)
  
  pred <- calc_estimates (t(mat), level = level)
  
  data <- x[[1]]
  data <- data[,!colnames(data) %in% colnames]
  pred <- cbind(data, pred)
  return (pred)
}
