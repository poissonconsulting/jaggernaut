delete_variable_variables <- function (x) {
  
  assert_that(is.data.frame(x))

  for(col in colnames(x)) {
    if(anyDuplicated(x[[col]])) {
      x <- x[,!colnames(x) %in% col,drop = FALSE] 
    }
  }
  x
}
