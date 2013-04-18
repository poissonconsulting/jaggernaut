

process_select <- function (select) {
  
  if(!(is.null(select) || (is.character(select) && length(select))))
     stop("select should be NULL or a character vector with at least one element")

  if (is.null (select))
    return (select)
  
  nchar <- nchar(select)
  bol <- substr(select,nchar,nchar) == '*'
  if (any(bol))
    select[bol] <- substr(select[bol],1,nchar[bol]-1)
        
  for (i in seq_along(select)) {
    nchar <- nchar(select[i])
    if (substr(select[i],nchar,nchar) == ')') {
      select[i] <- substr(select[i],1,nchar-1)
      parts <- strsplit(select[i],'(',fixed=T)[[1]]
      stopifnot(length(parts)==2)
      select[i] <- parts[2]
    }
  }
  return (select)
}
