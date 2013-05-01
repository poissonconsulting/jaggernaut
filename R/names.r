
names_data <- function (data) {
  
  if (!(is.data.frame(data) || is_data_list(data)))
    stop("data must be a data.frame or data list")
  
  if(is.data.frame(data)) {
    return (colnames(data))
  }
  return (names(data))
}

names_select <- function (select) {
  if (!(is.null(select) || is.character(select))) {
    stop("select must be NULL or class character")
  }
  if (is.character(select) && length(select) == 0) {
    stop("select must have one or more elements")
  }  
  
  if(is.null(select))
    return (NULL)
  
  nchar <- nchar(select)
  standardise <- substr(select,nchar,nchar) == '*'
  if (any(standardise)) {
    select[standardise] <- substr(select[standardise],1,nchar[standardise]-1)
  }
  
  nchar <- nchar(select)
  centre <- substr(select,nchar,nchar) == '+'
  if (any(centre)) {
    select[centre] <- substr(select[centre],1,nchar[centre]-1)
  }
  
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

