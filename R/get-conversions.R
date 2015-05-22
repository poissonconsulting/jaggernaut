get_conversions <- function (select) {  
  standardise <- NULL
  centre <- NULL
  transform <- list()

  if(!is.null(select)) 
    list(select = select, centre = centre, standardise = standardise, 
         transform = transform)
    
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
      transform[[select[i]]] <- parts[1]
    } else
      transform[[select[i]]] <- NULL
  }
  
  standardise <- select[standardise]
  centre <- select[centre]
  list(select = select, centre = centre, standardise = standardise, transform = transform)
}
