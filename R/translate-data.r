
translate_data <- function (select, data, dat = NULL)
{  
  if (!(is.null(select) || is.character(select)))
    stop ("select must be NULL or a character vector")
  
  if (!(is.data.frame(data) || is_data_list(data)))
    stop ("data must be a data.frame or data list")
  
  if (!(is.null(dat) || (is.data.frame(data) && is.data.frame(dat)) || (is_data_list(data) && is_data_list(dat))))
    stop("dat should be NULL or a data.frame or a data list")

  if (is.null(dat)) 
    dat <- data

  vars <- names_select(select)

  bol <- !vars %in% names_data(data)
  if (any(bol)) {
    stop(paste(vars[bol],"in select but not variable names in data"))
  }

  reserved <- c("row","all","fixed","random","deviance")
  
  if (is.data.frame(data)) {
    reserved <- c(reserved,"nrow")
  }
  
  bol <- reserved %in% names_data(data)
  
  if (any(bol)) {
    stop(paste(reserved[bol],"must not be variable names in data"))
  }
  
  standardise <- NULL
  centre <- NULL
  
  if (!is.null(select)) {
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
    
    transform <- list()
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
    
    data <- data[select]
    dat <- dat[select[select %in% names_data(dat)]]
        
    for (i in seq_along(select)) {
      var <- select[i]
      if (!is.null(transform[[var]])) {
        
        cmd <- paste0("data[[var]]<-",transform[[var]],"(data[[var]])")
        eval(parse(text = cmd))  
        
        if(var %in% names_data(dat)) {
          cmd <- paste0("dat[[var]]<-",transform[[var]],"(dat[[var]])")
          eval(parse(text = cmd))        
        }
      }
    }
  }
  
  if (is.data.frame(dat)) {
    nrow <- nrow(dat)
  } else {
    nrow <- NULL
  }

  facs <- list()
  for (name in names_data(dat)) {
    if (is.factor(dat[[name]])) {
      facs[[paste0("n",name)]]<-nlevels(dat[[name]])
    }
  }

  data <- convert_data (data, centre = centre, standardise = standardise, dat = dat)
  
  data <- c(as.list(data), facs, nrow = nrow)
  
  return (data)
}
