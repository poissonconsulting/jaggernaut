
translate_data <- function (select, data, dat = NULL) {
  
  stopifnot(is_null(select) || is_character_vector(select))
  stopifnot(is_convertible_data(data))
  stopifnot(is_null(dat) || (is_convertible_data(dat) &&
                               is_convertible_data_frame(dat) == is_convertible_data_frame(data)))

  if (is.null(dat)) 
    dat <- data
  
  bol <- !names(dat) %in% names(data)
  if(any(bol)) {
    data <- merge(data, datalist::new_data(dat[bol]))
  }

  vars <- names_select(select)

  bol <- !vars %in% names(data)
  if (any(bol))
    stop("The following variables are in select but not data: ", 
         paste(vars[bol], collapse = ", "))

  reserved <- c("all","fixed","random")
  
  bol <- reserved %in% names(data)
  
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
    dat <- dat[select[select %in% names(dat)]]
        
    for (i in seq_along(select)) {
      var <- select[i]
      if (!is.null(transform[[var]])) {
        
        cmd <- paste0("data[[var]]<-",transform[[var]],"(data[[var]])")
        eval(parse(text = cmd))  
        
        if(var %in% names(dat)) {
          cmd <- paste0("dat[[var]]<-",transform[[var]],"(dat[[var]])")
          eval(parse(text = cmd))        
        }
      }
    }
  }

  facs <- list()
  for (name in names(dat)) {
    if (is.factor(dat[[name]])) {
      facs[[paste0("n",name)]]<-nlevels(dat[[name]])
    }
  }

  data <- convert_data (data, centre = centre, standardise = standardise, dat = dat)
  
  data <- c(as.list(data), facs)
    
  return (data)
}
