
translate_data <- function (object, data, dat = NULL)
{  
  if (!is.jmodel(object))
    stop ("object must be of class jmodel")
  
  if (!is.data.frame(data))
    stop ("data must be of class data.frame")
  
  if (!is.null(dat) && !is.data.frame(dat)) 
    stop("dat should be NULL or a data.frame")  
  
  if(any(c('nrow','row') %in% colnames (data)))
    stop ("'row' and 'nrow' must not be colnames in data")

  if (is.null(dat)) 
    dat <- data
  
  standardise <- NULL
  
  select <- object$select

  if (!is.null(select)) {
    
    nchar <- nchar(select)
    standardise <- substr(select,nchar,nchar) == '*'
    if (any(standardise)) {
      select[standardise] <- substr(select[standardise],1,nchar[standardise]-1)
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
        
    data <- subset(data, select = select)
    dat <- subset(dat, select = select[select %in% colnames(dat)])
    
    cap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
        
    for (i in seq_along(select)) {
      var <- select[i]
      if (!is.null(transform[[var]])) {
        colname <- paste0(cap(transform[[var]]),var)
        stopifnot(!colname %in% colnames(data))
        vec <- data[,var,drop=T]
        
        cmd <- paste0("data[,colname]<-",transform[[var]],"(vec)")
        eval(parse(text = cmd))  
        data[,var] <- NULL

        if(var %in% colnames (dat)) {
          vec <- dat[,var,drop=T]
          cmd <- paste0("dat[,colname]<-",transform[[var]],"(vec)")
          eval(parse(text = cmd))        
          dat[,var] <- NULL
        }
        standardise[standardise == var] <- colname
      }
    }
  }
  
  nrow <- nrow(dat)

  facs <- list()
  for (colname in colnames(dat)) {
    if (is.factor(dat[, colname])) {
      facs[[paste0("n",colname)]]<-nlevels(dat[, colname])
    }
  }

  data <- convert_data (data, standardise = standardise, dat = dat)
  
  data <- c(as.list(data), facs, nrow = nrow)
  
  if (is.function (object$modify_data))
    data <- object$modify_data (data)
  
  return (data)
}