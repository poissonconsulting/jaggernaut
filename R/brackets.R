reverse_brackets <- function(x) {
  osquare <- gregexpr("[[]", x)
  csquare <- gregexpr("[]]", x)
  osquiggly <- gregexpr("[{]", x)
  csquiggly <- gregexpr("[}]", x)
  ocurved <- gregexpr("[(]", x)
  ccurved <- gregexpr("[)]", x)
  
  regmatches(x, osquare) <- "]"
  regmatches(x, csquare) <- "["
  regmatches(x, osquiggly) <- "}"
  regmatches(x, csquiggly) <- "{"
  regmatches(x, ocurved) <- ")"
  regmatches(x, ccurved) <- "("
  
  x
}

pass_brackets <- function (x, i, forward = TRUE) {
  nx <- nchar(x) 
  if(missing(i)){
    i <- ifelse(!forward, nx, 1)
  }
  if(!forward) {
    x <- reverse_brackets(reverse_strings(x))
    i <- nx - i + 1
  }
  sc <- substr(x, i, i)
  names(i) <- sc
  repeat {
    ii <- as.integer(regexpr("[][{}()]", substr(x, i + 1, nx)))
    if(ii == -1) {
      stop ("unmatched brackets")
    }
    i[1] <- i + ii
    names(i) <- paste0(names(i),substr(x, i - ii + 1, i - 1)) 
    ec <- substr(x, i, i)
    if(grepl("[[{(]", ec)) {
      ii <- pass_brackets(x, i)
      i[1] <- ii
      names(i) <- paste0(names(i), names(ii))
    } else if (ec == switch(sc, 
      "[" = "]",
      "{" = "}",
      "(" = ")",
      stop("error"))) {
      names(i) <- paste0(names(i), ec)
      if(!forward) {
        i[1] <- nx - i + 1        
        names(i) <- reverse_brackets(reverse_strings(names(i)))
      }
      return (i)
    } else {
      stop("unmatched brackets")
    }
  }
}  
