nls_out <- function(nls_obj) { 
  if(is.null(nls_obj)) { 
    k<-NA 
  } else { 
    if(class(nls_obj)!="try-error") {
      k<-summary(nls_obj)$coefficients[2,1] 
    } else {
      k<-NA
    }
  } 
  return(k)
}
