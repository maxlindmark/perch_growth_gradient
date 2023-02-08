nls_out <- function(nls_obj) { 
  if(is.null(nls_obj)) { 
    k<-NA 
    k_se<-NA 
    linf<-NA
    linf_se<-NA 
  } else { 
    if(class(nls_obj)!="try-error") {
      k<-summary(nls_obj)$coefficients[2,1]
      k_se<-summary(nls_obj)$coefficients[2,2] 
      linf<-summary(nls_obj)$coefficients[1,1]
      linf_se<-summary(nls_obj)$coefficients[1,2]
    } else {
      k<-NA
      k_se<-NA
      linf<-NA
      linf_se<-NA
    }
  } 
  return(data.frame(k, k_se, linf, linf_se))
}