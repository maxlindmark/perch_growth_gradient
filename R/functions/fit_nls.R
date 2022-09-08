fit_nls <- function(length,age) { 
  if(length(age)>2) {
    try(nls(length~VBGF(age,Linf,k), start=c(Linf=500,k=0.3)),silent=T) 
  } 
}
