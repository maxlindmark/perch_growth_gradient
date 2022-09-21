fit_nls <- function(length,age) { 
  if(length(age)>2) {
    try(nls(length~VBGF(age,Linf,k),start=c(Linf=400,k=0.25)),silent=T) 
  } 
}
