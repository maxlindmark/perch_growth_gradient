fit_nls_multstart <- function(length,age) { 
  if(length(age)>2) {
    try(nls_multstart(
      length~VBGF(age=age,Linf,k),
      data=data.frame(cbind(length,age)),
      iter=500,
      start_lower=c(Linf=100,k=0.1),
      start_upper=c(Linf=500,k=0.6),
      supp_errors='Y'
      )
    ) 
  } 
}
