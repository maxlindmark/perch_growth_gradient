fit_nls_multstart <- function(length,age,min_nage=3,model="VBGF") { 
  if(length(age)>=min_nage) {
    switch(model,
           Gompertz=try(nls_multstart(
             length~Gompertz(age,Linf,k,lag),
             data=data.frame(cbind(length,age)),
             iter=500,
             start_lower=c(Linf=100,k=0.1,lag=0),
             start_upper=c(Linf=500,k=0.6,lag=5),
             supp_errors='Y')
             ),
           VBGF=try(nls_multstart(
             length~VBGF(age=age,Linf,k),
             data=data.frame(cbind(length,age)),
             iter=500,
             start_lower=c(Linf=100,k=0.1),
             start_upper=c(Linf=500,k=0.6),
             supp_errors='Y')
             ),
    stop("Error: '",model,"' model is not available")
    )
  } 
}
