fit_nls <- function(length,age,min_nage=3,model="VBGF") { 
  if(length(age)>=min_nage) {
    switch(model,
           Gompertz=try(nls(length~Gompertz(age,Linf,k,lag),start=c(Linf=400,k=0.25,lag=1)),silent=T),
           VBGF=try(nls(length~VBGF(age,Linf,k),start=c(Linf=400,k=0.25)),silent=T),
           stop("Error: '",model,"' model is not available")
           )
  }
}
