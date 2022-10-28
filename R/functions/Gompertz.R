Gompertz <- function(age,Linf,k,lag){
  Linf*exp(-lag*exp(-k*age))
}
