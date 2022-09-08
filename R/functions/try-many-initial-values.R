# FUNCITON TO TRY MANY DIFFERENT SETS OF STARTING VALUES IN NLSLM
# dataset: two columns, named x (length) and y (reproductive measure)

# returns the a and b coefficients of the best fit found
try_many_inits = function(dataset) {
  
  # number of initial values to try
  n_inits = 1000
  
  # initial values for a and b
  init_a = runif(n_inits, 0, 1)
  init_b = runif(n_inits, 1, 5)
  
  # containers to store the coefficients and residual sum of squares at each set of initial values
  ssq = numeric(n_inits)
  coefs = matrix(NA, n_inits, 2)
  fit = list()
  
  # the power function
  POW2<-function(L,a,b) { a*L^b }	
  
  # loop through initial values, fitting the model for each and storing the coefs and ssq
  for (i in 1:n_inits) {
    
    # print a progress indicator
    cat("\rProgress: ", round(i/n_inits * 100), "%", sep = "")
    
    # some sets of initial values may give fitting errors (e.g., singular gradient)
    # prevent a crash if this happens by catching the error
    fit[[i]] = tryCatch(
      expr = nlsLM(y~POW2(x,a,b),data=dataset,start=list(a=init_a[i], b=init_b[i]), control=nls.control(maxiter=999)),
      error = function(e) NA
    )
    
    # summarize output for this set of inits
    # different task depending on whether error was returned
    if (length(fit[[i]]) == 1) {
      ssq[i] = NA
      coefs[i,] = NA
    } else {
      ssq[i] = sum(resid(fit[[i]])^2)
      coefs[i,] = coef(fit[[i]])
    }
  }; cat("\n")
  
  # return the fitted model object of the best fit
  best = fit[[which.min(ssq)]]
  return(best)
}