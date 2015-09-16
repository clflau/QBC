#discrete_logistic.R

discreteLogisticFun <- function(N0, Rd, KK, ttmax=10){
  #initialize variable to a vector of NA values
  NN <- matrix(NA, nrow = 1, ncol = ttmax+1)
  NN[1] <- N0
  
  #use a loop to iterate the model the desired number of times
  for(tt in 1:ttmax){
    NN[tt+1] <- NN[tt]*(1+(Rd*(1-NN[tt]/KK)))
  }
  return(print(NN))
  plot(1:(ttmax+1),NN, xlab="time", ylab="N", col="blue")
}

#mini exercise 3.3.1

discreteLogisticFun(N0=10, Rd=0.5, KK=100)
discreteLogisticFun(N0=10, Rd=0.9, KK=100)
discreteLogisticFun(N0=10, Rd=0.1, KK=100)
