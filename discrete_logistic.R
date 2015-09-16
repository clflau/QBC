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


#mini exercise 3.3.2

#make vector for each Rd value
vectRd = c(-0.3, 0.3, 1.3, 1.9, 2.2, 2.7)

par(mfrow = c(2, 3))

#loop discreteLogisticFun over each element of vectRd
for (ii in 1:length(vectRd)){
  discreteLogisticFun(N0 = 10, K = 100, Rd = ii)
}

