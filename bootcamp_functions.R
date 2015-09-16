#bootcamp_functions.R
#writing my first function

#geometric growth model function
geometricGrowthFun <- function(N0, RR, ttmax=10){
  #initialize variable to a vector of NA values
  NN <- matrix(NA, nrow = 1, ncol = ttmax+1)
  NN[1] <- N0
  
  #use a loop to iterate the model the desired number of times
  for(tt in 1:ttmax){
    NN[tt+1] <- RR*NN[tt]
  }
  print(NN)
  plot(1:(ttmax+1),NN, xlab="time", ylab="N", col="blue")
}