#Pseudo-code exercise

#################################

#question:
#The geometric growth model is the simplest model for population
#growth in discrete time. It assumes that every year the size of the
#population changes by the same factor, R.
#N(t+1) = R * N(t)
#Exercise: write pseudo-code for a program that simulates the
#growth of a population for 10 years, starting with N=100 animals and
#assuming R = 1.05, and prints the final population size.

#ans:
#assign N=100 and R=1.05
#for loop over t for 10 years
  #assign N to R*N 
#print final N

N <-100
R <-1.05
for(t in 1:10){
  N = R*N
}
print(N)

#Bonus: modify the pseudo-code so the program will make a plot of
#N versus t.

#ans:
#assign N=100 and R=1.05
#initialize a vector NN to hold all values of N through time (fill it with NA values, set first element to 100)
#for loop over t for 10 years
  #assign each element of NN to R*NN
#plot the NN vector against time

#geometricGrowthScrip.R
#a script to simulate and plot the discrete logistic model

#setup: none needed

#set initial conditions and parameter values
N0 <- 100
RR <- 1.05
ttmax<- 10

#initialize variable to a vector of NA values
NN <- matrix(NA, nrow = 1, ncol = ttmax+1)
NN[1] <- N0

#use a loop to iterate the model the desired number of times
for(tt in 1:ttmax){
  NN[tt+1] <- RR*NN[tt]
}
print(NN)
plot(1:(ttmax+1),NN, xlab="time", ylab="N", col="blue")
