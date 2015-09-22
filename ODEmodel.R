#ODEmodel.R

#Mini-exercise 4.1.1

library(deSolve)

expGrowthODE <- function(tt, yy, pars) {
  derivs <- pars["rr"] * yy
  return(list(derivs))
}

#syntax: output <- lsoda(init, tseq, ODEfunction, pars)
#init: initial value of state variable
#tseq: vector of time points
#pars: vector of parameters

init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 0.7)

expGrowthOutput <- lsoda( init, tseq, expGrowthODE, pars)

plot(expGrowthOutput[,1], expGrowthOutput[,2], col="blue", type="l")


#Mini-exercise 4.2.1

logGrowthODE <- function(tt, yy, pars) {
  derivs <- pars["rr"] * yy*(1-(yy/pars["kk"]))
  return(list(derivs))
}

init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 2, kk = 20)

logGrowthOutput <- lsoda(init, tseq, logGrowthODE, pars)

plot(logGrowthOutput[,1], logGrowthOutput[,2], col="red", type="l")


#Mini-exercise 4.2.2

logGrowthHarvODE <- function(tt, yy, pars) {
  derivs <- pars["rr"] * yy*(1-(yy/pars["kk"])-pars["hh"]*yy)
  return(list(derivs))
}

init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 2, kk = 100, hh = 0.9)

logGrowthHarvOutput <- lsoda(init, tseq, logGrowthHarvODE, pars)

plot(logGrowthHarvOutput[,1], logGrowthHarvOutput[,2], col="green", type="l")

###############################################
#Sensitivity analysis


init <- 1
tseq <- seq(0, 20, by=0.01)
pars <- c(rr = 0.8, kk = 100, hh = 0.9)

ICVec <- seq(0, 200, by=10)
plot(x=NULL, y=NULL, xlim=c(1,max(tseq)), ylim=c(0,2*pars["kk"]), xlab="time",
     ylab="N", main="Logistic growth")
for (ii in 1:length(ICVec)) {
  init <- ICVec[ii]
  logisticOutput <- lsoda(init, tseq, logGrowthODE, pars)
  lines(logisticOutput[,1], logisticOutput[,2], col="red", type="l")
}

index <- which(logisticOutput[,1]==5)
nnVec <- rep(NA, 1, length(ICVec))

for (ii in 1:length(ICVec)) {
  init <- ICVec[ii]
  logisticOutput <- lsoda(init, tseq, logGrowthODE, pars)
  nnVec[ii] <- logisticOutput[index,2]
}

x11() # if needed
plot(ICVec, nnVec, xlab="Initial population size",
     ylab="Population at time=5", type="b", col="red")


