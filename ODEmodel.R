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
pars <- c(rr = 0.2, kk = 100, hh = 0.9)

ICVec <- seq(0, 200, by=10)
plot(x=NULL, y=NULL, xlim=c(1,max(tseq)), ylim=c(0,2*pars["kk"]), xlab="time",
     ylab="N", main="Logistic growth")
for (ii in 1:length(ICVec)) {
  init <- ICVec[ii]
  logisticOutput <- lsoda(init, tseq, logGrowthODE, pars)
  lines(logisticOutput[,1], logisticOutput[,2], col="red", type="l")
}

#Batch run

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


#Two-variable differential equation model
##The Lotka-Volterra predator-prey model

# source("LV_script.R")
# A script to run a simple 2-dimensional ODE system
# equations are
# dN/dt = r N - c N P
# dP/dt = e c N P - d P
library(deSolve)
# pred-prey cycles
pars <- c(rr = 2, cc = 0.1, ee = 0.1, dd = 0.2); init <- c(200, 20)
tseq <- seq(0, 50, by=0.02)
# define the model equations
# note that yy is now a vector with two elements for the two state variables
lvPredpreyODE <- function(tt, yy, pars) {
  derivs <-rep(NA, 2)
  derivs[1] <- pars["rr"] * yy[1] - pars["cc"] * yy[1] * yy[2]
  derivs[2] <- pars["ee"] * pars["cc"] * yy[1] * yy[2] - pars["dd"] * yy[2]
  return(list(c(derivs)))
}
lvPredpreyOutput <- lsoda( init, tseq, lvPredpreyODE, pars)
# make array of two plots
par(mfrow=c(1,2))
# plot output as time series
plot(lvPredpreyOutput[,1], lvPredpreyOutput[,2],
     col="blue", type="l",
     xlab="time", ylab="# of individuals",
     ylim = c(0,1.2*max(lvPredpreyOutput[,2])))
# add a line to the plot with the predators
points(lvPredpreyOutput[,1], lvPredpreyOutput[,3], col="red", type="l")
# add a simple legend
legText = c("Prey", "Predator")
legend("topright", legText, lty=1, col = c("blue","red"))
# plot output as a phase plot, in second subplot
plot(lvPredpreyOutput[,2], lvPredpreyOutput[,3],
     xlab="Prey", ylab="Predators", type="l", col="black")