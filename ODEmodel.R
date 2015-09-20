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







