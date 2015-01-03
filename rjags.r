library(pscl)
data(absentee)
attach(absentee)
require(rjags)

y <- (absdem - absrep) / (absdem + absrep) * 100
x <- (machdem - machrep) / (machdem + machrep) * 100

forJags <- list(y=y[1:20], x=x[1:20], n=20, xstar=x[21])
inits <- list(list(beta=c(0,0), sigma=5))

foo <- jags.model(file="rjags.bug", data=forJags, inits=inits)

out <- coda.samples(model=foo, variable.names=c("beta", "sigma"),
                    n.iter=50000, thin=5)
