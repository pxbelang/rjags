#Load libraries
library(spdep)
library(INLA)

#Load data from a shapefile included in the spdep package
nc.sids <- readShapePoly(system.file("etc/shapes/sids.shp", package="spdep")[1])

#Create adjacency matrix
nc.nb <- poly2nb(nc.sids)

#Compute expted number of cases
nc.sids$EXP<-nc.sids$BIR74*sum(nc.sids$SID74)/sum(nc.sids$BIR74)

#Compute proportion of non-white births
nc.sids$NWPROP<-nc.sids$NWBIR74/nc.sids$BIR74

#Convert the adjacency matrix into a file in the INLA format
nb2INLA("nc.adj", nc.nb)

#Create areas IDs to match the values in nc.adj
nc.sids$ID<-1:100
m1<-inla(SID74~NWPROP+f(nc.sids$ID, model="besag", graph="nc.adj"),
         family="poisson", E=nc.sids$EXP, data=as.data.frame(nc.sids),
         control.predictor=list(compute=TRUE))

#Alternatively, a sparse matrix can be used
adjmat<-as(nb2mat(nc.nb, style="B"), "dgTMatrix") #Binary adjacency matrix
m2<-inla(SID74~NWPROP+f(nc.sids$ID, model="besag", graph=adjmat),
         family="poisson", E=nc.sids$EXP, data=as.data.frame(nc.sids),
         control.predictor=list(compute=TRUE))


#Get realtive risk estimates
nc.sids$RR1<-m1$summary.fitted.values[,1]
nc.sids$RR2<-m2$summary.fitted.values[,1]

#Display relative risk estimates to show that both examples fit the same model
spplot(nc.sids, c("RR1", "RR2"))
