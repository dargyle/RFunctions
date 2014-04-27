#Clean the Data
library(foreign)
library(bayesm)
#source("./TicToc.r")


if(.Platform$OS.type=="windows"){
  setwd('G:/Copy/MicrofinanceNetworks')
  } else {
  setwd('/home/daniel/Copy/MicrofinanceNetworks/')
}

level <- '_HH_' #two options household ('HH') and individual ('')
relationship <- 'allVillageRelationships' #multiple options, see the documentation
vilno <- 1

### Load the adjacency matrix
net <- read.csv(file=paste('./DiffusionOfMicrofinance/Data/1. Network Data/',
                           'Adjacency Matrices/adj_',
                           relationship,
                           level,
                           'vilno_',
                           vilno,
                           '.csv',
                           sep=''),
                header=FALSE)

### Load the ids and use as row/column names
ids <- as.matrix(read.csv(file=paste('./DiffusionOfMicrofinance/Data/1. Network Data/',
                                  'Adjacency Matrix Keys/key',
                                  level,
                                  'vilno_',
                                  vilno,
                                  '.csv',
                                  sep=''),
                       header=FALSE)
)
dimnames(net) <- list(ids,ids)

### Load the outcomes
participation <- read.csv(file=paste('./DiffusionOfMicrofinance/Matlab Replication/India Networks/MF',
                                     vilno,
                                     '.csv',
                                     sep=''),
                          header=FALSE)

### Load the control variables
controls <- read.dta('G:/Copy/MicrofinanceNetworks/DiffusionOfMicrofinance/Data/2. Demographics and Outcomes/household_characteristics.dta') 
controls.net <- controls[controls$village==vilno,]
# May need to do some merging of controls from the individual level file

### Run some inital analysis
#Remove Isolates
isolates <- !rowSums(net)==0
net2 <- net[isolates,isolates]/rowSums(net)[isolates]

y <- as.vector(t(participation)[isolates])
Gy <- as.matrix(net2)%*%y
x <- as.matrix(cbind(controls.net$hhSurveyed[isolates],controls.net$leader[isolates]))
Gx <- as.matrix(net2)%*%x
GGx <-  as.matrix(net2)%*%as.matrix(net2)%*%x

data.list <- list(y=as.vector(y), #outcome
                  z=as.matrix(cbind(x,Gx,GGx)), #instruments
                  x=as.vector(Gy), #endogenous variable
                  w=as.matrix(cbind(x,Gx)) #exogenous variable               
                  )
mcmc.list <- list(R=10000)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not

result <- rivGibbs(Data=data.list,Mcmc=mcmc.list)
result2 <- rivDP(Data=data.list,Mcmc=mcmc.list)
