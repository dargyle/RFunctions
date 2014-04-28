#Clean the Data
library(foreign)
library(bayesm)
library(foreach)
#source("./TicToc.r")


if(.Platform$OS.type=="windows"){
  setwd('G:/Copy/MicrofinanceNetworks')
  } else {
  setwd('/home/daniel/Copy/MicrofinanceNetworks/')
}

level <- '_HH_' #two options household ('HH') and individual ('')
relationship <- 'allVillageRelationships' #multiple options, see the documentation
vilno <- 60

### Load the control variables
# May need to do some merging of controls from the individual level file
controls <- read.dta('G:/Copy/MicrofinanceNetworks/DiffusionOfMicrofinance/Data/2. Demographics and Outcomes/household_characteristics.dta') 

### Function that takes in a village number, a matrix of whole sample controls,
### a network level (household or individual), a type of network relatiosnhip,
### and some options about doing a traditional style analysis.
### Returns a list of relelvant data

makeData <- function(vilno,controls,level,relationship,classroom.style=FALSE,remove.isolates=TRUE){
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

controls.net <- controls[controls$village==vilno,c('room_no','hhSurveyed','leader')]

#Remove Isolates
if(remove.isolates==TRUE){
  isolates <- !rowSums(net)==0
  ids <- ids[isolates]
  G <- as.matrix(net[isolates,isolates]/rowSums(net)[isolates])
  y <- as.vector(t(participation)[isolates])
  x <- as.matrix(controls.net[isolates,])
  } else {
  G <- net
  y <- as.vector(t(participation))
  x <- as.matrix(cbind(controls.net$hhSurveyed,controls.net$leader))
}

# Estimate using traditional model
if(classroom.style==TRUE){ 
  G.temp <- array(1/(nrow(G)-1),dim(G))
  diag(G.temp) <- 0
  G <- G.temp
}

Gy <- G%*%y
Gx <- G%*%x
GGx <-  G%*%G%*%x

return(data.frame(vilno=vilno,ids=ids,y=y,Gy=Gy,x=x,Gx=Gx,GGx=GGx))
}

# List the villages that had microfinance
villages <- c(1,2,3,4,6,9,10,12,15,19,20,21,23,24,25,28,29,31,32,33,
              36,37,39,41,42,43,45,46,47,48,50,51,52,55,57,59,60,62,
              64,65,66,67,68,70,71,72,73,75,77)

### Run some traditional analysis
blah <- ldply(villages,makeData,controls=controls,level=level,relationship=relationship,classroom.style=TRUE)

data.list <- list(y=as.vector(blah$y), #outcome
                  z=as.matrix(cbind(rep(1,nrow(blah)),blah[,5:13])), #instruments
                  x=as.vector(blah$Gy), #endogenous variable
                  w=as.matrix(cbind(rep(1,nrow(blah)),blah[,5:10])) #exogenous variable               
)
mcmc.list <- list(R=10000)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not

result.trad <- rivGibbs(Data=data.list,Mcmc=mcmc.list)

### Run some inital network analysis
blah <- ldply(villages,makeData,controls=controls,level=level,relationship=relationship,)

data.list <- list(y=as.vector(blah$y), #outcome
                  z=as.matrix(cbind(rep(1,nrow(blah)),blah[,5:13])), #instruments
                  x=as.vector(blah$Gy), #endogenous variable
                  w=as.matrix(cbind(rep(1,nrow(blah)),blah[,5:10])) #exogenous variable               
                  )
mcmc.list <- list(R=10000)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not

result <- rivGibbs(Data=data.list,Mcmc=mcmc.list)

data.list <- list(y=as.vector(blah$y), #outcome
                  z=as.matrix(blah[,4:13]), #instruments
                  x=as.vector(blah$Gy), #endogenous variable
                  w=as.matrix(blah[,4:10]) #exogenous variable               
)
mcmc.list <- list(R=10000)

result2 <- rivDP(Data=data.list,Mcmc=mcmc.list)
