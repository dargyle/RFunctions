#Clean the Data
library(foreign)
library(bayesm)
library(foreach)
library(plyr)
require(texreg)
#source("./TicToc.r")
source(ProcessBayesm.r)


if(.Platform$OS.type=="windows"){
  setwd('G:/Copy/MicrofinanceNetworks')
  } else {
  setwd('/home/daniel/Copy/MicrofinanceNetworks/')
}

level <- '_HH_' #two options household ('HH') and individual ('')
relationship <- 'allVillageWeighted' #multiple options, see the documentation
#vilno <- 60

### Load the control variables
# May need to do some merging of controls from the individual level file
controls <- read.dta('./DiffusionOfMicrofinance/Data/DemographicsAndOutcomes/household_characteristics.dta') 

### Function that takes in a village number, a matrix of whole sample controls,
### a network level (household or individual), a type of network relatiosnhip,
### and some options about doing a traditional style analysis.
### Returns a list of relelvant data

makeData <- function(vilno,
                     controls,
                     level,
                     relationship,
                     classroom.style=FALSE,
                     within.global=FALSE,
                     remove.isolates=TRUE,
                     row.normalize=TRUE){
### Load the adjacency matrix
net <- read.csv(file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                           'AdjacencyMatrices/adj_',
                           relationship,
                           level,
                           'vilno_',
                           vilno,
                           '.csv',
                           sep=''),
                header=FALSE)

### Load the ids and use as row/column names
ids <- as.matrix(read.csv(file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                                  'AdjacencyMatrixKeys/key',
                                  level,
                                  'vilno_',
                                  vilno,
                                  '.csv',
                                  sep=''),
                       header=FALSE)
)
dimnames(net) <- list(ids,ids)

### Load the outcomes
participation <- read.csv(file=paste('./DiffusionOfMicrofinance/MatlabReplication/IndiaNetworks/MF',
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

# Row Normalize the matix
if(row.normalize==TRUE){
  G <- G/rowSums(G)
}

# Estimate using traditional model
if(classroom.style==TRUE){ 
  G.temp <- array(1/(nrow(G)),dim(G))
  diag(G.temp) <- 0
  G <- G.temp
}

Gy <- G%*%y
Gx <- G%*%x
GGx <-  G%*%G%*%x

if(within.global==TRUE){
  n <- nrow(G)
  within.trans <- (diag(n) - array(1/n,c(n,n)))
  #within.trans <- (diag(n) - G)
  return(data.frame(vilno=vilno,
                    ids=ids,
                    y=within.trans%*%y,
                    Gy=within.trans%*%Gy,
                    x=within.trans%*%x,
                    Gx=within.trans%*%Gx,
                    GGx=within.trans%*%GGx))
  } else {
  return(data.frame(vilno=vilno,ids=ids,y=y,Gy=Gy,x=x,Gx=Gx,GGx=GGx))
  }
}
# List the villages that had microfinance
villages <- c(1,2,3,4,6,9,10,12,15,19,20,21,23,24,25,28,29,31,32,33,
             36,37,39,41,42,43,45,46,47,48,50,51,52,55,57,59,60,62,
             64,65,66,67,68,70,71,72,73,75,77)

villages <- c(1,19,48,77)

### Run some traditional analysis
blah <- ldply(villages,
              makeData,
              controls=controls,
              level=level,
              relationship=relationship,
              classroom.style=TRUE,
              within.global=TRUE)

### Get column indices for the variables (^ indicates beginning of string, $ the end)
y.index <- grep('^y$',colnames(blah))
Gy.index <- grep('^Gy$',colnames(blah))
z.index <- grep('x.',colnames(blah)) #Must include exogenous regressors with intstruments
w.index <- grep('^G{0,1}x.',colnames(blah)) #Must leave out GGx terms

data.list <- list(y=as.vector(blah[,y.index]), #outcome
                  z=as.matrix(blah[,z.index]), #instruments
                  x=as.vector(blah[,x.index]), #endogenous variable
                  w=as.matrix(blah[,w.index]) #exogenous variable               
)
mcmc.list <- list(R=10000)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not

result.trad <- rivGibbs(Data=data.list,Mcmc=mcmc.list)

### Run some inital network analysis
blah <- ldply(villages,
              makeData,
              controls=controls,
              level=level,
              relationship=relationship,
              within.global=TRUE)

data.list <- list(y=as.vector(blah$y), #outcome
                  z=as.matrix(blah[,5:13]), #instruments
                  x=as.vector(blah$Gy), #endogenous variable
                  w=as.matrix(blah[,5:10]) #exogenous variable               
)
mcmc.list <- list(R=10000)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not

result <- rivGibbs(Data=data.list,Mcmc=mcmc.list)

result2 <- rivDP(Data=data.list,Mcmc=mcmc.list)

mcmcIV <- setClass('mcmcIV',
                   slots = c(bayesm='list',
                             data.list='list'
                   ),
                   #contains = 'list'
)

### For now, only set up to report second stage
### Create an extract function for an mcmc object
extract.mcmcIV <- function(model){
  require(coda)
  # Process the data/results
  bayesm <- attributes(model)$bayesm
  
  #Combine explanatory variable results
  mcmc.model <- cbind(bayesm$beta,bayesm$gamma)
  #Assign variable names
  mcmc.names <- c('Gy',colnames(attributes(model)$data.list$w))
  colnames(mcmc.model) <- mcmc.names
  
  #Assign mcmc class
  mcmc.model <- as.mcmc(mcmc.model)
  mcmc.HPD <- HPDinterval(mcmc.model)
  
  s <- summary(mcmc.model)
  names <- colnames(mcmc.model)
  co <- s$statistics[,'Mean']
  se <- s$statistics[,'SD']
  ci.low <- mcmc.HPD[,"lower"]
  ci.up <- mcmc.HPD[,"upper"]
  
  n <- length(attributes(model)$data.list$y) 
  gof <- c(n)  
  #In future include a model convergence test, predictions?
  gof.names <- c('Num.\\ obs.')
  gof.decimal <- c(FALSE)
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    ci.low = ci.low,
    ci.up = ci.up,
    gof = gof,
    gof.names = gof.names,
    gof.decimal =gof.decimal
  )
  return(tr)
}

setMethod('extract',
          signature = className('mcmcIV'),
          definition = extract.mcmcIV)


### Make into a table
model.trad <- mcmcIV(bayesm=result.trad,data.list=data.list)
model.trad.table <- extract(model.trad)

model <- mcmcIV(bayesm=result,data.list=data.list)
model.table <- extract(model)

model.DP <- mcmcIV(bayesm=result2,data.list=data.list)
model.DP.table <- extract(model.DP)

screenreg(list(model.trad.table,model.table,model.DP.table),ci.force=TRUE,digits=3)

