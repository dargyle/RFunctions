#Clean the Data
library(foreign)
library(doSNOW)
library(bayesm)
library(foreach)
library(plyr)
require(texreg)
library(statnet)
library(coda)
#source(ProcessBayesm.r)


if(.Platform$OS.type=="windows"){
  setwd('G:/Copy/MicrofinanceNetworks')
  } else {
  setwd('/home/daniel/Copy/MicrofinanceNetworks/')
}
source("./TicToc.r")

level <- '_HH_' #two options household ('HH') and individual ('')
#relationship <- 'allVillageWeighted' #multiple options, see the documentation
relationship <- 'allVillageRelationships' #multiple options, see the documentation

### Load the control variables
# May need to do some merging of controls from the individual level file
controls <- read.dta('./DiffusionOfMicrofinance/Data/DemographicsAndOutcomes/household_characteristics2.dta') 
controls.list <- c('leader','room_no','bed_no','hhSurveyed','noelectricity','nolatrine','GMorOBC')

### Function that takes in a village number, a matrix of whole sample controls,
### a network level (household or individual), a type of network relatiosnhip,
### and some options about doing a traditional style analysis.
### Returns a list of relelvant data

makeData <- function(vilno,
                     controls,
                     controls.list,
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

controls.net <- controls[controls$village==vilno,controls.list]
controls.net$evcent <- evcent(net)

#Remove Isolates
if(remove.isolates==TRUE){
  isolates <- !rowSums(net)==0
  ids <- ids[isolates]
  G <- as.matrix(net[isolates,isolates])
  y <- as.vector(t(participation)[isolates])
  x <- as.matrix(controls.net[isolates,])
  } else {
  G <- net
  y <- as.vector(t(participation))
  x <- as.matrix(controls.net)
}

# Row Normalize the matix
if(row.normalize==TRUE){
  G <- G/rowSums(G)
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

makeDataList <- function(blah,DP=FALSE){
### Get column indices for the variables (^ indicates beginning of string, $ the end)
y.index <- grep('^y$',colnames(blah))
x.index <- grep('^Gy$',colnames(blah))
z.index <- grep('x.',colnames(blah)) #Must include exogenous regressors with intstruments
w.index <- grep('^G{0,1}x.',colnames(blah)) #Must leave out GGx terms

if(DP==TRUE){
data.list <- list(y=as.vector(blah[,y.index]), #outcome
                  z=as.matrix(blah[,z.index]), #instruments
                  x=as.vector(blah[,x.index]), #endogenous variable
                  w=as.matrix(blah[,w.index]) #exogenous variable               
)
} else {
  data.list <- list(y=as.vector(blah[,y.index]), #outcome
                    z=as.matrix(cbind(blah[,z.index],cons=1)), #instruments
                    x=as.vector(blah[,x.index]), #endogenous variable
                    w=as.matrix(cbind(blah[,w.index],cons=1)) #exogenous variable  
  )
}
return(data.list)
}

# List the villages that had microfinance
# villages <- c(1,2,3,4,6,9,10,12,15,19,20,21,23,24,25,28,29,31,32,33,
#             36,37,39,41,42,43,45,46,47,48,50,51,52,55,57,59,60,62,
#             64,65,66,67,68,70,71,72,73,75,77)

#villages <- c(28,39,48,75)

villages.complete <- c(28,32,33,36,37,39,42,43,45,48,
                       50,52,55,57,64,66,67,68,
                       70,71,72)
villages <- villages.complete
#villages <- c(28,32,33,36)

### Run some traditional analysis
trad.data.matrix <- ldply(villages,
                          makeData,
                          controls=controls,
                          controls.list=controls.list,
                          level=level,
                          relationship='allVillageRelationships',
                          classroom.style=TRUE,
                          within.global=FALSE,
                          row.normalize=TRUE)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not
trad.data.list <- makeDataList(trad.data.matrix,DP=TRUE)

### Run some traditional analysis
binary.data.matrix <- ldply(villages,
                          makeData,
                          controls=controls,
                          controls.list=controls.list,
                          level=level,
                          relationship='allVillageRelationships',
                          classroom.style=FALSE,
                          within.global=TRUE,
                          row.normalize=TRUE)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not
binary.data.list <- makeDataList(binary.data.matrix,DP=TRUE)

### Run some traditional analysis
weighted.data.matrix <- ldply(villages,
                            makeData,
                            controls=controls,
                            controls.list=controls.list,
                            level=level,
                            relationship='allVillageWeighted',
                            classroom.style=FALSE,
                            within.global=TRUE,
                            row.normalize=TRUE)

### IMPORTANT: rivGibbs requires an intercept (unless I demean it), rivDP does not
weighted.data.list <- makeDataList(weighted.data.matrix,DP=TRUE)

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
  #mcmc.model <- as.mcmc.list(bayesm$beta,bayesm$gamma)
  #Assign variable names
  mcmc.names <- c('Gy',colnames(attributes(model)$data.list$w))
  #names(mcmc.model) <- mcmc.names
  
  s <- rbind(summary(as.mcmc.list(bayesm$beta))$statistics,
             summary(as.mcmc.list(bayesm$gamma))$statistics)
  quant <- rbind(summary(as.mcmc.list(bayesm$beta))$quantiles,
                 summary(as.mcmc.list(bayesm$gamma))$quantiles)
  
  names <- mcmc.names
  co <- s[,'Mean']
  se <- s[,'SD']
  ci.low <- quant[,'2.5%']
  ci.up <- quant[,'97.5%']
  
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

workers <- makeCluster(4)
registerDoSNOW(workers)

runs <- 4
mcmc.param <- list(R=20000)
tic()
parallel <- foreach(i=1:runs,.packages='bayesm') %dopar% rivGibbs(Data=binary.data.list,
                                           Mcmc=mcmc.param)
toc()
tic()
parallel.DP <- foreach(i=1:runs,.packages='bayesm') %dopar% rivDP(Data=binary.data.list,
                                                                  Mcmc=mcmc.param)
toc()
stopCluster(workers)

extractParallel <- function(parallel){
betas <- vector(mode='list',length=runs)
gammas <- vector(mode='list',length=runs)
deltas <- vector(mode='list',length=runs)
Istars <- vector(mode='list',length=runs)
for(i in 1:runs){
  betas[[i]] <- parallel[[i]]$beta
  gammas[[i]] <- parallel[[i]]$gamma
  deltas[[i]] <- parallel[[i]]$delta
  if(!is.null(parallel[[i]]$Istar)) Istars[[i]] <- as.mcmc(parallel[[i]]$Istar)
}
result <- list(deltadraw=mcmc.list(deltas),
               betadraw=mcmc.list(betas),
               gammadraw=mcmc.list(gammas),
               if(is.null(Istars[[1]])) Istar=NULL else Istar=mcmc.list(Istars))
return(result)
}

result <- extractParallel(parallel)
result.DP <- extractParallel(parallel.DP)

model.parallel <- mcmcIV(bayesm=result,data.list=binary.data.list)
model.parallel.table <- extract(model.parallel)

model.parallel.DP <- mcmcIV(bayesm=result.DP,data.list=binary.data.list)
model.parallel.DP.table <- extract(model.parallel.DP)

# ### Make into a table
# model.trad <- mcmcIV(bayesm=result.trad,data.list=trad.data.list)
# model.trad.table <- extract(model.trad)
# 
# model.binary <- mcmcIV(bayesm=result.binary,data.list=binary.data.list)
# model.binary.table <- extract(model.binary)
# 
# model.weighted <- mcmcIV(bayesm=result.weighted,data.list=weighted.data.list)
# model.weighted.table <- extract(model.weighted)

screenreg(list(model.parallel.table,model.parallel.DP.table),ci.force=TRUE,digits=3)
texreg(list(model.parallel.table,model.parallel.DP.table),
       file='bayesian.tex',
       ci.force=TRUE,
       digits=3,
       caption='Bayesian Estimation',
       caption.above=TRUE)
