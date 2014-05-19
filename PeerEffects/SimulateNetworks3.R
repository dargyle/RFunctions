library(statnet)
library(foreach)
library(foreign)
library(doSNOW)

if(.Platform$OS.type=="windows"){
  setwd('G:/Copy/MicrofinanceNetworks')
} else {
  setwd('/home/daniel/Copy/MicrofinanceNetworks/')
}
source("./TicToc.r")

#vilno <- 28
villages.complete <- c(28,32,33,36,37,39,42,43,45,48,
                       50,52,55,57,64,66,67,68,
                       70,71,72)

workers <- makeCluster(4)
registerDoSNOW(workers)

foreach(vilno=villages.complete,.packages=c('foreign','statnet')) %dopar% {

num.sims <- 100
### Load the control variables
# May need to do some merging of controls from the individual level file
controls <- read.dta('./DiffusionOfMicrofinance/Data/DemographicsAndOutcomes/household_characteristics2.dta') 
controls.list <- colnames(controls)

### Function that takes in a village number, a matrix of whole sample controls,
### a network level (household or individual), a type of network relatiosnhip,
### and some options about doing a traditional style analysis.
### Returns a list of relelvant data

mcmc.list <- list(R=10000)

relationship='allVillageRelationships'
level <- '_HH_' #two options household ('HH') and individual ('')
remove.isolates=FALSE
only.surveyed=TRUE

# simulateNets <- function(vilno,
#                      controls,
#                      controls.list,
#                      level,
#                      mcmc.list,
#                      num.sims,
#                      relationship='allVillageRelationships',
#                      remove.isolates=FALSE,
#                      only.surveyed=TRUE){
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
  rownames(controls.net) <- controls.net[,'adjmatrix_key']
  #class(controls.net$hohreligion) <- 'character'
  
  i <- sapply(controls.net, is.factor)
  controls.net[i] <- lapply(controls.net[i], as.character)

  #Remove Isolates
  if(only.surveyed==TRUE){
    surveyed <- controls.net[controls.net$hhSurveyed==1,'adjmatrix_key']
    G <- as.matrix(net[surveyed,surveyed])
    y <- as.vector(t(participation)[as.numeric(surveyed)])
    x <- controls.net[surveyed,]
  } else {
    G <- net
    y <- as.vector(t(participation))
    x <- controls.net
  }
  
  #Remove Isolates
#   if(remove.isolates==TRUE){
#     isolates <- !rowSums(net)==0
#     ids <- ids[isolates]
#     G <- as.matrix(net[isolates,isolates])
#     y <- as.vector(t(participation)[isolates])
#     x <- controls.net[isolates,]
#   } else {
#     G <- net
#     y <- as.vector(t(participation))
#     x <- controls.net
#   }

full.net <- network(net,vertex.attr=controls.net,matrix.type='adjacency',directed=FALSE)
est.net <- network(G,vertex.attr=x,matrix.type='adjacency',directed=FALSE)

#target.edges <- sum(degree(full.net)[surveyed]/2)
# target.caste <- sum(diag(mixingmatrix(full.net,'castesubcaste')[[2]]))
# target.nolatrine <- sum(diag(mixingmatrix(full.net,'nolatrine')[[2]]))
# target.noelectricity <- sum(diag(mixingmatrix(full.net,'noelectricity')[[2]]))
# target.leader <- sum(diag(mixingmatrix(full.net,'leader')[[2]]))

# target.caste <- sum((outer(controls.net$castesubcaste,controls.net$castesubcaste,'==')&net==1)[surveyed,surveyed])
# target.nolatrine <- sum((outer(controls.net$nolatrine,controls.net$nolatrine,'==')&net==1)[surveyed,surveyed])
# target.noelectricity <- sum((outer(controls.net$noelectricity,controls.net$noelectricity,'==')&net==1)[surveyed,surveyed])
# target.leader <- sum((outer(controls.net$leader,controls.net$leader,'==')&net==1)[surveyed,surveyed])

###Think about if these other target counts are correct? Are edges with both surveyed doubled counted while those with only one are single counted

#Of the edges we observe, what percentage are (between caste for example)
m <- network.edgecount(full.net)
pct.surveyed <- sum(controls.net$hhSurveyed==1)/length(controls.net$hhSurveyed)

obs.edges <- m
obs.caste <- sum(diag(mixingmatrix(full.net,'castesubcaste')[[2]]))
obs.nolatrine <-sum(diag(mixingmatrix(full.net,'nolatrine')[[2]]))
obs.noelectricity <- sum(diag(mixingmatrix(full.net,'noelectricity')[[2]]))
obs.leader <- sum(diag(mixingmatrix(full.net,'leader')[[2]]))

target.edges <- round(sum(degree(full.net)[surveyed])*pct.surveyed)
target.caste <- round((sum(diag(mixingmatrix(full.net,'castesubcaste')[[2]]))/m)*target.edges)
target.nolatrine <- round((sum(diag(mixingmatrix(full.net,'nolatrine')[[2]]))/m)*target.edges)
target.noelectricity <- round((sum(diag(mixingmatrix(full.net,'noelectricity')[[2]]))/m)*target.edges)
target.leader <- round((sum(diag(mixingmatrix(full.net,'leader')[[2]]))/m)*target.edges)

samp.deg.dist <- degreedist(full.net)

target.stats <- c(target.edges,
                  target.caste,
                  target.nolatrine,
                  target.noelectricity,
                  target.leader)

fit <- ergm(full.net ~ edges + 
                       nodematch('castesubcaste') + 
                       nodematch('nolatrine') + 
                       nodematch('noelectricity') + 
                       nodematch('leader'),
            target.stats = target.stats
            )

sim.fit <- simulate(fit,nsim=num.sims,
                     control = control.simulate.ergm(MCMC.interval = 10000))

save(sim.fit,target.stats,file=paste0('./Simulations/',vilno,'sims.Rdata'))
return(vilno)
}

### Generate Stata Datasets
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

makeDataSims <- function(vilno,
                     i,
                     controls,
                     controls.list,
                     classroom.style=FALSE,
                     within.global=FALSE,
                     remove.isolates=TRUE,
                     row.normalize=TRUE){
  ### Load the adjacency matrix
  load(paste0('./Simulations/',vilno,'sims.Rdata'))
  net <- as.sociomatrix(sim.fit[[i]])
  
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

### Run some traditional analysis
foreach(i=1:num.sims,.packages=c('foreign','plyr')) %dopar% {
trad.data.matrix <- ldply(villages.complete,
                          i,
                          makeDataSims,
                          controls=controls,
                          controls.list=controls.list,
                          classroom.style=TRUE,
                          within.global=FALSE,
                          row.normalize=TRUE)
write.dta(trad.data.matrix,file=paste0('./Simulations/netsims',i,'.Rdata'))
i
}

stopCluster(workers)