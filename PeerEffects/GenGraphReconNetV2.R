library(statnet)
library(ergm)
library(foreach)
library(doSNOW)

if(.Platform$OS.type=="windows"){
  setwd('G:/Copy/MicrofinanceNetworks')
} else {
  setwd('/home/daniel/Copy/MicrofinanceNetworks/')
}
source("./TicToc.r")

#foreach network in villages.complete <- c(28,32,33,36,37,39,42,43,45,48,
#50,52,55,57,64,66,67,68,
#70,71,72)

#Foreach run in A-D

foreach(i=1:num.sims,.packages=c('statnet','abind')) %dopar% {

for(run in LETTERS[1:4]){
  load(paste0("G:/Copy/MicrofinanceNetworks/Simulations/",run,village,"sims.Rdata"))
  tic()
  blah2 <- as.sociomatrix(sim.fit, simplify=FALSE, 
                        #constraint=~observed
  )
  toc()

  tic()
  thing <- abind(blah2,along=3)
  toc()

  # Find simulated probability matrix with simple weighting
  #matrix of the percentage of times a link appears
  assign(paste0(run,village),
         apply(thing,1:2,sum,na.rm=TRUE))
}
### Add up the 4 sum mat
sum.mat <- get(paste0('A',village)) + get(paste0('B',village)) + get(paste0('C',village)) + get(paste0('D',village))
### Make probability matrix, and done!
prob.mat <- sum.mat/max(sum.mat)

write.table(prob.mat,file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                                   'AdjacencyMatrices/adj_',
                                   'allReconstructed',
                                   '_HH_',
                                   'vilno_',
                                   village,
                                   '.csv',
                                   sep=''),
            row.names=FALSE,
            col.names=FALSE,
            sep=',',
            dec='.',
            qmethod='double')

}
