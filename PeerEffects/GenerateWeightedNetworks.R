
if(.Platform$OS.type=="windows"){
  setwd('G:/Copy/MicrofinanceNetworks')
} else {
  setwd('/home/daniel/Copy/MicrofinanceNetworks/')
}

level <- '_HH_' #two options household ('HH') and individual ('')
relationship <- 'allVillageRelationships' #multiple options, see the documentation

villages <- c(1,2,3,4,6,9,10,12,15,19,20,21,23,24,25,28,29,31,32,33,
             36,37,39,41,42,43,45,46,47,48,50,51,52,55,57,59,60,62,
             64,65,66,67,68,70,71,72,73,75,77)
relationships <- c('borrowmoney',
                   'giveadvice',
                   'helpdecision',
                   'keroricecome',
                   'keroricego',
                   'lendmoney',
                   'medic',
                   'nonrel',
                   'rel',
                   'templecompany',
                   'visitcome',
                   'visitgo')


for(vilno in villages){
  for(relationship in relationships){
    net <- as.matrix(
                read.csv(file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                           'AdjacencyMatrices/adj_',
                           relationship,
                           level,
                           'vilno_',
                           vilno,
                           '.csv',
                           sep=''),
                header=FALSE)
    )

    assign(paste(relationship,
             vilno,
             sep=''),
       net)
    }
  
  allweighted <- array(0,dim(net))
  for(relationship in relationships){
    allweighted <- allweighted + get(paste(relationship,vilno,sep=''))
  }
write.table(allweighted,file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                                 'AdjacencyMatrices/adj_',
                                 'allVillageWeighted',
                                 level,
                                 'vilno_',
                                 vilno,
                                 '.csv',
                                 sep=''),
          row.names=FALSE,
          col.names=FALSE,
          sep=',',
          dec='.',
          qmethod='double')
}


level <- '_' #two options household ('HH') and individual ('')
relationship <- 'allVillageRelationships' #multiple options, see the documentation

villages <- c(1,2,3,4,6,9,10,12,15,19,20,21,23,24,25,28,29,31,32,33,
              36,37,39,41,42,43,45,46,47,48,50,51,52,55,57,59,60,62,
              64,65,66,67,68,70,71,72,73,75,77)
relationships <- c('borrowmoney',
                   'giveadvice',
                   'helpdecision',
                   'keroricecome',
                   'keroricego',
                   'lendmoney',
                   'medic',
                   'nonrel',
                   'rel',
                   'templecompany',
                   'visitcome',
                   'visitgo')
ind.data <- read.dta('./DiffusionOfMicrofinance/Data/DemographicsAndOutcomes/individual_characteristics.dta')
house.data <- read.dta('./DiffusionOfMicrofinance/Data/DemographicsAndOutcomes/household_characteristics.dta')

for(vilno in villages){
  for(relationship in relationships){
    net <- as.matrix(
      read.csv(file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                          'AdjacencyMatrices/adj_',
                          relationship,
                          level,
                          'vilno_',
                          vilno,
                          '.csv',
                          sep=''),
               header=FALSE)
    )
    ids <- as.matrix(
      read.csv(file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                          'AdjacencyMatrixKeys/key_',
                          'vilno_',
                          vilno,
                          '.csv',
                          sep=''),
               header=FALSE)
    )
    
    ind.data.vilno <- ind.data[ind.data$village==vilno,]
    house.data.vilno <- house.data[house.data$village==vilno,]
    
    ### Generate a network with all households
    hhid <- house.data.vilno$hhid
    full.net <- network.initialize(length(hhid),directed=FALSE)
    full.net %v% 'vertex.names' <- hhid
    
    ### Generate a variable that indicates female repsondents
    female.key <- ind.data.vilno[ind.data.vilno$resp_gend==2,'adjmatrix_key']
    female.index <- which(ind.data.vilno$resp_gend==2)
    
    net2 <- net[female.key,female.key]
    dimnames(net2) <- list(ind.data.vilno$hhid[female.index],
                           ind.data.vilno$hhid[female.index])
    blah <- network(net2)
    as.matrix.network(blah,matrix.type='edgelist')
    as.matrix(blah)
    
    # Generate empty network of all households
    # Label vertices with houshold labels
    # Add egdes via collapsed data (indexing or edgelist)
    # Save as a matrix with no labels for use in algorithm
    
    HHid <- as.numeric(substr(ids,nchar(ids)+1-5,nchar(ids)+1-3))
    temp.net <- aggregate(net,by=list(id=HHid),FUN=sum)
    temp.net$id <- NULL
    temp.net <- aggregate(t(temp.net),by=list(id=HHid),FUN=sum)
    temp.net$id <- NULL
    temp.net <- t(temp.net)
    diag(temp.net) <- 0
    bin.net <- temp.net>0
    
    ### Will come back to this later, it will be trickier than I thought
    ### Need to use the id csv to find household ids for vertices
    
    
    
    assign(paste(relationship,
                 vilno,
                 sep=''),
           net)
  }
  
  allweighted <- array(0,dim(net))
  for(relationship in relationships){
    allweighted <- allweighted + get(paste(relationship,vilno,sep=''))
  }
  write.table(allweighted,file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                                     'AdjacencyMatrices/adj_',
                                     'allVillageWeighted',
                                     level,
                                     'vilno_',
                                     vilno,
                                     '.csv',
                                     sep=''),
              row.names=FALSE,
              col.names=FALSE,
              sep=',',
              dec='.',
              qmethod='double')
}

