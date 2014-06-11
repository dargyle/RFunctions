
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

    # Limit to female nominated ties    
    ind.data.vilno <- ind.data[ind.data$village==vilno,]
    female.id <- ind.data.vilno[ind.data.vilno$resp_gend==2,"adjmatrix_key"]
    fem.net <- array(0,dim(net))
    fem.net[female.id,] <- net[female.id,]
    fem.net[,female.id] <- net[,female.id]
    
    HHid <- as.numeric(substr(ids,nchar(ids)+1-5,nchar(ids)+1-3))
    temp.net <- aggregate(fem.net,by=list(id=HHid),FUN=sum)
    temp.net$id <- NULL
    temp.net <- aggregate(t(temp.net),by=list(id=HHid),FUN=sum)
    temp.net$id <- NULL
    temp.net <- t(temp.net)
    diag(temp.net) <- 0
    bin.fem.net <- temp.net>0
    
  write.table(bin.fem.net,file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                                     'AdjacencyMatrices/adj_',
                                     'allVillageRelationshipsFemale',
                                     '_HH_',
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

### Do the same thing for male networks
for(vilno in villages){
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
  
  # Limit to female nominated ties    
  ind.data.vilno <- ind.data[ind.data$village==vilno,]
  male.id <- ind.data.vilno[ind.data.vilno$resp_gend==1,"adjmatrix_key"]
  m.net <- array(0,dim(net))
  m.net[male.id,] <- net[male.id,]
  m.net[,male.id] <- net[,male.id]
  
  HHid <- as.numeric(substr(ids,nchar(ids)+1-5,nchar(ids)+1-3))
  temp.net <- aggregate(m.net,by=list(id=HHid),FUN=sum)
  temp.net$id <- NULL
  temp.net <- aggregate(t(temp.net),by=list(id=HHid),FUN=sum)
  temp.net$id <- NULL
  temp.net <- t(temp.net)
  diag(temp.net) <- 0
  bin.m.net <- temp.net>0
  
  write.table(bin.m.net,file=paste('./DiffusionOfMicrofinance/Data/NetworkData/',
                                     'AdjacencyMatrices/adj_',
                                     'allVillageRelationshipsMale',
                                     '_HH_',
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
