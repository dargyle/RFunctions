
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

