require(bayesm)
require(texreg)

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
  mcmc.names <- c('GY',colnames(attributes(model)$data.list$w))
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

