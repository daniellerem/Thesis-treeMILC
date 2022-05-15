##########################################################################################

#        PARALLELISED version of   Script to apply MILC on the generated datasets       #

##########################################################################################




options(scipen = 999)                                                     #scientific notation off

nsim   = 100                                                              #number of simulation iterations
nsize  = 5000                                                             #sample size of each dataset
nboot  = 5                                                                #number of bootstrapped datasets per simulated dataset
nconds = 8                                                                #number of simulation conditions

set.seed(123)


#PACKAGES/REQUIREMENTS
library(poLCA) 
library(confreq) 
library(dplyr) 
library(resample)
library(parallel)
library(doParallel)
library(doRNG)


starttime=Sys.time()


impdats <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)

prop_x_boot = rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
covar_boot  = rep(list(rep(list(array(NA,dim = c(4,4,nboot), dimnames=NULL)), nsim)), nconds)

load("simdata.RData")                                                                                   #load data

#code necessary for parallelizing
myCluster <- parallel::makeCluster(4, type="PSOCK")   
RNGkind("L'Ecuyer-CMRG")
registerDoParallel(myCluster)
clusterSetRNGStream(cl=myCluster)
set.seed(123)
clusterExport(cl=myCluster, varlist=c('nsim', 'nsize', 'nboot', 'nconds', 'prop_x_boot', "covar_boot"))
clusterExport(cl=myCluster, varlist=c('impdats', 'simbootdat'))
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))            #function to combine results in a list of lists
}



#START
resMILC <-  foreach(i=1:nconds, 
                    .maxcombine = nsim, 
                    .multicombine = T, 
                    .packages=c("poLCA", "confreq", "dplyr", "resample", "parallel", "bigstatsr"), 
                    .combine="comb", 
                    .init=list(list(), list() ) )%dorng%{ 
source("FUN_LC4.R")                                                   
  for(j in 1:nsim){                                                            
    
    for(k in 1:nboot){ 
      impdats[[i]][[j]][[k]] = posteriors(bootdat = simbootdat[[i]][[j]], k = k)     #LC part of MILC: apply LC model to calculate posteriors
      
      for(l in 1: nsize){                                                       
        impdats[[i]][[j]][[k]][l,"imp"] = which(rmultinom(n  = 1, 
                                                          size = 1,
                                                          prob=impdats[[i]][[j]][[k]][l,c("1","2","3","4")]) == 1)  }
                                                                                   #MI part of MILC: imputation of LC   
        #extract results
        prop_x_boot[[i]][[j]][[k]] <-  prop.table(table(impdats[[i]][[j]][[k]]$imp))
        covar_boot[[i]][[j]][,,k] <-  prop.table(table(impdats[[i]][[j]][[k]]$imp, impdats[[i]][[j]][[k]]$Y5))  
         }
  }
  results=list(prop_x_boot, covar_boot)
  return(results)
}
SimulationTotalTime = Sys.time()-starttime
SimulationTotalTime

#remove empty lists and store both estimates in seperate lists of lists named prop_x_boot and covar_boot

for(i in 1:nconds){  
  for(j in 1:nsim){                                                             
    for(k in 1:nboot){
      prop_x_boot[[i]][[j]][[k]] <- resMILC[[1]][[i]][[i]][[j]][[k]]
      covar_boot[[i]][[j]][,,k]  <- resMILC[[2]][[i]][[i]][[j]][,,k]
      }}}
save(prop_x_boot, file = "MILCproportions.RData")                          
save(covar_boot, file = "MILCcovariates.RData")
#important: save or move the results to the post-processing folder

