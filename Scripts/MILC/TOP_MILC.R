##########################################################################################

#                     Script to apply MILC on the generated datasets                   #

##########################################################################################

#necessary packages 
library(poLCA) 
library(confreq) 
library(dplyr) 

options(scipen = 999)                                                     #scientific notation off

nsim   = 100                                                              #number of simulation iterations
nsize  = 5000                                                             #sample size of each dataset
nboot  = 5                                                                #number of bootstrapped datasets per simulated dataset
nconds = 8                                                                #number of simulation conditions

set.seed(123)


load("simdata.RData")

source("FUN_LC4.R")                                                   

#to store results in                                                       
impdats <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
prop_x_boot <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
covar_boot  = rep(list(rep(list(array(NA,dim = c(4,4,nboot), dimnames=NULL)), nsim)), nconds)

starttime=Sys.time()
pb <- txtProgressBar(min = 0, max = nconds, style = 3, width = 50,  char = "=")   

for(i in 1:nconds){                                                             # loop over conditions
 message(paste("\nStart with condition" ,i))
  for(j in 1:nsim){                                                             # loop over simulation iterations
    
    for(k in 1:nboot){ 
      log <- capture.output({ #to avoid unnecessary messages are displayed (not all possible patterns are in the bootstrapped data)
              impdats[[i]][[j]][[k]] = posteriors(bootdat = simbootdat[[i]][[j]], k = k)  })
      
      for(l in 1: nsize){                                                       
        impdats[[i]][[j]][[k]][l,"imp"] = which(rmultinom(n  = 1, size = 1,
                      prob=impdats[[i]][[j]][[k]][l,c("1","2","3","4")]) == 1)  #impute class   
        
        
        prop_x_boot[[i]][[j]][[k]] = prop.table(table(impdatstree[[i]][[j]][[k]]$imp))
        covar_boot[[i]][[j]][,,k] = prop.table(table(impdatstree[[i]][[j]][[k]]$imp, impdatstree[[i]][[j]][[k]]$Y5))
        
        
      }
    }
  }
  save(prop_x_boot, file = "MILCproportions.RData")                          
  save(covar_boot, file = "MILCcovariates.RData")
  #important: save or move the results to the post-processing folder
  
  setTxtProgressBar(pb, i)
  Sys.time()-starttime
  
}
SimulationTotalTime = Sys.time()-starttime
SimulationTotalTime
