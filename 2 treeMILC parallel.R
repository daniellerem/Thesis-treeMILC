#2_TOP_treeMILC

#packages
library(poLCA) 
library(confreq) 
library(dplyr) 
library(parallel)
library(doParallel)
library(doRNG)

nsim   = 100  
nsize  = 5000 
nboot  = 5
nconds = 8

set.seed(123)
load("Simdata.RData")
source("2b_LCmodel2.R")                                                   
source("2c_LCmodel3.R")                                                   

impdats2 <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
impdats3 <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
simbootdat2subset <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
rownrsubset <-  rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
impdatstree <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)

ordat     <- rep(list(vector("list", nsim)), nconds)
sub_ordat <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
posdat    <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
subset    <- rep(list(rep(list(vector("list", nboot)),nsim)), nconds)

prop_x_boot = rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
covar_boot  = rep(list(rep(list(array(NA,dim = c(4,4,nboot), dimnames=NULL)), nsim)), nconds)


starttime=Sys.time() 

#Code necessary for parallelizing over the conditions
#note: #the for each loop parallelizes over the 8 conditions, so having 2, 4, or 8 cores is useful to speed up the process 
myCluster <- parallel::makeCluster(detectCores()-1, type="PSOCK")               
RNGkind("L'Ecuyer-CMRG")
registerDoParallel(myCluster)
clusterSetRNGStream(cl=myCluster)
set.seed(123)
clusterExport(cl=myCluster, varlist=c('nsim', 'nsize', 'nboot', 'nconds', 'prop_x_boot', "covar_boot", 'prop_x_boot_true', "covar_boot_true"))
clusterExport(cl=myCluster, varlist=c('impdats2', 'impdats3', 'impdatstree', 'rownrsubset', 'simbootdat2subset', 'posdat', 'subset', 'sub_ordat'))

comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}                                                                               #function to have a list of lists as output
    
res_tree <-  foreach(i=1:nconds, 
                     .maxcombine = nsim, 
                     .multicombine = T, 
                     .packages=c("poLCA", "confreq", "dplyr", "resample", "parallel"), 
                     .combine="comb", .init=list(list(), list()) )%dorng%{   
  source("2b_LCmodel2.R")                                                   
  source("2c_LCmodel3.R") 
  #START
  for(j in 1:nsim){                                                             
    
    for(k in 1:nboot){ 
      
      #SELECTION ERROR PART
      impdats2[[i]][[j]][[k]]     <-  posteriors2(bootdat = simbootdat[[i]][[j]], k = k)  
      for(l in 1: nsize){impdats2[[i]][[j]][[k]][l,"imp"] = which(rmultinom(n  = 1, size = 1, prob=impdats2[[i]][[j]][[k]][l,c("1","2")]) == 1)}   

      #make subset of units whose imputed LC is 2
      rownrsubset[[i]][[j]][[k]]  <-  which(impdats2[[i]][[j]][[k]][,"imp"]==2)                #retrieve rownumbers in original df for the subset
      subset[[i]][[j]][[k]]       <-  impdats2[[i]][[j]][[k]][rownrsubset[[i]][[j]][[k]],]       #rownumbers of subset are the same as in the impdats2 set
      
      #MEASUREMENT ERROR PART
      impdats3[[i]][[j]][[k]]     <-  posteriors3(bootdat=simbootdat[[i]][[j]], subsetrows=rownrsubset[[i]][[j]][[k]], k = k)   
      #rownursubset is the list with id/row numbers. to get the correct rows in the orginal dataframe 'l' has to be a character instead of a number 
      for(l in unlist(rownrsubset[[i]][[j]][[k]])){                             #impute subset
        impdats3[[i]][[j]][[k]][as.character(l),"imp"] <-  which(rmultinom(n  = 1, size = 1, 
                                                           prob=impdats3[[i]][[j]][[k]][as.character(l),c("2","3","4")]) == 1)+1 }  
      #+1 is added to the imputed LCs so they are named 2, 3, and 4 and avoid overlap with LC1 from SEl-error part
      
      
      #COMBINE RESULTS
      impdatstree[[i]][[j]][[k]]  <- impdats2[[i]][[j]][[k]]   
      for(l in rownrsubset[[i]][[j]][[k]]){impdatstree[[i]][[j]][[k]][as.character(l),"imp"] <- impdats3[[i]][[j]][[k]][as.character(l),"imp"]}           
      
      #extract results
      prop_x_boot[[i]][[j]][[k]]  <-  prop.table(table(impdatstree[[i]][[j]][[k]]$imp))
      covar_boot[[i]][[j]][,,k]   <-  prop.table(table(impdatstree[[i]][[j]][[k]]$imp, impdatstree[[i]][[j]][[k]]$Y5))

    }#end loop over nboot
  }#end loop over nsim
  return(list(prop_x_boot,covar_boot))
}#end loop over conditions

SimulationTotalTime = Sys.time()-starttime
SimulationTotalTime


  prop_x_boot     <-  rep(list(rep(list(vector("list", nboot)),nsim)), nconds)
  covar_boot      <-  rep(list(rep(list(array(NA,dim = c(4,4,nboot), dimnames=NULL)), nsim)), nconds)

for(i in 1:nconds){                                                             # loop over conditions
  cat(i)
  for(j in 1:nsim){                                                             # loop over simulation iterations
    for(k in 1:nboot){ 
        prop_x_boot[[i]][[j]][[k]]  <- res_tree[[1]][[i]][[i]][[j]][[k]]
        covar_boot[[i]][[j]][,,k]   <- res_tree[[2]][[i]][[i]][[j]][,,k]
    }
  }
}


stopCluster(myCluster)
