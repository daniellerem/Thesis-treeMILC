

options(scipen = 999)                                                     #scientific notation off

nsim   = 100                                                              #number of simulation iterations
nsize  = 5000                                                             #sample size of each dataset
nboot  = 5                                                                #number of bootstrapped datasets per simulated dataset
nconds = 8                                                                #number of simulation conditions

set.seed(123)

#load function used for the dataset generation
source("1a_specify_simconditions.R")                                            
source("1b_simulate_dataset.R")
source("1c_generate_bootstraps.R")

#load packages necessary for the dataset generation
library(poLCA) 
library(confreq) 
library(dplyr) 

sim_conds  = simconds()                                                          
simbootdat = vector("list", nconds)

#DATASET GENERATION
for(i in 1:nconds){                                                             # loop over conditions
  cat(i)
  for(j in 1:nsim){                                                             # loop over simulation iterations
    
    sim_data = simdata(nsize      = nsize,                                      # generate a simulated dataset
                       sel_error  = sim_conds[[i]][[1]],
                       meas_error = sim_conds[[i]][[2]])
    
    
    simbootdat[[i]][[j]] = bootdata(data  = sim_data,                           # bootstraps of the simulated dataset
                                    nboot = nboot)
    
  }
}
save(simbootdat, file = "simdata.RData")


#EXTRACT SIMULATED POPULATION VALUES
longdat          = rep(list(vector("list", nsim)),nconds)                        #create empty lists to store the       
prop_x_nsim_true = rep(list(vector("list", nsim)),nconds)                        #(intermediate) results in
covar_nsim_true  = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
prop_true        = vector("list", nconds)
covar_true       = vector("list", nconds)


for(i in 1:nconds){                                                             
  cat(i)
  for(j in 1:nsim){     
    log <- capture.output({ #make sure not all messages are displayed about the pattern frequencies
      longdat[[i]][[j]]          <-    as.data.frame(fre2dat(simbootdat[[i]][[j]][,c(1:7)]))    #transfer pattern frequencies to longformat dataframe (column 7 contains the pattern frequencies original data)
    })
    prop_x_nsim_true[[i]][[j]]   <-  prop.table(table(longdat[[i]][[j]]$trueclass))
    covar_nsim_true[[i]][,,j]    <-  prop.table(table(longdat[[i]][[j]]$trueclass, longdat[[i]][[j]]$Y5))
  }#end loop over nsim
  covar_true[[i]]                <-  apply(covar_nsim_true[[i]], MARGIN = c(1, 2), FUN = mean)   # average over nsim
  prop_true[[i]]                 <-  colMeans(bind_rows(prop_x_nsim_true[[i]][[j]])) 
}#end loop over nconds

save(covar_true, file = "SimulatedCovariates.RData")                            #the class size estimates
save(prop_true,  file = "SimulatedProportions.RData")                           #the class-covariate estimates
