
##########################################################################################

#       Script to process results of MILC and tree-MILC: calculate performance measures  #

##########################################################################################


#1. SET UP 
options(scipen = 999)                                                     #scientific notation off

nsim   = 100                                                              #number of simulation iterations
nsize  = 5000                                                             #sample size of each dataset
nboot  = 5                                                                #number of bootstrapped datasets per simulated dataset
nconds = 8                                                                #number of simulation conditions

set.seed(123)


#load necessary packages
library(poLCA) 
library(confreq) 
library(dplyr) 
library(resample)
library(parallel)

#load functions
source("FUN_prop_estimates.R")
source("FUN_cov_estimates.R")

#MILC estimates         - originally in SCRIPTS/MILC folder
load("MILCProportions.RData")                               #the class size estimates  
load("MILCCovariates.RData")                                #the class-covariate estimates

#tree-MILC              - originally in SCRIPTS/tree-MILC folder
load("treeMILCProportions.RData")                           #the class size estimates  
load("treeMILCCovariates.RData")                            #the class-covariate estimates


#Population estimates  - originally in DATA folder
load("SimulatedProportions.RData")                          #the class size estimates  
load("SimulatedCovariates.RData")                           #the class-covariate estimates



#2. Create empty storage for (intermediate) results
prop_x_nsim = rep(list(vector("list", nsim)), nconds)
bias_x_nsim = rep(list(vector("list", nsim)), nconds)
prop_x_nsimTREE = rep(list(vector("list", nsim)), nconds)
bias_x_nsim_sq = rep(list(vector("list", nsim)), nconds)
bias_x_nsim = rep(list(vector("list", nsim)), nconds)
bias_x_nsim_abs = rep(list(vector("list", nsim)), nconds)
bias_x_rel_nsim = rep(list(vector("list", nsim)), nconds)
bias_x      = vector("list", nconds)
bias_x_abs      = vector("list", nconds)
bias_x_rel  = vector("list", nconds)
res_x      = vector("list", nconds)
cov_x       = vector("list", nconds)
cov_z       = vector("list", nconds)
rmse_z      = vector("list", nconds)
rmse_x      = vector("list", nconds)
sesd_x      = vector("list", nconds)
sesd_z      = vector("list", nconds)
ciwidth     = vector("list", nconds)
totalvariance  = vector("list", nconds)
cover       = vector("list", nconds)
testie      = vector("list", nconds)  
coverage_z  = vector("list", nconds)
ciwidth_z   = vector("list", nconds)


covar_nsim         = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias    = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias_rel = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias2    = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias_sq = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_bias         = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_bias_rel     = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_var          = vector("list", nconds)
covar_res          = vector("list", nconds)



#3. Calculate performance measures

for (i in 1:nconds) {
    cat(i)
  for(j in 1:nsim){                                                            # loop over simulation iterations
    #3a pool results 
    prop_x_nsim[[i]][[j]]      = colMeans(bind_rows(prop_x_boot[[i]][[j]]))    # class proportions per iteration
    bias_x_nsim_abs[[i]][[j]]  = abs(prop_x_nsim[[i]][[j]] - prop_xTRUE[[i]])  # absolute bias per iteration
    bias_x_nsim_sq[[i]][[j]]   = (prop_x_nsim[[i]][[j]] - prop_xTRUE[[i]])^2   # absolute bias per iteration
    bias_x_rel_nsim[[i]][[j]]  = abs(prop_x_nsim[[i]][[j]] - prop_xTRUE[[i]])/prop_xTRUE[[i]]  # absolute bias per iteration
    covar_nsim[[i]][,,j]       = apply(covar_boot[[i]][[j]], MARGIN = c(1, 2), FUN = mean)     # average over bootstraps
    covar_nsim_bias[[i]][,,j]  = abs(covar_nsim[[i]][,,j]- covar_resTRUE[[i]])
    covar_nsim_bias_sq[[i]][,,j]  = apply(covar_nsim_bias[[i]][,,j], MARGIN=c(1,2), FUN=function(x) x^2)
    

  }
  
  
  
  #3b summarize results per condition

  covar_res[[i]]      = apply(covar_nsim[[i]], MARGIN = c(1, 2), FUN = mean)      # average over nsim
  covar_bias[[i]]     = abs(covar_res[[i]]-covar_resTRUE[[i]])                    # average bias of covar-class relation
  covar_bias_rel[[i]] = abs(covar_res[[i]]-covar_resTRUE[[i]])/covar_resTRUE[[i]] 
 
  res_x[[i]]          = colMeans(bind_rows(prop_x_nsim[[i]]))
   bias_x[[i]]        = colMeans(bind_rows(bias_x_nsim[[i]]))                     # average absolute bias 
   bias_x_abs[[i]]    = colMeans(bind_rows(bias_x_nsim_abs[[i]]))                 # average absolute bias 
   bias_x_rel[[i]]    = colMeans(bind_rows(bias_x_rel_nsim[[i]]))
  cov_x[[i]]  = coverage(prop_boot = prop_x_boot[[i]],                            # total variance, coverage and ciwidth
                              prop  = prop_x_nsim[[i]],
                              pop   = prop_xTRUE[[i]],
                              nsize = nsize,
                              nboot = nboot,
                              nsim  = nsim)   


  cov_z[[i]] = covariates(covar_b = covar_boot[[i]], 
                          covar_s = covar_nsim[[i]], 
                          covar_pop = covar_resTRUE[[i]], 
                          nsize = nsize,   
                          nboot = nboot,     
                          nsim  = nsim)
   
  
    
  coverage_z[[i]] <- cov_z[[i]][[2]]
  
  ciwidth_z[[i]] <- cov_z[[i]][[3]]  
  
  rmse_z[[i]] = apply(X=apply(covar_nsim_bias_sq[[i]][,,j] , MARGIN = c(1,2), FUN=mean), 
                      MARGIN = c(1,2), 
                      FUN=sqrt)                                  
  
 

  totalvariance[[i]]    <-  cov_x[[i]][[1]]                #total variance
  cover[[i]]            <-  cov_x[[i]][[2]]                #coverage
  ciwidth[[i]]          <-  cov_x[[i]][[3]]                #ci95width
  
  
  
  rmse_x[[i]] = sqrt(colMeans(bind_rows(bias_x_nsim_sq[[i]])))
  

}









#TREE
#results for plots
bias_x_rel_nsimTREE <- bias_x_rel_nsim
bias_x_relTREE <- bias_x_rel
bias_x_TREE <- bias_x
rmse_xTREE=rmse_x
ciwidthTREE=ciwidth
coverTREE=cover
sesd_xTREE=sesd_x
coverTREE=cover
prop_x_bootTREE=prop_x_boot
res_xTREE=res_x
totalvarianceTREE=totalvariance

#covar
covar_bias_TREE=covar_bias
covar_bias_relTREE=covar_bias_rel
covar_nsim_biasTREE=covar_nsim_bias
covar_bias_relTREE=covar_bias_rel
covar_nsim_bias_sqTREE=covar_nsim_bias_sq
covar_nsimTREE=covar_nsim
covar_resTREE=covar_res
cov_zTREE=cov_z
cov_xTREE=cov_x
rmse_zTREE=rmse_z
ciwidth_zTREE=ciwidth_z
coverage_zTREE=coverage_z





