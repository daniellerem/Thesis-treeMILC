
##########################################################################################

#       Script to process results of MILC and tree-MILC: calculate performance measures  #

##########################################################################################


#1. SET UP 
options(scipen = 999)                                                     #scientific notation off

nsim   = 1000                                                              #number of simulation iterations
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
#2a MILC
prop_x_nsim     = rep(list(vector("list", nsim)), nconds)
bias_x_nsim     = rep(list(vector("list", nsim)), nconds)
bias_x_nsim_sq  = rep(list(vector("list", nsim)), nconds)
bias_x_nsim     = rep(list(vector("list", nsim)), nconds)
bias_x_nsim_abs = rep(list(vector("list", nsim)), nconds)
bias_x_rel_nsim = rep(list(vector("list", nsim)), nconds)

bias_x      = vector("list", nconds)
bias_x_abs  = vector("list", nconds)
bias_x_rel  = vector("list", nconds)
res_x       = vector("list", nconds)
cov_x       = vector("list", nconds)
rmse_x      = vector("list", nconds)
ciwidth     = vector("list", nconds)
totalvariance  = vector("list", nconds)
cover       = vector("list", nconds)

covar_nsim         = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias    = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias_rel= rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias_sq = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_bias         = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_bias_rel     = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_var          = vector("list", nconds)
covar_res          = vector("list", nconds)
cov_z              = vector("list", nconds)
rmse_z             = vector("list", nconds)
coverage_z         = vector("list", nconds)
ciwidth_z          = vector("list", nconds)

#2b storage for tree-MILC results
prop_x_nsimTREE     = rep(list(vector("list", nsim)), nconds)
bias_x_nsimTREE     = rep(list(vector("list", nsim)), nconds)
bias_x_nsim_sqTREE  = rep(list(vector("list", nsim)), nconds)
bias_x_nsimTREE     = rep(list(vector("list", nsim)), nconds)
bias_x_nsim_absTREE = rep(list(vector("list", nsim)), nconds)
bias_x_rel_nsimTREE = rep(list(vector("list", nsim)), nconds)

bias_xTREE      = vector("list", nconds)
bias_x_absTREE  = vector("list", nconds)
bias_x_relTREE  = vector("list", nconds)
res_xTREE       = vector("list", nconds)
cov_xTREE       = vector("list", nconds)
rmse_xTREE      = vector("list", nconds)
ciwidthTREE     = vector("list", nconds)
totalvarianceTREE  = vector("list", nconds)
coverTREE       = vector("list", nconds)

covar_nsimTREE         = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_biasTREE    = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias_relTREE= rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_nsim_bias_sqTREE = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)
covar_biasTREE         = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_bias_relTREE     = rep(list(matrix(NA,nrow=4, ncol=4, dimnames=NULL)), nconds)
covar_varTREE          = vector("list", nconds)
covar_resTREE          = vector("list", nconds)
cov_zTREE              = vector("list", nconds)
rmse_zTREE             = vector("list", nconds)
coverage_zTREE         = vector("list", nconds)
ciwidth_zTREE          = vector("list", nconds)


#3. Calculate performance measures

for (i in 1:nconds) {
    cat(i)
  for(j in 1:nsim){                                                            # loop over simulation iterations
    #3a pool results and calculate necessary intermediate results
    # .i for MILC
    prop_x_nsim[[i]][[j]]      = colMeans(bind_rows(prop_x_boot[[i]][[j]]))    # pooled class proportions
    bias_x_nsim_abs[[i]][[j]]  = abs(prop_x_nsim[[i]][[j]] - prop_true[[i]])  # absolute error per iteration
    bias_x_nsim_sq[[i]][[j]]   = (prop_x_nsim[[i]][[j]] - prop_true[[i]])^2   # absolute error per iteration
    bias_x_rel_nsim[[i]][[j]]  = abs(prop_x_nsim[[i]][[j]] - prop_true[[i]])/prop_true[[i]]  # absolute relative error per iteration
    covar_nsim[[i]][,,j]       = apply(covar_boot[[i]][[j]], MARGIN = c(1, 2), FUN = mean)     # pooled class-covariate estimates
    covar_nsim_bias[[i]][,,j]  = abs(covar_nsim[[i]][,,j]- covar_true[[i]])
    covar_nsim_bias_sq[[i]][,,j]  = apply(covar_nsim_bias[[i]][,,j], MARGIN=c(1,2), FUN=function(x) x^2)
    
    # .ii for tree-MILC
    prop_x_nsimTREE[[i]][[j]]      = colMeans(bind_rows(prop_x_bootTREE[[i]][[j]]))    # pooled class proportions
    bias_x_nsim_absTREE[[i]][[j]]  = abs(prop_x_nsimTREE[[i]][[j]] - prop_true[[i]])  # absolute error per iteration
    bias_x_nsim_sqTREE[[i]][[j]]   = (prop_x_nsimTREE[[i]][[j]] - prop_true[[i]])^2   # absolute error per iteration
    bias_x_rel_nsimTREE[[i]][[j]]  = abs(prop_x_nsimTREE[[i]][[j]] - prop_true[[i]])/prop_true[[i]]  # absolute relative error per iteration
    covar_nsimTREE[[i]][,,j]       = apply(covar_bootTREE[[i]][[j]], MARGIN = c(1, 2), FUN = mean)     # pooled class-covariate estimates
    covar_nsim_biasTREE[[i]][,,j]  = abs(covar_nsimTREE[[i]][,,j]- covar_true[[i]])
    covar_nsim_bias_sqTREE[[i]][,,j]  = apply(covar_nsim_biasTREE[[i]][,,j], MARGIN=c(1,2), FUN=function(x) x^2)
    
    

  }
  
  
  
  #3b summarize results per condition

  covar_res[[i]]      = apply(covar_nsim[[i]], MARGIN = c(1, 2), FUN = mean)      # average over nsim
  covar_bias[[i]]     = abs(covar_res[[i]]-covar_true[[i]])                       # average bias of covar-class relation
  covar_bias_rel[[i]] = abs(covar_res[[i]]-covar_true[[i]])/covar_true[[i]] 
 
  res_x[[i]]          = colMeans(bind_rows(prop_x_nsim[[i]]))
   bias_x[[i]]        = colMeans(bind_rows(bias_x_nsim[[i]]))                     # average absolute bias 
   bias_x_abs[[i]]    = colMeans(bind_rows(bias_x_nsim_abs[[i]]))                 # average absolute bias 
   bias_x_rel[[i]]    = colMeans(bind_rows(bias_x_rel_nsim[[i]]))
   
   covar_resTREE[[i]]      = apply(covar_nsimTREE[[i]], MARGIN = c(1, 2), FUN = mean)      # average over nsim
   covar_biasTREE[[i]]     = abs(covar_resTREE[[i]]-covar_true[[i]])                       # average bias of covar-class relation
   covar_bias_relTREE[[i]] = abs(covar_resTREE[[i]]-covar_true[[i]])/covar_true[[i]] 
   
   res_xTREE[[i]]          = colMeans(bind_rows(prop_x_nsimTREE[[i]]))
   bias_xTREE[[i]]        = colMeans(bind_rows(bias_x_nsimTREE[[i]]))                     # average absolute bias 
   bias_x_absTREE[[i]]    = colMeans(bind_rows(bias_x_nsim_absTREE[[i]]))                 # average absolute bias 
   bias_x_relTREE[[i]]    = colMeans(bind_rows(bias_x_rel_nsimTREE[[i]]))
   
   #Calculate total variance, coverage and ciwidth for class sizes estimates
 #i. MILC
    cov_x[[i]]  = coverage(prop_boot = prop_x_boot[[i]],                          #bootstrap estimates from method
                              prop  = prop_x_nsim[[i]],                         #pooled estimates from method
                              pop   = prop_true[[i]],                           #simulated population estimates
                              nsize = nsize,
                              nboot = nboot,
                              nsim  = nsim)   

    totalvariance[[i]]    <-  cov_x[[i]][[1]]                #total variance
    cover[[i]]            <-  cov_x[[i]][[2]]                #coverage
    ciwidth[[i]]          <-  cov_x[[i]][[3]]                #ci95width
  ##ii. tree-MILC
    cov_xTREE[[i]]  = coverage(prop_boot = prop_x_bootTREE[[i]],                          #bootstrap estimates from method
                           prop  = prop_x_nsimTREE[[i]],                         #pooled estimates from method
                           pop   = prop_true[[i]],                           #simulated population estimates
                           nsize = nsize,
                           nboot = nboot,
                           nsim  = nsim)   
    
    totalvarianceTREE[[i]]    <-  cov_xTREE[[i]][[1]]                #total variance
    coverTREE[[i]]            <-  cov_xTREE[[i]][[2]]                #coverage
    ciwidthTREE[[i]]          <-  cov_xTREE[[i]][[3]]                #ci95width
    
    #Calculate total variance, coverage and ciwidth for class-covariate estimates
    #i. MILC
    cov_z[[i]] = covariates(covar_b = covar_boot[[i]],                          #boostrap estimates from method
                          covar_s = covar_nsim[[i]],                            #pooled estimates from method
                          covar_pop = covar_true[[i]],                          #simulated population estimates
                          nsize = nsize,   
                          nboot = nboot,     
                          nsim  = nsim)
   
  coverage_z[[i]]         <- cov_z[[i]][[2]]                 #coverage
  ciwidth_z[[i]]          <- cov_z[[i]][[3]]                 #ciwidth
  
  #ii. tree-MILC
  cov_zTREE[[i]] = covariates(covar_b = covar_bootTREE[[i]],                          #boostrap estimates from method
                          covar_s = covar_nsimTREE[[i]],                            #pooled estimates from method
                          covar_pop = covar_true[[i]],                          #simulated population estimates
                          nsize = nsize,   
                          nboot = nboot,     
                          nsim  = nsim)
  
  coverage_zTREE[[i]]         <- cov_zTREE[[i]][[2]]                 #coverage
  ciwidth_zTREE[[i]]          <- cov_zTREE[[i]][[3]]                 #ciwidth
  
  #RMSE
  rmse_x[[i]]     = sqrt(colMeans(bind_rows(bias_x_nsim_sq[[i]])))
  rmse_xTREE[[i]] = sqrt(colMeans(bind_rows(bias_x_nsim_sqTREE[[i]])))
  
  rmse_z[[i]]     = apply(X=apply(covar_nsim_bias_sq[[i]][,,j] , MARGIN = c(1,2), FUN=mean), 
                      MARGIN = c(1,2), 
                      FUN=sqrt)                                  
  rmse_zTREE[[i]] = apply(X=apply(covar_nsim_bias_sqTREE[[i]][,,j] , MARGIN = c(1,2), FUN=mean), 
                      MARGIN = c(1,2), 
                      FUN=sqrt)                                  
  
 

}#END


save.image(file="PerformanceMeasures.RData")










