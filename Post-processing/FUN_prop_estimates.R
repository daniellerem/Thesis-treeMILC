
##########################################################################################

#      Function to calculate CI interval performance measures for the class sizes        #

##########################################################################################


coverage = function(prop_boot,  prop, pop, nsize, nboot, nsim){                 #prop_boot are the results from the methods for each bootstrap sample
                                                                                #prop are the pooled estimates per simulation iteration 
                                                                                #pop is the population value
  
  covmat  = matrix(NA, nsim, length(pop))
  cimat   = matrix(NA, nsim, length(pop))
  tvarmat = matrix(NA, nsim, length(pop))
  
  for(j in 1:nsim){
    
    fun_var     = function(prop_boot) (prop_boot*(1-prop_boot))/nsize           #function for between variance      
    res_var     = lapply(prop_boot[[j]], fun_var)                               #within variance
   
     tvarmat[j,] =                                                              # total variance
      colMeans(bind_rows(res_var)) +                                            
      colVars(as.matrix(bind_rows(prop_boot[[j]]))) +
      colVars(as.matrix(bind_rows(prop_boot[[j]])))/nboot
    
    #Confidence interval around the pooled estimates
    
    ll = prop[[j]] - qt(.975, nsize-1) * sqrt(tvarmat[j,])                      # CI limits
    ul = prop[[j]] + qt(.975, nsize-1) * sqrt(tvarmat[j,])  
    
    #coverage of population value in CI
    covmat[j,] = ll < pop & pop < ul
    cimat[j,]  = ul - ll
    
  }
  
  tvar    = colMeans(tvarmat)
  cov     = colMeans(covmat)                                                    # average over nsim
  ciwidth = colMeans(cimat)
  
  return(list(totalvariance=tvar, coverage=cov, ci95width=ciwidth))             #return the total variance, the coverage of the CI and the width of the CI
}


#note: see thesis document for full explanation about the equations