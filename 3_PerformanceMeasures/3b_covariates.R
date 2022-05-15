covariates = function(covar_b, covar_s, covar_pop, nsize, nboot, nsim){
  

  covarmat = rep(list(matrix(nrow=4,ncol=4)), nsim)
  cimat   = rep(list(matrix(nrow=4,ncol=4)), nsim)
  res_var  = array(NA, dim=c(4, 4,nsim))
  tvarmat =  rep(list(matrix(nrow=4,ncol=4)), nsim)
  
  cov  = matrix(NA, 4, 4)
  ciwidth   = matrix(NA, 4, 4)
  tvar = matrix(NA, 4, 4)

  fun_var     = function(prop_boot) (prop_boot*(1-prop_boot))/nsize
  for(j in 1:nsim){
    res_var     = apply(covar_b[[j]],MARGIN=c(1,2,3), FUN=fun_var)     
    tvarmat[[j]] = apply(X=(res_var), MARGIN=c(1,2), FUN = mean) +                # within variance                
                   apply(X=(covar_b[[j]]), MARGIN=c(1,2), FUN = var) +             #between variance
      apply(X=(covar_b[[j]]), MARGIN=c(1,2), FUN = var)/nboot
    
    ll = covar_s[,,j] - qt(.975, nsize-1) * apply(X=(tvarmat[[j]]), MARGIN=c(1,2), FUN = sqrt)  
    ul = covar_s[,,j] + qt(.975, nsize-1) * apply(X=(tvarmat[[j]]), MARGIN=c(1,2), FUN = sqrt)   
    
    covarmat[[j]] = ll < covar_pop & covar_pop < ul 
    cimat[[j]]  = ul - ll
    
  }  
  tvar    = apply(X=(array(unlist(tvarmat),dim=c(4,4,nsim))), MARGIN=c(1,2), FUN = mean)
  covar     = apply(X=(array(unlist(covarmat),dim=c(4,4,nsim))), MARGIN=c(1,2), FUN = mean)                                                 # average over nsim
  ciwidth = apply(X=(array(unlist(cimat),dim=c(4,4,nsim))), MARGIN=c(1,2), FUN = mean)
 return(list(totalvariance=tvar, coverage=covar, ci95width=ciwidth))  }


