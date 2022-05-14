#2b     
#selection error part of tree-MILC 
#specific case: apply 2 classes on data with 4 categories

posteriors2 = function(bootdat,  k){
  
  
  log <- capture.output({ #make sure not all messages are displayed
    longdat = as.data.frame(fre2dat(bootdat[,c(1:6,k+7)]))                      #column 7 contains the original data (n)
  })
  

  for(v in 1:ncol(longdat)){                                                    # replace 3's and 4's with 2's in all columns 
  longdat[,v] <- sapply(longdat[,v],function(x) replace(x, x%in% c(3,4), 2))
  }
  
    
                                                                                #apply LC model with 2 classes
    LCA <-  poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,  
                data    = longdat,       
                nclass  = 2,      
                nrep    = 10)
  
  
  order  <-  c(which.max(LCA$probs$Y1[,1]),                                     #order classes by their column maxima to solve the label switching problem
               which.max(LCA$probs$Y1[,2]))
  
  probs.start.new <- poLCA.reorder(LCA$probs.start, order)                      #reorder poLCA object 
  
  LCAr    <- poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,
                   data    = longdat,
                   nclass  = 2,
               probs.start = probs.start.new)
  
  ordat   <-  as.data.frame(fre2dat(bootdat[,c(1:7)]))
  ordat2  <-  ordat
  
  for(v in 1:ncol(longdat)){                                                     # replace 3's and 4's with 2's in all columns
    ordat2[,v] <- sapply(ordat[,v],function(x) replace(x, x%in% c(3,4), 2))
  }
  
  posdat = cbind(ordat,                                                          #use the original data with categories 1 till 4
                 poLCA.posterior(lc = LCAr, y = ordat2[,-6]))                    #use the data with only categories 1 and 2 to calculate the posteriors
                                                                                 #exclude the 'trueclass' column from posterior calculation
                                         
  
  return(posdat)
  
}




