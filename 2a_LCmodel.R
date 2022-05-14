posteriors = function(bootdat, k){
  
  #note: capture
  
  
  log <- capture.output({ #make sure not all messages are displayed
      longdat = as.data.frame(fre2dat(bootdat[,c(1:6,k+7)]))                     #column 7 contains the original data
   })

    LCA <-  poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,                       #apply LC model to dataset with 4 classes
                data      = longdat,       
                nclass    = 4,      
                nrep      = 10)
    
  order <-  c(which.max(LCA$probs$Y1[,1]),                                       #use "column-maxima switched label detection"-principle to solve the label switching problem
            which.max(LCA$probs$Y1[,2]), 
            which.max(LCA$probs$Y1[,3]),
            which.max(LCA$probs$Y1[,4]))                                        
  
  probs.start.new <- poLCA.reorder(LCA$probs.start, order)
  
    LCAr          <- poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,
                           data    = longdat,
                           nclass  = 4,
                       probs.start = probs.start.new)
  
  entr   <- poLCA.entropy(LCAr)                                                 #entropy is calculated for possible later use
  
  ordat  <-  as.data.frame(fre2dat(bootdat[,c(1:7)]))
  
  posdat <-  cbind(ordat, 
                   poLCA.posterior(lc = LCAr, y = ordat[,-6]),                  #exclude 'trueclass' column from posterior calculation
                   entr = entr)                        
                                                                                
  return(posdat)
  
}




