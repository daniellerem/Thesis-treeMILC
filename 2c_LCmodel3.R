#2c
#Measurement error part of tree-MILC
posteriors3 = function(bootdat, subsetrows, k){
  
  
  log <- capture.output({ #make sure not all messages are displayed             #change bootdata to longdata format
    longdat = as.data.frame(fre2dat(bootdat[,c(1:6,k+7)]))                      #column 7 contains the original data, the k columns next to it the frequencies of the bootstrapped datasets
  })
  

 subset <-  longdat[subsetrows,]             

    LCA <-  poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,  
                data = subset,       
                nclass = 3,      
                nrep = 10)
  
  order <-  c(which.max(LCA$probs$Y1[,2]), 
            which.max(LCA$probs$Y1[,3]), 
            which.max(LCA$probs$Y1[,4]))                             
  
  probs.start.new <- poLCA.reorder(LCA$probs.start, order)
  
    LCAr <- poLCA(formula = cbind(Y1, Y2, Y3, Y4, Y5) ~ 1,
                     data = subset,
                   nclass = 3,
              probs.start = probs.start.new)
  
   ordat    <-  as.data.frame(fre2dat(bootdat[,c(1:7)]))                         #y1-y5, 6=trueclass, 7=original N
  sub_ordat <-  ordat[subsetrows,] 
  
  
  posdat <-  cbind(sub_ordat, 
                 poLCA.posterior(lc = LCAr,  y = sub_ordat[,-c(6)]),                #exclude 'trueclass' column from posterior calculation
                 subsetrows) 
  
  posdat <- rename(.data=posdat,  "2"="1", "3"="2", "4"="3")                     #rename columns  (to avoid duplicate name with LC=1 from the 2class model)
  
  return(posdat)
  
}




