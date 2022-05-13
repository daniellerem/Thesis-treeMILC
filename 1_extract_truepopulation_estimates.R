#Calculate population values of simulated dataset



load("simdata.RData")                                                           #the data is stored in response pattern freqencies - format



#create storeage
longdat          = rep(list(vector("list", nsim)),nconds)                       #to store the data in long format
prop_x_nsim_true = rep(list(vector("list", nsim)),nconds)

covar_nsim_true  = rep(list(array(NA,dim = c(4,4,nsim), dimnames=NULL)), nconds)

prop_true        =  vector("list", nconds)
covar_true       =  vector("list", nconds)


for(i in 1:nconds){                                                             
  cat(i)
  for(j in 1:nsim){     
    log <- capture.output({ #make sure not all messages are displayed about the pattern frequencies
      longdat[[i]][[j]]            <-    as.data.frame(fre2dat(simbootdat[[i]][[j]][,c(1:7)]))    #transfer pattern frequencies to longformat dataframe (column 7 contains the pattern frequencies original data)
      })
      prop_x_nsim_true[[i]][[j]]   <-  prop.table(table(longdat[[i]][[j]]$trueclass))
      covar_nsim_true[[i]][,,j]    <-  prop.table(table(longdat[[i]][[j]]$trueclass, longdat[[i]][[j]]$Y5))
  }#end nsim
  covar_true[[i]]                  <-  apply(covar_nsim_true[[i]], MARGIN = c(1, 2), FUN = mean)       # average over nsim
  prop_true[[i]]                   <-  colMeans(bind_rows(prop_x_nsim_true[[i]][[j]])) 
}#end nconds
