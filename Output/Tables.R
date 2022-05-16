##########################################################################################

#       Script to make the tables of the results of MILC and tree-MILC                   #

##########################################################################################


#necessary packages
library(xtable)
library(knitr)
library(stats)


#Table 6; population estimates, class sizes
xtable(digits=4,   (matrix(unlist(prop_true),  nrow=4, ncol = 8)))

#Table 7; population estimates, class-covariate cross-tables

unlistedtruecov=(array(unlist(covar_true), dim=c(4,4,8)))
apply(unlistedtruecov, 3, xtable, digits=4)

#TABLE 8: results for the latent class sizes for MILC and treeMILC in terms of four performance measures
xtable(digits=4, rbind(
  #MARE (mean absolute relative error)
  (matrix(unlist(bias_x_rel),  nrow=4, ncol = 8)), 
  (matrix(unlist(bias_x_relTREE),  nrow=4, ncol = 8)), 
  #rmse
  (matrix(unlist(rmse_x),  nrow=4, ncol = 8)), 
  (matrix(unlist(rmse_xTREE),  nrow=4, ncol = 8)), 
  #coverage
  (matrix(unlist(cover),  nrow=4, ncol = 8)), 
  (matrix(unlist(coverTREE),  nrow=4, ncol = 8)), 
  #ciwidth
  (matrix(unlist(ciwidth),  nrow=4, ncol = 8)), 
  (matrix(unlist(ciwidthTREE),  nrow=4, ncol = 8)) )) 



 #Results for covariates:

#condition 1
c1 = xtable(digits=4, rbind(
  cbind(covar_bias[[1]], rmse_z[[1]]),
  cbind(covar_biasTREE[[1]], rmse_zTREE[[1]])))


#Per condition one block of 8 x 8 consisting of four block of 4x4 cross-tables
latextables8<- vector(mode = "list", length = 8)

for (i in 1:8) {
  latextables8[[i]] <-  xtable(digits=4, rbind(
    cbind(covar_bias[[i]], rmse_z[[i]]),
    cbind(covar_biasTREE[[i]], rmse_zTREE[[i]])))
  print(latextables8)
}
latextables8
#to get the same output as the table in the thesis project, a custom-made latex table is made. 
#the necessary elements are the following: \hline and \multirow{8}{*}{[conditionnr]} before each condition
# & \multirow{4}{*}{MILC} before the first four lines of each condition, 
#and & \multirow{4}{*}{tree-MILC} before the last four lines of each condition
#if wanted, the template for the table can be found in the .tex file in the folder  `ThesisText`

#Table 9: Results of accuracy performance measures for the cross tables
 
 latextables_acc<- vector(mode = "list", length = 8)
 
 for (i in 1:8) {
   latextables_acc[[i]] <-  xtable(digits=4, rbind(
     cbind(covar_bias[[i]], rmse_z[[i]]),
     cbind(covar_biasTREE[[i]], rmse_zTREE[[i]])))
   print(latextables_acc)
 }
 latextables_acc

#Table 10: Results of confidence interval performance measures for the cross tables
 latextables_ci<- vector(mode = "list", length = 8)
 
 for (i in 1:8) {
   latextables_ci[[i]] <-  xtable(digits=4, rbind(
     cbind(coverage_z[[i]], ciwidth_z[[i]]),
     cbind(coverage_zTREE[[i]], ciwidth_zTREE[[i]])))
   print(latextables_ci)
 }
 latextables_ci

                   

