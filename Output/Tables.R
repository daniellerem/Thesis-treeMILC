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



#TABLE 8: results for the latent class sizes for MILC and treeMILC in terms of four performance measures
xtable(digits=4, rbind(
  (matrix(unlist(bias_x_rel),  nrow=4, ncol = 8)), #MARE (mean absolute relative error)
  (matrix(unlist(bias_x_relTREE),  nrow=4, ncol = 8)), 
  (matrix(unlist(rmse_x),  nrow=4, ncol = 8)), #rmse
  (matrix(unlist(rmse_xTREE),  nrow=4, ncol = 8)), 
  (matrix(unlist(cover),  nrow=4, ncol = 8)), #coverage
  (matrix(unlist(coverTREE),  nrow=4, ncol = 8)), 
  (matrix(unlist(ciwidth),  nrow=4, ncol = 8)), 
  (matrix(unlist(ciwidthTREE),  nrow=4, ncol = 8)) )) #ciwidth


#Table 9: Results of accurac performance measures for the cross tables



#TABLE TREE MILC prop beide
xtable(digits=4, rbind(t(matrix(unlist(bias_x_rel),  nrow=4, ncol = 8)), t(matrix(unlist(bias_x_relTREE),  nrow=4, ncol = 8)), 
                       t(matrix(unlist(rmse_x),  nrow=4, ncol = 8)), t(matrix(unlist(rmse_xTREE),  nrow=4, ncol = 8)),
                       t(matrix(unlist(cover),  nrow=4, ncol = 8)),  t(matrix(unlist(coverTREE),  nrow=4, ncol = 8)), 
                       t(matrix(unlist(ciwidth),  nrow=4, ncol = 8)), t(matrix(unlist(ciwidthTREE),  nrow=4, ncol = 8)) )) #ciwidth

#\hline between each
#\mult9irow{4}{*}{rmse} 
covvie=(array(unlist(covar_bias_rel), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)

covvie=(array(unlist(rmse_z), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)

covvie=(array(unlist(coverage_z), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)

covvie=(array(unlist(ciwidth_z), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)

#covariates TREE

covvie=(array(unlist(covar_bias_relTREE), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)

covvie=(array(unlist(rmse_zTREE), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)

covvie=(array(unlist(coverage_zTREE), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)

covvie=(array(unlist(ciwidth_zTREE), dim=c(4,4,8)))
apply(covvie, 3, xtable, digits=4)



rmse=t(matrix(unlist(rmse_x), ncol=4, dimnames=list(conditions= c(paste0("con", seq(8))), classes= c(paste0("c", seq(4))))))
bias=t(matrix(unlist(bias_x_rel), ncol=4, dimnames=list(conditions= c(paste0("con", seq(8))), classes= c(paste0("c", seq(4))))))
coverage=t(matrix(unlist(cover), ncol=4, dimnames=list(conditions= c(paste0("con", seq(8))), classes= c(paste0("c", seq(4))))))#se/sd

ftable(digits=5, rmse,bias,coverage, row.vars = cbind("classes", "rmse", "bias","cover"))
ftable(digits=5, row.vars = , x=rbind(bias=t(matrix(unlist(bias_x_rel), ncol=4)), 
                                      rmse=t(matrix(unlist(rmse_x), ncol=4)),
                                      cov=t(matrix(unlist(ciwidth), ncol=4)) )) #coverage


