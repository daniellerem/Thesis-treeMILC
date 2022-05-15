#4 Plots and figures
library(poLCA) 
library(dplyr) 
library(ggplot2)
library(xtable)
#library(tables)
#library(superheat)
#library(plot.matrix)
library(RColorBrewer)
#library(reshape2)

options(scipen = 999)

nsim   = 1000
nsize  = 5000 
nboot  = 5
nconds = 8
#pop = c(0.15, 0.34, 0.2975, 0.2125)
#load("prop_popcalc.RData")
#load("covar_popcalc.RData")

#to do: make functions out of the plots with data as input


#prepare data
#per conditie, en per class in 1 figuur met boxplots (dan zien we de bias en de spreiding)
#bias_x_rel_nsim or bias_x_nsim

#milc
myarray <- array(unlist(bias_x_rel_nsim), dim = c(4,nsim,nconds)) #4 classes, nsim = nsim, nconds=8
dimnames(myarray) <- list(classes=cbind("LC 1", "LC 2", "LC 3","LC 4"), numsim=seq(nsim), condition=seq(8))
longdat <- melt(myarray)
longdat$method = "MILC"
#Tree
myarraytree <- array(unlist(bias_x_rel_nsimTREE), dim = c(4,nsim,nconds)) #4 classes, nsim = nsim, nconds=8
dimnames(myarraytree) <- list(classes=cbind("LC 1", "LC 2", "LC 3","LC 4"), numsim=seq(nsim), condition=seq(8))
longdattree <- melt(myarraytree)
longdattree$method = "tree-MILC"

#combine
longdatcom = rbind(longdat, longdattree)

summary(longdatcom) #condition needs to be categorical
longdatcom$condition <- as.character(longdat$condition)
longdatcom$classes <- as.character(longdat$classes)

#BOXPLOTS
ggplot(data = longdatcom, aes(x= condition, y=value, fill=classes)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        plot.margin = margin(5,10,2,2),
        axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), 
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=20))+ 
  scale_fill_discrete(labels=c("1", '2', '3','4'))+
      geom_boxplot() +facet_grid(method~.) +
  guides(fill = guide_legend(override.aes = list(size=1)))
ggsave(file="Boxplot.png")
theme_get()

ggplot(data = longdatcom, aes(x= condition, y=value, fill=method)) +
  scale_fill_manual(values=c("red", "darkgreen")) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        plot.margin = margin(5,10,2,2),
        axis.title.x = element_text(size=15), axis.title.y = element_text(size=15), 
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=20))+ 
  geom_boxplot() +facet_grid(classes~.) +
  guides(fill = guide_legend(override.aes = list(size=1)))

ggsave(file="Boxplot_classes.png")




#BIAS PROPORTIONS
mymatrix <- matrix(unlist(bias_x_rel), nrow=4, ncol = 8) #use bias data
dimnames(mymatrix) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmatrel <- melt(mymatrix)
longmatrel$method="MILC"

#tree
mymatrixtree <- matrix(unlist(bias_x_relTREE), nrow=4, ncol = 8) #use bias data
dimnames(mymatrixtree) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmatreltree <- melt(mymatrixtree)
longmatreltree$method="tree-MILC"
longmatrel <- rbind(longmatrel,longmatreltree)
summary(longmatrel) #both needs to be categorical
longmatrel$condition <- as.character(longmatrel$condition)
longmatrel$classes <- as.character(longmatrel$classes)

#BIAS 
mymatrix <- matrix(unlist(bias_x_rel), nrow=4, ncol = 8) #use bias data
dimnames(mymatrix) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmat <- melt(mymatrix)
longmat$method="MILC"

#tree
mymatrixtree <- matrix(unlist(bias_x_relTREE), nrow=4, ncol = 8) #use bias data
dimnames(mymatrixtree) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmattree <- melt(mymatrixtree)
longmattree$method="tree-MILC"
longmat <- rbind(longmat,longmattree)
summary(longmat) #both needs to be categorical
longmat$condition <- as.character(longmat$condition)
longmat$classes <- as.character(longmat$classes)


ggplot(longmatrel, aes(x=classes, y=reorder(condition,desc(condition)), fill=value)) +
  ylab("condition")+xlab("class") + 
#  ggtitle("Absolute relative bias") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=18),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+ 
  geom_tile()+scale_fill_distiller(direction = 1, palette="Purples")+
  geom_text(aes(label=format(round(value,3)),nsmall=3), size=8) +
  facet_grid(.~method)
ggsave("absrelbiasprop.png")


#BIAS COVAR REL
myarray <- array(unlist(covar_bias_rel), dim=c(4,4,8)) #use bias data
dimnames(myarray) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatrel <- melt(myarray, varnames=c("classes", "covariate", "condition"))
covlongmatrel$method <- "MILC"

myarray_tree <- array(unlist(covar_bias_relTREE), dim=c(4,4,8)) #use bias data
dimnames(myarray_tree) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatrel_tree <- melt(myarray_tree, varnames=c("classes", "covariate", "condition"))
covlongmatrel_tree$method <- "tree-MILC"
covlongmatrel <- rbind(covlongmatrel, covlongmatrel_tree)


summary(covlongmatrel) #both needs to be categorical
covlongmatrel$condition <- as.character(covlongmatrel$condition)
#make 1_MILC en 1_tree-MILC
covlongmatrel$classes <- as.character(covlongmatrel$classes)
covlongmatrel$covariate <- as.character(covlongmatrel$covariate)


ggplot(covlongmatrel, aes(x=classes, y=reorder(covariate, desc(covariate)), fill=value)) +
  xlab("classes")+ylab("covariate") + 
  #ggtitle("Absolute relative bias") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=30),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+ 
  geom_tile()+scale_fill_distiller(direction = 1, palette="Purples", limits=c(0,0.2))+
  geom_text(aes(label=format(round(value,3)),nsmall=3), size=5) +
  facet_grid(condition~method) #
ggsave("absrelbiascovar.png")


#CIWDITH COVAR
myarray <- array(unlist(ciwidth_z), dim=c(4,4,8)) 
dimnames(myarray) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatCI_milc <- melt(myarray, varnames=c("classes", "covariate", "condition"))
covlongmatCI_milc$method <- "MILC"

myarray_tree <- array(unlist(ciwidth_zTREE), dim=c(4,4,8)) 
dimnames(myarray_tree) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatCI_tree <- melt(myarray_tree, varnames=c("classes", "covariate", "condition"))
covlongmatCI_tree$method <- "tree-MILC"
covlongmatCI <- rbind(covlongmatCI_milc, covlongmatCI_tree)

summary(covlongmatCI) #both needs to be categorical
covlongmatCI$condition <- as.character(covlongmatCI$condition)
#make 1_MILC en 1_tree-MILC
covlongmatCI$classes <- as.character(covlongmatCI$classes)
covlongmatCI$covariate <- as.character(covlongmatCI$covariate)


ggplot(covlongmatCI, aes(x=classes, y=reorder(covariate, desc(covariate)), fill=value)) +
  xlab("classes")+ylab("covariate") +
  #ggtitle("95% CI width") + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=30),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+   
  geom_tile()+scale_fill_distiller(direction = 1, palette="Blues", limits=c(0,0.04))+
  geom_text(aes(label=format(round(value,3)),nsmall=3), size=5) +
  facet_grid(condition~method) #
ggsave("ciwidthcovar.png")


 #CIWDITH PROP
mymatrix <- matrix(unlist(ciwidth), nrow=4, ncol = 8) #use bias data
dimnames(mymatrix) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmat_ci <- melt(mymatrix)
longmat_ci$method="MILC"
mymatrixTREE <- matrix(unlist(ciwidthTREE), nrow=4, ncol = 8) #use bias data
dimnames(mymatrixTREE) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmat_ciTREE <- melt(mymatrixTREE)
longmat_ciTREE$method="tree-MILC"
longmat_ci=rbind(longmat_ci, longmat_ciTREE)

longmat_ci$condition <- as.character(longmat_ci$condition)
longmat_ci$classes <- as.character(longmat_ci$classes)

ggplot(longmat_ci, aes(x=classes, y=reorder(condition,desc(condition)), fill=value)) +
  ylab("condition")+xlab("classes") + 
  #ggtitle("95% CI width") + 
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=30),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+   
  geom_tile()+scale_fill_distiller(direction = 1,   limits=range(0,.04)) +
  geom_text(aes(label=format(round(value,3)),nsmall=3), size=8) +
  facet_grid(.~method)
ggsave("ciwidthprop.png")

#COVERAGE COVAR  #-> nieuwe kleuren, groen = tussen 0.92 en 0.97, rest is rood? (of erboven donkergroen?)
myarray <- array(unlist(coverage_z), dim=c(4,4,8)) #use bias data
dimnames(myarray) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatCOV <- melt(myarray, varnames=c("classes", "covariate", "condition"))
covlongmatCOV$method="MILC"
myarray <- array(unlist(coverage_zTREE), dim=c(4,4,8)) #use bias data
dimnames(myarray) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatCOVTREE <- melt(myarray, varnames=c("classes", "covariate", "condition"))
covlongmatCOVTREE$method="tree-MILC"

covlongmatCOV=rbind(covlongmatCOV, covlongmatCOVTREE)
summary(covlongmatCOV) #both needs to be categorical
covlongmatCOV$condition <- as.character(covlongmatCOV$condition)
covlongmatCOV$classes <- as.character(covlongmatCOV$classes)
covlongmatCOV$covariate <- as.character(covlongmatCOV$covariate)


ggplot(covlongmatCOV,  aes(x=classes, y=reorder(covariate, desc(covariate)), fill=value)) +
  xlab("classes")+ylab("covariate") + 
  #ggtitle("Coverage of 95% CI") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=20),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+   
  geom_tile()+ scale_fill_distiller(palette="RdYlGn", direction=1, limits=c(0,1))+ #RdYlGn
  geom_text(aes(label=round(value,2)), size=5) +
  facet_grid(condition~method) #



#COVERAGE PROPORTIONS
mymatrix <- matrix(unlist(cover), nrow=4, ncol = 8) #use bias data
dimnames(mymatrix) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmat_cov <- melt(mymatrix)
longmat_cov$method="MILC"

mymatrix <- matrix(unlist(coverTREE), nrow=4, ncol = 8) #use bias data
dimnames(mymatrix) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmat_covTREE <- melt(mymatrix)
longmat_covTREE$method="tree-MILC"
longmat_cov=rbind(longmat_cov, longmat_covTREE)

longmat_cov$condition <- as.character(longmat_cov$condition)
longmat_cov$classes <- as.character(longmat_cov$classes)

ggplot(longmat_cov, aes(x=classes, y=reorder(condition,desc(condition)), fill=value)) +
  ylab("condition")+xlab("class") + 
  #ggtitle("Coverage of 95% CI") + 
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=30),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+ 
  geom_tile()+ scale_fill_distiller(palette="RdYlGn", direction=1, limits=c(0,1))+ #RdYlGn
  geom_text(aes(label=format(round(value,2)),nsmall=2), size=8) +
  facet_wrap(method~., ncol=4) 

ggsave("coverageprop.png")
#covar: rmse
myarray <- array(unlist(rmse_z), dim=c(4,4,8)) #use bias data
dimnames(myarray) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatRMSE <- melt(myarray, varnames=c("classes", "covariate", "condition"))
covlongmatRMSE$method="MILC"

myarray <- array(unlist(rmse_zTREE), dim=c(4,4,8)) #use bias data
dimnames(myarray) <- list(classes = seq(4), covariate=seq(4), condition =seq(8)) #4 classes, nconds=8
covlongmatRMSEtree <- melt(myarray, varnames=c("classes", "covariate", "condition"))
covlongmatRMSEtree$method="tree-MILC"
covlongmatRMSE=rbind(covlongmatRMSEtree, covlongmatRMSE)

head(covlongmatRMSE)
summary(covlongmatRMSE) #both needs to be categorical
covlongmatRMSE$condition <- as.character(covlongmatRMSE$condition)
covlongmatRMSE$classes <- as.character(covlongmatRMSE$classes)
covlongmatRMSE$covariate <- as.character(covlongmatRMSE$covariate)


ggplot(covlongmatRMSE,  aes(x=classes, y=reorder(covariate, desc(covariate)), fill=value)) +
  xlab("classes")+ylab("covariate") +
  #ggtitle("RMSE") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=30),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+ 
  geom_tile()+scale_fill_distiller(direction = 1, palette="Reds", limits=c(0,0.032))+
  geom_text(aes(label=format(round(value,3),nsmall=3)), size=5) +
  facet_grid(condition~method) #

#Prop: rmse
mymatrix <- matrix(unlist(rmse_x), nrow=4, ncol = 8) #use bias data
dimnames(mymatrix) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmat_rmse <- melt(mymatrix)
longmat_rmse$method="MILC"
mymatrix <- matrix(unlist(rmse_xTREE), nrow=4, ncol = 8) #use bias data
dimnames(mymatrix) <- list(classes = seq(4), condition =seq(8)) #4 classes, nconds=8
longmat_rmseTREE <- melt(mymatrix)
longmat_rmseTREE$method="tree-MILC"
longmat_rmse=rbind(longmat_rmse, longmat_rmseTREE)
longmat_rmse$condition <- as.character(longmat_rmse$condition)
longmat_rmse$classes <- as.character(longmat_rmse$classes)


ggplot(longmat_rmse, aes(x=classes, y=reorder(condition,desc(condition)), fill=value)) +
  ylab("condition")+xlab("class") +
  #ggtitle("RMSE") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size=30), 
        axis.title.x = element_text(size=18), axis.title.y = element_text(size=18), 
        strip.text.x = element_text(size=20) , strip.text.y = element_text(size=30),
        legend.text = element_text(size=15),legend.title = element_text(size=20), 
        legend.key=element_rect(size=2), legend.key.size = unit(2, "cm"),
        axis.text = element_text(size=10))+ 
  geom_tile()+
  scale_fill_distiller(direction = 1, palette = "Reds", limits=c(0,0.032))+
  geom_text(aes(label=format(round(value,3),nsmall=3)), size=8) +
  facet_wrap(method~., ncol=4) #
#limit=c(0,.05)  #limits om prop en cov te laten matchen qua kleur
ggsave("rmseprop.png")

