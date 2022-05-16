#################################################################################################

#       Script to visualise results of MILC and tree-MILC: Class size (proportions) estimates   #

#################################################################################################

library(poLCA) 
library(dplyr) 
library(reshape2)
library(tidyverse)
library(ggplot2)
library(xtable)
library(RColorBrewer)

options(scipen = 999)

nsim   = 1000
nsize  = 5000 
nboot  = 5
nconds = 8


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

