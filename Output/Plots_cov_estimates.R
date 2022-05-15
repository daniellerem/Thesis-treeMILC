#4 Plots and figures
library(poLCA) 
library(dplyr) 
library(ggplot2)
library(reshape2)
library(xtable)
library(tables)
library(superheat)
library(plot.matrix)
library(RColorBrewer)
library(reshape2)

options(scipen = 999)

nsim   = 1000
nsize  = 5000 
nboot  = 5
nconds = 8



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
  geom_tile()+scale_fill_distiller(direction = 1, palette="Purples")+
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
  geom_tile()+scale_fill_distiller(direction = 1, palette="Reds")+
  geom_text(aes(label=format(round(value,3),nsmall=3)), size=5) +
  facet_grid(condition~method) #


