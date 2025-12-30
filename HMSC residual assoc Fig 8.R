#HMSC results of residual associations among species
#Danelle updated in December 2025 (dmlarson@usgs.gov)


#session info
sessionInfo()


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#libraries-----
library(Hmsc)
library(corrplot)


#Set WD----------
setwd("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\HMSC final fit models Dec 2025\\")#Danelle's WD

#### Load models -------

#### Upper pool 4 ####
load("DL_PoolU4_models_thin_1000_samples_250_chains_4.Rdata")
pool_4U<-models[[1]]
pool_4U
post_pool_4U<-convertToCodaObject(pool_4U)

#### Lower pool 4 ####

load("DL_PoolL4_models_thin_1000_samples_250_chains_4.Rdata")
pool_4L<-models[[1]]
pool_4L
post_pool_4L<-convertToCodaObject(pool_4L)

#### Pool 8 ####
load("DL_Pool8_models_thin_1000_samples_250_chains_4.Rdata")
pool_8<-models[[1]]
pool_8
post_pool_8<-convertToCodaObject(pool_8)


#### Pool 13 ####
load("DL_Pool13_models_thin_1000_samples_250_chains_4.Rdata")
pool_13<-models[[1]]
pool_13
post_pool_13<-convertToCodaObject(pool_13)



#FIGURE 8 association plots-----

# residual association plots-
# Omega parameter, i.e., species co-occurrences 
OmegaCor = computeAssociations(pool_4L)
supportLevel = 0.90
for (r in 1:pool_4L$nr){ 
  plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE") 
  toPlot = ((OmegaCor[[r]]$support>supportLevel) +                                    (OmegaCor[[r]]$support<(1-supportLevel))>0)*OmegaCor[[r]]$mean 
  par(xpd=T) 
  colnames(toPlot)=rownames(toPlot)=gsub("_"," ",x=colnames(toPlot)) 
  corrplot(toPlot[plotOrder,plotOrder], method = "color", col=colorRampPalette(c("deepskyblue2","white","firebrick1"))(3), title=paste("random effect level:",pool_4L$rLNames[r]),type="full",tl.col="black",tl.pos='L', mar=c(0,0,6,0))
}

#export the four association plots, combined in Publisher
#note this is only Lower Pool 4


#Other Pool's residual association plots-
# Omega parameter, i.e., species co-occurrences 
OmegaCor = computeAssociations(pool_4L)
supportLevel = 0.90
HMSCFig7<-
  for (r in 1:pool_4L$nr){ 
    plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE") 
    toPlot = ((OmegaCor[[r]]$support>supportLevel) +                                    (OmegaCor[[r]]$support<(1-supportLevel))>0)*OmegaCor[[r]]$mean 
    par(xpd=T) 
    colnames(toPlot)=rownames(toPlot)=gsub("_"," ",x=colnames(toPlot)) 
    corrplot(toPlot[plotOrder,plotOrder], method = "color", col=colorRampPalette(c("deepskyblue2","white","firebrick1"))(3), title=paste("random effect level:",pool_4L$rLNames[r]),type="full",tl.col="black",tl.pos='L', mar=c(0,0,6,0))
    
    print(HMSCFig7)
    ggplot2::ggsave(filename = paste0("random effect level:",pool_4L,".png"),HMSCFig7, path = "C:\\Users\\dmlarson\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\")
  }

#supplemental association plots
OmegaCor = computeAssociations(pool_4U)
supportLevel = 0.90
HMSCFig7<-
  for (r in 1:pool_4U$nr){ 
    plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE") 
    toPlot = ((OmegaCor[[r]]$support>supportLevel) +                                    (OmegaCor[[r]]$support<(1-supportLevel))>0)*OmegaCor[[r]]$mean 
    par(xpd=T) 
    colnames(toPlot)=rownames(toPlot)=gsub("_"," ",x=colnames(toPlot)) 
    corrplot(toPlot[plotOrder,plotOrder], method = "color", col=colorRampPalette(c("deepskyblue2","white","firebrick1"))(3), title=paste("random effect level:",pool_4U$rLNames[r]),type="full",tl.col="black",tl.pos='L', mar=c(0,0,6,0))
    
    print(HMSCFig7)
    ggplot2::ggsave(filename = paste0("random effect level:",pool_4U,".png"),HMSCFig7, path = "C:\\Users\\dmlarson\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\")
  }

OmegaCor = computeAssociations(pool_8)
supportLevel = 0.90
HMSCFig7<-
  for (r in 1:pool_8$nr){ 
    plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE") 
    toPlot = ((OmegaCor[[r]]$support>supportLevel) +                                    (OmegaCor[[r]]$support<(1-supportLevel))>0)*OmegaCor[[r]]$mean 
    par(xpd=T) 
    colnames(toPlot)=rownames(toPlot)=gsub("_"," ",x=colnames(toPlot)) 
    corrplot(toPlot[plotOrder,plotOrder], method = "color", col=colorRampPalette(c("deepskyblue2","white","firebrick1"))(3), title=paste("random effect level:",pool_8$rLNames[r]),type="full",tl.col="black",tl.pos='L', mar=c(0,0,6,0))
    
    print(HMSCFig7)
    ggplot2::ggsave(filename = paste0("random effect level:",pool_8,".png"),HMSCFig7, path = "C:\\Users\\dmlarson\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\")
  }


OmegaCor = computeAssociations(pool_13)
supportLevel = 0.90
HMSCFig7<-
  for (r in 1:pool_13$nr){ 
    plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE") 
    toPlot = ((OmegaCor[[r]]$support>supportLevel) +                                    (OmegaCor[[r]]$support<(1-supportLevel))>0)*OmegaCor[[r]]$mean 
    par(xpd=T) 
    colnames(toPlot)=rownames(toPlot)=gsub("_"," ",x=colnames(toPlot)) 
    corrplot(toPlot[plotOrder,plotOrder], method = "color", col=colorRampPalette(c("deepskyblue2","white","firebrick1"))(3), title=paste("random effect level:",pool_13$rLNames[r]),type="full",tl.col="black",tl.pos='L', mar=c(0,0,6,0))
    
    print(HMSCFig7)
    ggplot2::ggsave(filename = paste0("random effect level:",pool_13,".png"),HMSCFig7, path = "C:\\Users\\dmlarson\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\")
  }



###END
