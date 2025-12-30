#HMSC results of variance partitioning
#Danelle Larson (dmlarson@usgs.gov)

# Script for postproccessing HMSC results of all pools (with evaluation of model convergence for all models)
#Danelle updated in December 2025

#session info
sessionInfo()


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#libraries-----
library(Hmsc)
library(viridis)
library(kableExtra)



#Set WD----------
setwd("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\HMSC final fit models Dec 2025\\")#Danelle's WD

#### Load models -------

#### Upper pool 4 ####
load("DL_PoolU4_models_thin_1000_samples_250_chains_4.Rdata")
pool_4U<-models[[1]]
pool_4U


#### Lower pool 4 ####

load("DL_PoolL4_models_thin_1000_samples_250_chains_4.Rdata")
pool_4L<-models[[1]]
pool_4L


#### Pool 8 ####
load("DL_Pool8_models_thin_1000_samples_250_chains_4.Rdata")
pool_8<-models[[1]]
pool_8

#### Pool 13 ####
load("DL_Pool13_models_thin_1000_samples_250_chains_4.Rdata")
pool_13<-models[[1]]
pool_13


#Calculate predicted communities----------
#some are saved as matrices, others array? not joining in the next steps...
predY4U=computePredictedValues(pool_4U, expected=FALSE)
predY4L=computePredictedValues(pool_4L, expected=FALSE)
predY8=computePredictedValues(pool_8, expected=FALSE)
predY13=computePredictedValues(pool_13, expected=FALSE)
head(predY4U)


#model fits---------
MF_4U = evaluateModelFit(hM = pool_4U, predY = predY4U)
MF_4L = evaluateModelFit(hM = pool_4L, predY = predY4L)
MF_8 = evaluateModelFit(hM = pool_8, predY = predY8)
MF_13 = evaluateModelFit(hM = pool_13, predY = predY13)


####Variance partition------
#Do all four pools separately to allow for variation among them
#par(mfrow=c(1,1))
#par(mfrow=c(2,2)) # experimenting with making 4 panel plot given all model names are the same.
pool_4U$covNames
pool_4L$covNames
pool_8$covNames
pool_13$covNames 

#Group covariates into different schemes; published figures on group6

# Specify groups of how the variation should be partitioned // each covariate individually
#group1=c(1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)


# groups including temporal (mean) and spatial (resid) contribution
#group4=c(1,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2) 

# groups including hydro, wq, and climate
#group5=c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3) 

# groups including covariate groupings hydrogeomorphology, water quality, and climate separated into temporal and spatial effects
group6=c(1,1,2,1,2,1,2,1,2,3,4,3,4,3,4,3,4,5,6) 

#groupnames1 = pool_4L$covNames[-1] #Specify group names individually per covariate, less the intercept
#groupnames4 = c("temporal", "spatial")
#groupnames5 = c("hydrology", "water quality", "climate")
groupnames6 = c("hydrogeomorphology_mean (temporal)","hydrogeomorphology_residual (spatial)", "water quality_mean (temporal)", "water quality_residual (spatial)","climate_mean (temporal)","climate_residual (spatial)")





#compute species specific variance partitioning for covariate grouping 6
VP_U4_6 = computeVariancePartitioning(hM = pool_4U, group = group6, groupnames = groupnames6)
VP_L4_6 = computeVariancePartitioning(hM = pool_4L, group = group6, groupnames = groupnames6)
VP_8_6 = computeVariancePartitioning(hM = pool_8, group = group6, groupnames = groupnames6)
VP_13_6 = computeVariancePartitioning(hM = pool_13, group = group6, groupnames = groupnames6)

#output results (beta estimates) as data tables
#this is available in Supplemental Table 2 and Supplemental Table 3
VP_U4_6
VP_L4_6
VP_8_6
VP_13_6


VP_U4_6%>%
  kbl%>%
  kable_styling()

VP_L4_6%>%
  kbl%>%
  kable_styling()

VP_8_6%>%
  kbl%>%
  kable_styling()


VP_13_6%>%
  kbl%>%
  kable_styling()



#Figure 4--------
#Plot Tjur R 2 values for each pool


VPr = VP_U4_6
for(k in 1:pool_4U$ns){
  VPr$vals[,k] = MF_4U$TjurR2[k]*VPr$vals[,k]
}

VPr2 = VP_L4_6
for(k in 1:pool_4L$ns){
  VPr2$vals[,k] = MF_4L$TjurR2[k]*VPr2$vals[,k]
}

VPr3 = VP_8_6
for(k in 1:pool_8$ns){
  VPr3$vals[,k] = MF_8$TjurR2[k]*VPr3$vals[,k]
}

VPr4 = VP_13_6
for(k in 1:pool_13$ns){
  VPr4$vals[,k] = MF_13$TjurR2[k]*VPr4$vals[,k]
}


png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig4_U4final.tiff", width = 8, height = 8, units = 'in', res = 300)
barplot(VPr$vals, col=viridis(10, option ="turbo"), cex.names=1.5,cex.lab=1.5, cex.axis =1.5, cex.main=1.5, main="Upper Pool 4", ylab="Tjur R2", xlab = "", las=2,ylim = c(0, 0.8))
dev.off()


png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig4_L4final.tiff", width = 8, height = 8, units = 'in', res = 300)
barplot(VPr2$vals, col=viridis(10, option ="turbo"), cex.names=1.5,cex.lab=1.5, cex.axis =1.5, cex.main=1.5, main="Lower Pool 4", ylab="Tjur R2", xlab = "", las=2, ylim = c(0, 0.8))
dev.off()

png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig4_8final.tiff", width = 10, height = 8, units = 'in', res = 300)
barplot(VPr3$vals, col=viridis(10, option ="turbo"),cex.names=1.5,cex.lab=1.5, cex.axis =1.5, cex.main=1.5, main="Pool 8", ylab="Tjur R2", xlab = "", las=2,ylim = c(0, 0.8))
dev.off()

png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig4_13final.tiff", width = 8, height = 8, units = 'in', res = 300)
barplot(VPr4$vals, col=viridis(10, option ="turbo"), cex.names=1.5,cex.lab=1.5, cex.axis =1.5, cex.main=1.5, main="Pool 13", ylab="Tjur R2", xlab = "", las=2,ylim = c(0, 0.8))
dev.off()

#generic legend 
png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig4_legendfinal.tiff", width = 8, height = 8, units = 'in', res = 300)
barplot(VP_13_6$vals, col=viridis(10, option ="turbo"), ylab="proportion of explained variation", cex.lab=1, cex.axis = 1, main="Pool 13",las=2, legend.text = TRUE) #without legend
#legend("topright",
#legend = rownames(VPr4$vals),
#pch = 15,
#col=viridis(10, option ="turbo"),cex=2)
dev.off()

#combined four pools in Publisher for tight presentation




###END