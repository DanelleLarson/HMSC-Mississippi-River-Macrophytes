#HMSC results of macrophyte phylogenetic signals and functional traits
#Danelle updated in December 2025 (dmlarson@usgs.gov)


#session info
sessionInfo()


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#libraries-----
library(Hmsc)
library(tidyverse)
library(kableExtra)



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



# phylogenetic signal---------- 
#high rho values near 1 indicate a strong taxonomic niche conservatism signal
plot(post_pool_4U$Rho)
summary(post_pool_4U$Rho)

plot(post_pool_4L$Rho)
summary(post_pool_4L$Rho)

plot(post_pool_8$Rho)
summary(post_pool_8$Rho)

plot(post_pool_13$Rho)
summary(post_pool_13$Rho)




#Suppl Figure 9 ------
#trait environment relationship
gamma=getPostEstimate(pool_4U, "Gamma")
plotGamma(pool_4U, gamma, supportLevel=.90,param = "Sign",colors=colorRampPalette(c("deepskyblue2","white","firebrick1")) ) 
gamma=getPostEstimate(pool_4L, "Gamma")
#head(gamma)
plotGamma(pool_4L, gamma, supportLevel=.90,param = "Sign",colors=colorRampPalette(c("deepskyblue2","white","firebrick1")) ) 
#SAV negatively associated with logN and TSS as expected, positive with hydrology
gamma=getPostEstimate(pool_8, "Gamma")
plotGamma(pool_8, gamma, supportLevel=.90,param = "Sign",colors=colorRampPalette(c("deepskyblue2","white","firebrick1")) ) 
gamma=getPostEstimate(pool_13, "Gamma")
plotGamma(pool_13, gamma, supportLevel=.90,param = "Sign",colors=colorRampPalette(c("deepskyblue2","white","firebrick1")) ) 

#Danelle modified in Publisher to combine all pool results and make professional looking







## Functional Trait responses to environmental changes-------

#compute species specific variance partitioning for all individual covariates
# groups including covariate groupings hydrogeomorphology, water quality, and climate separated into temporal and spatial effects
group6=c(1,1,2,1,2,1,2,1,2,3,4,3,4,3,4,3,4,5,6) 
groupnames6 = c("hydrogeomorphology_mean (temporal)","hydrogeomorphology_residual (spatial)", "water quality_mean (temporal)", "water quality_residual (spatial)","climate_mean (temporal)","climate_residual (spatial)")

#compute species specific variance partitioning for covariate grouping 6, for trait variation explained
VP_U4_6 = computeVariancePartitioning(hM = pool_4U, group = group6, groupnames = groupnames6)
VP_L4_6 = computeVariancePartitioning(hM = pool_4L, group = group6, groupnames = groupnames6)
VP_8_6 = computeVariancePartitioning(hM = pool_8, group = group6, groupnames = groupnames6)
VP_13_6 = computeVariancePartitioning(hM = pool_13, group = group6, groupnames = groupnames6)

#The modelled functional traits of life form and pollination mode for the total explained variation in species occurrences
VP_U4_6$R2T$Y 
VP_L4_6$R2T$Y# 40%, quite a lot!
VP_8_6$R2T$Y
VP_13_6$R2T$Y

#Suppl Fig 10-------
#how much do the traits explain out of the variation among the species in their responses to environmental covariates?
#which fixed effects are driving the total variation explained for trait responses? 

VP_U4_6$R2T$Beta%>%
  kbl%>%
  kable_styling()
par(mar = c(10, 4, 2, 1))
barplot(VP_U4_6$R2T$Beta,main="Upper Pool 4", las=2, ylim = c(0.0, 0.8), ylab="Variance explained by fixed effects") 

VP_L4_6$R2T$Beta%>%
  kbl%>%
  kable_styling()
par(mar = c(10, 4, 2, 1))
barplot(VP_L4_6$R2T$Beta,main="Lower Pool 4", las=2, ylim = c(0.0, 0.8), ylab="Variance explained by fixed effects") 

VP_8_6$R2T$Beta%>%
  kbl%>%
  kable_styling()
par(mar = c(10, 4, 2, 1))
barplot(VP_8_6$R2T$Beta,main="Pool 8", las=2, ylim = c(0.0, 0.8), ylab="Variance explained by fixed effects") 

VP_13_6$R2T$Beta%>%
  kbl%>%
  kable_styling()
par(mar = c(10, 4, 2, 1))
barplot(VP_13_6$R2T$Beta,main="Pool 13", las=2, ylim = c(0.0, 0.8), ylab="Variance explained by fixed effects") 


###END

