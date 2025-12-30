#HMSC model performances for Mississippi macrophytes
#Danelle Larson (dmlarson@usgs.gov)

#session info
sessionInfo()


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#Libraries-----
library(Hmsc)
library(vioplot)


#Set WD----------
setwd("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\HMSC final fit models Dec 2025\\")#Danelle's WD


#Load models & diagnostics -------
#initial HMSC models converged. Explore each model performance for all four pools
#these metrics assess MCMC convergence
#psrf = potential scale reduction factor
#ess = effective sample size


#### Upper pool 4 ####
load("DL_PoolU4_models_thin_1000_samples_250_chains_4.Rdata")
pool_4U<-models[[1]]
pool_4U

post_pool_4U<-convertToCodaObject(pool_4U)

par(mfrow=c(2,3))

# Beta parameters 
hist(effectiveSize(post_pool_4U$Beta), main="ess(beta 4U)")
hist(gelman.diag(post_pool_4U$Beta, multivariate = FALSE)$psrf, main="psrf(beta 4U)")
vioplot(gelman.diag(post_pool_4U$Beta,multivariate = FALSE)$psrf,main="psrf(beta 4U)")

# Gamma parameters 
hist(effectiveSize(post_pool_4U$Gamma), main="ess(gamma U4)")
hist(gelman.diag(post_pool_4U$Gamma, multivariate = FALSE)$psrf, main="psrf(gamma U4)")
vioplot(gelman.diag(post_pool_4U$Gamma,multivariate = FALSE)$psrf,main="psrf(gamma U4)")

psrf_p4U<-(gelman.diag(post_pool_4U$Beta, multivariate=FALSE)$psrf)
psrf_p4U<-as.data.frame(psrf_p4U)
mean(psrf_p4U$`Point est.`)
mean(psrf_p4U$`Upper C.I.`)

#### Lower pool 4 ####
load("DL_PoolL4_models_thin_1000_samples_250_chains_4.Rdata")

pool_4L<-models[[1]]
pool_4L

post_pool_4L<-convertToCodaObject(pool_4L)

par(mfrow=c(2,6))

# Beta parameters
hist(effectiveSize(post_pool_4L$Beta), main="ess(beta 4L)")
hist(gelman.diag(post_pool_4L$Beta, multivariate = FALSE)$psrf, main="psrf(beta 4L)")
vioplot(gelman.diag(post_pool_4L$Beta,multivariate = FALSE)$psrf,main="psrf(beta 4L)")

# Gamma parameters
hist(effectiveSize(post_pool_4L$Gamma), main="ess(gamma 4L)")
hist(gelman.diag(post_pool_4L$Gamma, multivariate = FALSE)$psrf, main="psrf(gamma 4L)")
vioplot(gelman.diag(post_pool_4L$Gamma,multivariate = FALSE)$psrf,main="psrf(gamma 4L)")

psrf_P4_L<-(gelman.diag(post_pool_4L$Beta, multivariate=FALSE)$psrf)
psrf_P4_L<-as.data.frame(psrf_P4_L)
mean(psrf_P4_L$`Point est.`)
mean(psrf_P4_L$`Upper C.I.`)

#### Pool 8 ####
load("DL_Pool8_models_thin_1000_samples_250_chains_4.Rdata")

pool_8<-models[[1]]
pool_8

post_pool_8<-convertToCodaObject(pool_8)

par(mfrow=c(2,3))

# Beta parameters 
hist(effectiveSize(post_pool_8$Beta), main="ess(beta 8)")
hist(gelman.diag(post_pool_8$Beta, multivariate = FALSE)$psrf, main="psrf(beta 8)")
vioplot(gelman.diag(post_pool_8$Beta,multivariate = FALSE)$psrf,main="psrf(beta 8)")

# Gamma parameters 
hist(effectiveSize(post_pool_8$Gamma), main="ess(gamma 8)")
hist(gelman.diag(post_pool_8$Gamma, multivariate = FALSE)$psrf, main="psrf(gamma 8)")
vioplot(gelman.diag(post_pool_8$Gamma,multivariate = FALSE)$psrf,main="psrf(gamma 8)")

psrf_p8<-(gelman.diag(post_pool_8$Beta, multivariate=FALSE)$psrf)
psrf_p8<-as.data.frame(psrf_p8)
mean(psrf_p8$`Point est.`)
mean(psrf_p8$`Upper C.I.`)

#### Pool 13 ####
load("DL_Pool13_models_thin_1000_samples_250_chains_4.Rdata")

pool_13<-models[[1]]
pool_13

post_pool_13<-convertToCodaObject(pool_13)
par(mfrow=c(1,3))

# Beta parameters 
hist(effectiveSize(post_pool_13$Beta), main="ess(beta 13)")
hist(gelman.diag(post_pool_13$Beta, multivariate = FALSE)$psrf, main="psrf(beta 13)")
vioplot(gelman.diag(post_pool_13$Beta,multivariate = FALSE)$psrf,main="psrf(beta) 13")

# Gamma parameters 
hist(effectiveSize(post_pool_13$Gamma), main="ess(gamma 13)")
hist(gelman.diag(post_pool_13$Gamma, multivariate = FALSE)$psrf, main="psrf(gamma 13)")
vioplot(gelman.diag(post_pool_13$Gamma,multivariate = FALSE)$psrf,main="psrf(gamma 13)")

psrf_p13<-(gelman.diag(post_pool_13$Beta, multivariate=FALSE)$psrf)
psrf_p13<-as.data.frame(psrf_p13)
mean(psrf_p13$`Point est.`)
mean(psrf_p13$`Upper C.I.`)


#Calculate predicted communities----------
predY4U=computePredictedValues(pool_4U, expected=FALSE)
predY4L=computePredictedValues(pool_4L, expected=FALSE)
predY8=computePredictedValues(pool_8, expected=FALSE)
predY13=computePredictedValues(pool_13, expected=FALSE)
head(predY4U)


#Model performance metrics--------
#includes explanatory power and prediction powers

#model fit Pool_4U
MF_4U = evaluateModelFit(hM = pool_4U, predY = predY4U)
MF_4U$TjurR2 
mean(MF_4U$AUC) #0.98 = excellent
mean(MF_4U$TjurR2) #0.34


#model fit Pool 4L
MF_4L = evaluateModelFit(hM = pool_4L, predY = predY4L)
MF_4L$TjurR2
mean(MF_4L$AUC) #0.93 = excellent
mean(MF_4L$TjurR2) #0.31


#model fit Pool 8
MF_8 = evaluateModelFit(hM = pool_8, predY = predY8)
MF_8$TjurR2
mean(MF_8$AUC) #0.96 = excellent
mean(MF_8$TjurR2) #0.31

#model fit Pool 13
MF_13 = evaluateModelFit(hM = pool_13, predY = predY13)
MF_13$TjurR2
mean(MF_13$AUC) #0.96 = excellent
mean(MF_13$TjurR2) #0.35


#calculate predictive power of models using R2 metric
#WARNING - 2 fold cross validation took 80 hours for a single pool!!!
partition4U<-createPartition(pool_4U, nfolds=2)
predictivepower4U<-computePredictedValues(pool_4U,partition=partition4U)

partition4L<-createPartition(pool_4L, nfolds=2)
predictivepower4L<-computePredictedValues(pool_4L,partition=partition4L)

partition8<-createPartition(pool_8, nfolds=2)
predictivepower8<-computePredictedValues(pool_8,partition=partition8)

partition13<-createPartition(pool_13, nfolds=2)
predictivepower13<-computePredictedValues(pool_13,partition=partition13)

###END
