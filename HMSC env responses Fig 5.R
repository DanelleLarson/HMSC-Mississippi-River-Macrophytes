#HMSC results of macrophyte responses to the environment (beta estimates)

#Danelle updated in December 2025 (dmlarson@usgs.gov)

#session info
sessionInfo()


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

#libraries-----
library(Hmsc)


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



#FIG 5---------
# species environment relationship---------
#90% posterior probability show in color



png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig5_4Ufinal.png", width = 1600, height = 1200, res = 300)
beta=getPostEstimate(pool_4U, "Beta")
plotBeta(pool_4U, beta,
         supportLevel = 0.90,
         param = "Sign",
         plotTree = TRUE,
         spNamesNumbers = c(TRUE, FALSE),
         covNamesNumbers = c(TRUE, FALSE),
         colors = colorRampPalette(c("deepskyblue2", "white", "firebrick1")),
         split = 0.25,
         mar = c(6, 4, 2, 0),
         cex = c(0.5, 0.5, 0.6),
         marTree = c(6, 2, 2, 0))
dev.off()




png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig5_4Lfinal.png", width = 1600, height = 1200, res = 300)
beta=getPostEstimate(pool_4L, "Beta")
plotBeta(pool_4L, beta,
         supportLevel = 0.90,
         param = "Sign",
         plotTree = TRUE,
         spNamesNumbers = c(TRUE, FALSE),
         covNamesNumbers = c(TRUE, FALSE),
         colors = colorRampPalette(c("deepskyblue2", "white", "firebrick1")),
         split = 0.25,
         mar = c(6, 4, 2, 0),
         cex = c(0.5, 0.5, 0.6),
         marTree = c(6, 2, 2, 0))
dev.off()



png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig5_8final.png", width = 1600, height = 1200, res = 300)
beta=getPostEstimate(pool_8, "Beta")
plotBeta(pool_8, beta,
         supportLevel = 0.90,
         param = "Sign",
         plotTree = TRUE,
         spNamesNumbers = c(TRUE, FALSE),
         covNamesNumbers = c(TRUE, FALSE),
         colors = colorRampPalette(c("deepskyblue2", "white", "firebrick1")),
         split = 0.25,
         mar = c(6, 4, 2, 0),
         cex = c(0.5, 0.5, 0.6),
         marTree = c(6, 2, 2, 0))
dev.off()


png("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\Fig5_13final.png", width = 1600, height = 1200, res = 300)
beta=getPostEstimate(pool_13, "Beta")
plotBeta(pool_13, beta,
         supportLevel = 0.90,
         param = "Sign",
         plotTree = TRUE,
         spNamesNumbers = c(TRUE, FALSE),
         covNamesNumbers = c(TRUE, FALSE),
         colors = colorRampPalette(c("deepskyblue2", "white", "firebrick1")),
         split = 0.25,
         mar = c(6, 4, 2, 0),
         cex = c(0.5, 0.5, 0.6),
         marTree = c(6, 2, 2, 0))
dev.off()


###END

