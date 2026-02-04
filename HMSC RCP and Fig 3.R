#HMSC Mississippi River macrophyte 'region of common profile (RCP)' analysis
#Written by Benjamin Weigel and Danelle Larson (dmlarson@usgs.gov; danellelarson88@gmail.com) 
#updated in December 2025
#calculates RCPs for Lower Pool 4 macrophytes and all pool macrophytes
#shows community shifts over time, increases in functional and taxonomic diversity indices
#shows increases in submersed plants and decline in free-floating plants


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


#load libraries----

library(dplyr) #need for 'inner_join' function
library(ggplot2) #for figuse
library(ggpubr) #for 'ggarrange' function
library(viridis) #for color on figures
library(dplyr) #manipulate data frames
library(Hmsc) #only needed if computing predicted community values
library(abind)
library(vegan) #for the k-means clustering algorithm
library(NbClust) #number of optimal clusters, using the Calinski criterion
library(FD) #functional dispersion / diversity metric calculation
library(heatmap3) #heat map to show RCPs species composition
library(kableExtra) #kable styling for RCP heat map



#Set WD, load HMSC model----------
#using Lower Pool 4 only; computation resources were too great for all pools. Slight differences in species comp among pools may cloud a RCP analysis of the entire large river system
#load("/Users/weigel/Desktop/Mississippi Macrophytes/transfer_208222_files_4f02296c/DL_PoolL4_models_thin_1000_samples_250_chains_4 (2).Rdata")
setwd("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\RCPs\\RCPscripts\\") #Danelle's WD
load("DL_PoolL4_models_thin_1000_samples_250_chains_4 (2).Rdata") #HMSC output data
pool_4L<-models[[1]]


#Calculate predicted communities----------
#optional, if using predicted communities for RCP analysis

predY4L=computePredictedValues(pool_4L, expected=FALSE)
head(predY4L)

#model performance--------
#model fit Pool 4L
MF = evaluateModelFit(hM = pool_4L, predY = predY4L) 
mean(MF$AUC) #0.93 = excellent
mean(MF$TjurR2) #0.31 = good


###RCPs k means clustering algorithm---------

###### Calculate optimal clusters for RCP based on raw, binary data for Lower Pool 4 only
# cluster on raw data with unvegetated sites removed and using Jaccard index
#make community data frame
comm4L<-as.data.frame(pool_4L$Y)
head(comm4L)
#Add plot ID as row names
rownames(comm4L)=pool_4L$studyDesign$plot
head(comm4L)
#select all plots with >0 species; loses about 29% of sites. Ok because we want to cluster by species composition similarities, not joint absences.
comm4L_no0<-comm4L %>% 
  filter(rowSums(across(where(is.numeric)))!=0)
head(comm4L_no0)

#original compute of Jaccard dissimilarity for binary data and find best cluster
#WARNING: Takes at least 2 hours to calculate dissimiliarity and fit. 
bi_dist_comm4L<-dist.binary(comm4L_no0, method = 1, diag = FALSE, upper = FALSE) 
fit_comm4L <- cascadeKM(bi_dist_comm4L, 1, 10, iter = 1000)

#or load from existing run ("fit_comm4L.Rdata"), to save ~2 hours to calculate dissimilarity and fit
load("fit_comm4L.Rdata")

plot(fit_comm4L, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit_comm4L$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 2 clusters! assign each site to a cluster
RCP_4L<-as.data.frame(as.factor(fit_comm4L$partition[,2]))
colnames(RCP_4L)<-"RCP_4L"
#save(fit_comm4L, file="fit_comm4L.Rdata")

###############################################


#load data frames----
#RCPtraitsAll<-read.csv("RCPtraitsAll.csv") #included raw data with unvegetated plots
#unsure how this dataset ("RCPtraitsAllRawJaccard.csv") was created. Need to rectify in code.
RCPtraitsAll<-read.csv("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\RCPs\\RCPscripts\\RCPtraitsAllRawJaccard.csv") #includes vegetated plots only from Lower Pool 4, Jaccard index on 'raw'/not predicted community Dec 22,2022

str(RCPtraitsAll)

#add coordinates - creates "AllData" and AllData2Years" df
coords<-read.csv2("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\RCPs\\RCPscripts\\Plot_coords_L4.csv")
head(coords)
#colnames(coords)<-c("plot.1","x","y","year.1")
#coords2<-coords[-c(1377),] #remove row 1377 to match number of rows in RCPTraitsAll
#AllData<-cbind(coords,RCPtraitsAll)
AllData<-inner_join(RCPtraitsAll,coords,by="plot")
str(AllData)

AllData<- AllData %>%
  rename(year = year.x)%>%
  rename(x = EASTING)%>%
  rename(y = NORTHING)



#Figure 3 -----
#describes RCPs and temporal changes, without data points

p1<-ggplot(AllData) +
  aes(x = year, fill = factor(RCP_raw), col= factor(RCP_raw)) +
  geom_bar(position="fill") +
  scale_fill_viridis_d(option = "viridis") + 
  #scale_color_viridis_d(option = "viridis") + 
  theme_classic()+ 
  labs(tag = "(A)") +
  xlab("Year")+
  ylab("Proportion of plots")+
  xlim(1998,2022)+
  scale_color_viridis_d(option = "viridis",'RCP',labels=c("RCP1", "RCP2")) +
  #facet_wrap(vars(stratum))+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 12),axis.text.x=element_blank(),axis.title.x=element_blank())


p2<-ggplot(AllData) +
  aes(x = year, y = nbsp, colour = factor(RCP_raw)) +
  #geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4))+
  #geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  #stat_smooth( formula = y ~ s(x, k = 3), se = TRUE, col="black")+
  stat_smooth(method=(lm))+
  labs(tag = "(B)") +
  ylab("Species richness")+xlab("Year")+
  xlim(1998,2022)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 12),axis.text.x=element_blank(),axis.title.x=element_blank())


p3<-ggplot(AllData) +
  aes(x = year, y = FDis, col= factor(RCP_raw)) +
  #geom_point() +
  stat_smooth(method=lm)+
  labs(tag = "(C)") +
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") + 
  xlab("Year") + ylab("FDis")+
  xlim(1998,2022)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 12),axis.text.x=element_blank(),axis.title.x=element_blank())

p4<-ggplot(AllData) +
  aes(x = year, y = LIFE_FORMsubmersed, colour = factor(RCP_raw)) +
  #geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4))+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  #stat_smooth(formula = y ~ s(x, k = 3), se = TRUE, col="black")+
  stat_smooth(method=(lm))+
  labs(tag = "(D)") +
  ylab("Submersed plants")+
  xlab("Year")+
  xlim(1998,2022)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 12),axis.text.x=element_blank(),axis.title.x=element_blank())

p5<-ggplot(AllData) +
  aes(x = year, y = LIFE_FORMfloating, colour = factor(RCP_raw)) +
  #geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4))+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis",'RCP',labels=c("RCP1", "RCP2")) +
  #stat_smooth(formula = y ~ s(x, k = 3), se = TRUE, col="black")+
  stat_smooth(method=lm)+
  labs(tag = "(E)") +
  ylab("Free-floating plants")+
  xlab("Year")+
  xlim(1998,2022)+
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 12), axis.text.x = element_text(
                                    size = 12,
                                    angle = 45,
                                    hjust = 1,
                                    vjust = 1,
                                    margin = margin(t = 6)
                                  ))




p6<-ggplot(AllData) +
  aes(x = year, y = POLLINATIONepihydrophily, colour = factor(RCP_raw)) +
  #geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4))+
  # geom_jitter()+
  #scale_color_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis",'RCP',labels=c("RCP1", "RCP2")) +
  #stat_smooth(formula = y ~ s(x, k = 3), se = TRUE, col="black")+
  stat_smooth(method=(lm))+
  labs(tag = "(F)") +
  ylab("Epihydrophily")+
  xlab("Year")+
  xlim(1998,2022)+
  theme_classic()+
  theme(legend.position="bottom",legend.title = element_blank(),legend.text=element_text(size = 12),
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 12), axis.text.x = element_text(
                                    size = 12,
                                    angle = 45,
                                    hjust = 1,
                                    vjust = 1,
                                    margin = margin(t = 6)
                                  ))



HMSCFig3 <- ggarrange(p1,p2,p3,p4,p5,p6,ncol=2, nrow=3)
HMSCFig3
ggsave("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\HMSCFig3final.jpg",HMSCFig3,dpi=600,width=8.5,height=11)
dev.off()



#Supp Fig 1, RCP heatmap of species-----

#species prevalence for heatmap
phylo_ordered<-read.csv("RCP_prev_pool_L4_for_order.csv") 
rownames(phylo_ordered)<-(phylo_ordered[,1])
phylo_ordered<-as.matrix(phylo_ordered[,2:3])
phylo_ordered

HMSCheatmap<-heatmap(phylo_ordered, Colv = NA, Rowv = NA, main ="",scale= "column")



#Unpublished figures--------

#Lower Pool 4 map of RCP by stratum (BWC = backwaters; MBC = main border channel)
ggplot(AllData) +
  aes(x = x, y = y, colour = factor(RCP_raw)) +
  scale_color_viridis_d(option = "viridis") +
  geom_point(size = 1.5, alpha = 0.6) +
  theme_classic()+
  labs(tag = "(a)") +
  xlab("Easting")+
  ylab("Northing")+
  facet_wrap(vars(stratum))+
  theme(legend.position = "none",
        axis.title = element_text(size = 15,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 15))


#violin plots- RCP2 is more functionally diverse
ggplot(AllData) +
  aes(x = factor(RCP_raw), y = FDis, fill = factor(RCP_raw), col= factor(RCP_raw)) +
  geom_violin() +
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black" )+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") + 
  labs(tag = "(d)") +
  ylab("FDis")+xlab("RCP")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 15,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 12))



#Supp Fig 2, RCP changes through years by each pool--------

#first, calculate RCPs for all pools simultaneously
load("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\RCPs\\RCPs by Ben\\allData.Rdata")

#load packages
library(ade4) #calculate dissimilarity matrix
library(cluster) # calculate pairwise dissimilarities
library(dplyr)

#need to remove unvegetated sites because our aim is to describe how communities are similar, and clustering will try to make centroids with greatest distance
#select all plots with >0 species 

Y<-as.data.frame(Y)
#add PLOT column to dataframe Y to match up later...
rownames(S)=S$PLOT
Y$PLOT <- rownames(S)
head(Y)

comm_no0<-Y %>% 
  filter(rowSums(across(where(is.numeric)))!=0)
head(comm_no0)


# calculate distance dissimilarities data using the ade4 package
dist<-dist.binary(comm_no0, method = 1,diag = FALSE, upper = FALSE) #method 1 is Jaccard index

# calculate pairwise dissimilarities in the cluster package. Also suitable for binary data
dist_daisy<-daisy(Y) 


#cluster on distance matrix; beware -- took 18 hours!
fit_dist <- cascadeKM(dist, inf.gr=1,sup.gr= 6, iter = 100, 
                      criterion = "calinski")
plot(fit_dist, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit_dist$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

# Data suggests 2 clusters are best
RCP2<-as.factor(fit_dist$partition[,2])


#Make data frame including RCPs for processing
data<-cbind(S,RCP2,RCP4,RCP2kmeans) #cbind doesn't work b/c removed sites with no veg
data<-merge(S,RCP2,by=0) #merges by row.name, inner join
str(data)
#rename column y to RCP2
rename(data, RCP2_dist = y)

#write RCP info to a new data frame because running cascadeKM on the distance matrix took a long time.
write.csv(data, "RCPs_allpools_distmatrix.csv")



#plot RCP shifts over time

data<-read.csv("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\RCPs\\RCPs by Ben\\RCPs_allpools_distmatrix.csv")
#includes vegetated sites from 4 pools, RCP classification (y) is included

# RCP proportions over time - improved graphics
#reorder pools
levels(data$POOL)
data$Pool <- factor(data$POOL, levels=c('U4', 'L4', '8', '13'))
levels(data$Pool)
#name pools
levels(data$Pool) <- c('Upper Pool 4', 'Lower Pool 4', 'Pool 8', 'Pool 13')


data <- data %>%
  rename(
    RCP2 = y,
)

data$RCP2 <- factor(data$RCP2)

#density distribution of RCPs by pool over time     
# here RCP progression visualized as density distribution over time 
#for 2RCPs 
shiftsdensity<-ggplot(data) +
  aes(x = YEAR, fill = RCP2) +
  geom_density(adjust = 1L) +
  scale_fill_viridis_d(alpha = 0.5) +
  theme_minimal()+
  geom_density() +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  facet_wrap(vars(Pool), scales="free")+
  xlab("Year") + ylab("density")+
  theme_classic()+
  labs(fill='RCP')+
  theme(axis.text.x = element_text(angle = 45))+
  theme(legend.position="bottom")

ggsave("RCPshiftsdensity.jpg",shiftsdensity,dpi=600,width=8.5,height=7.5) 

#extra exploration plots not used for all pool data

#by stratum/habitat type      
ggplot(data) +
  aes(x = YEAR, fill = RCP2) +
  geom_bar(position="fill") +
  ylab("proportion of sites")+
  scale_fill_hue() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))+
  ggtitle("Pool RCP proportion over time")+
  facet_wrap(vars(STRATUM), scales="free")
#RCP1 increasing in IMP due to Vallisneria?

# Plotting all sampling points with RCP color geographically, per pool. 
ggplot(data) +
  aes(x = EASTING, y = NORTHING, colour = RCP2)+
  geom_point(size=1, alpha= 0.5) +
  scale_color_brewer(palette = "RdYlBu") +
  theme_bw() +
  facet_wrap(vars(POOL), scales="free")






###END


