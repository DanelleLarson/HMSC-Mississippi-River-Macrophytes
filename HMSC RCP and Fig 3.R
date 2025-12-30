#HMSC Mississippi River macrophyte 'region of common profile (RCP)' analysis
#created  by Danelle Larson (dmlarson@usgs.gov) in December 2025
#shows community shifts over time, increases in functional and taxonomic diversity indices
#shows increases in submersed plants and decline in free-floating plants




#ADD ORIGINAL RCP CLUSTER AALYSIS, TOO
#ADD Supp Fig 2, RCP changes through years by each pool


#the RCP's described are calculated and described from code named "Result processing Mississippi Macrophytes Adjusted 9Dec2022_DML edits.R"

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.


#load libraries----

library(dplyr) #need for 'inner_join' function
library(ggplot2) #for figuse
library(ggpubr) #for 'ggarrange' function
library(viridis) #for color on figures


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
                                  colour = "black"),  axis.text = element_text(size = 12))




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
                                  colour = "black"),  axis.text = element_text(size = 12))



HMSCFig3 <- ggarrange(p1,p2,p3,p4,p5,p6,ncol=2, nrow=3)
HMSCFig3
ggsave("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\HMSCFig3final.jpg",HMSCFig3,dpi=600,width=8.5,height=11)
dev.off()



#RCP heatmap of species, Supp Fig 1-----

#species prevalence for heatmap
phylo_ordered<-read.csv("RCP_prev_pool_L4_for_order.csv") 
rownames(phylo_ordered)<-(phylo_ordered[,1])
phylo_ordered<-as.matrix(phylo_ordered[,2:3])
phylo_ordered
#how to sort by phylogenetic tree?

HMSCheatmap<-heatmap(phylo_ordered, Colv = NA, Rowv = NA, main ="",scale= "column")
#lots of trouble with graphical parameters; maybe try something different if we like this





#Unused figures related to RCPs--------

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




###END


