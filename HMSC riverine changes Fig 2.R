
#HMSC Mississippi River environmental changes over time
#plots are the CWM environmental conditions per year for Lower Pool 4 only. 
#Danelle Larson (dmlarson@usgs.gov)



#Libraries-----
library(ggpubr) #ggarrange function for plotting


#Figure 2 in publication----------
#environmental changes over time
#plots are the CWM of environment. 

RCPtraitsenviron<-read.csv("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\RCPs\\RCPscripts\\RCPtraitsenviron_rawcommunity.csv")
str(RCPtraitsenviron)


p1<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = EXCEEDANCE_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  #stat_smooth(formula = y ~ s(x, k = 3), se = TRUE, col="black")+
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(A)") +
  ylim(90,125)+
  ylab("Exceedance")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (),axis.text = element_text(size = 12))

p2<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = CONNECTIVITY_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(B)") +
  ylab("Connectivity")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p3<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = VELOCITY_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(C)") +
  ylab("Velocity")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p4<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = AVGDEPTH_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(D)") +
  ylab("Depth")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p5<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = logP_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(E)") +
  ylab("logP")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))




p6<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = CHLOROPHYLL_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(F)") +
  ylab("Chlorophyll")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p7<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = TSS_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(G)") +
  ylab("TSS")+
  xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold", colour = "black"),
    axis.text.y=element_text(size = 12),
    axis.text.x = element_text(
      size = 12,
      angle = 45,
      hjust = 1,
      vjust = 1,
      margin = margin(t = 6)
    )
  )


p8<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = logN_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(H)") +
  ylab("logN")+
  xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold", colour = "black"),
    axis.text.y=element_text(size = 12),
    axis.text.x = element_text(
      size = 12,
      angle = 45,
      hjust = 1,
      vjust = 1,
      margin = margin(t = 6)
    )
  )

p9<-ggplot(RCPtraitsenviron) +
  aes(x = year, y = TEMPERATURE_mean) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0,w=0.4),colour = "gray")+
  # geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=lm, se=FALSE, linewidth=2)+
  labs(tag = "(I)") +
  ylab("Temperature")+
  xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold", colour = "black"),
    axis.text.y=element_text(size = 12),
    axis.text.x = element_text(
      size = 12,
      angle = 45,
      hjust = 1,
      vjust = 1,
      margin = margin(t = 6)
    )
  )


HMSCFig2 <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow=3,ncol=3)
HMSCFig2
ggsave("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Figures HMSC\\HMSCFig2final.jpg",HMSCFig2,dpi=600,width=8.5,height=11)
dev.off()





#Figure 2 unpublished------
#- environmental changes by RCP
#expected the environmental conditions to be similar by RCP, but wanted to be sure by plotting
RCPtraitsenviron2<-read.csv("C:\\Users\\dmlarson\\OneDrive\\OneDrive - DOI\\Hierarchical Modeling of Species Communities\\Analyses\\RCPs\\RCPscripts\\RCPtraitsenviron_rawcommunity.csv")
str(RCPtraitsenviron2)



p1<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = EXCEEDANCE_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(a)") +
  ylab("Exceedance")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (),axis.text = element_text(size = 12))

p2<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = CONNECTIVITY_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(b)") +
  ylab("Connectivity")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p3<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = VELOCITY_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(c)") +
  ylab("Velocity")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p4<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = AVGDEPTH_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(d)") +
  ylab("Depth")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p5<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = logP_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(e)") +
  ylab("log(total P)")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))




p6<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = CHLOROPHYLL_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(f)") +
  ylab("Chlorophyll a")+
  #xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"), axis.text.x=element_blank (), axis.title.x=element_blank (), axis.text = element_text(size = 12))

p7<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = TSS_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(g)") +
  ylab("TSS")+
  xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 10),axis.text.x = element_text (angle = 45))

p8<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = logN_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(h)") +
  ylab("log(total N)")+
  xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 10),axis.text.x = element_text (angle = 45))

p9<-ggplot(RCPtraitsenviron2) +
  aes(x = year, y = TEMPERATURE_mean, colour = factor(RCP_raw)) +
  geom_point(size= 2.5, alpha =0.6, position=position_jitter(h=0.9,w=0.9))+
  geom_jitter()+
  scale_color_viridis_d(option = "viridis") +
  stat_smooth(method=(lm), se=FALSE, linewidth=3)+
  scale_fill_viridis_d(option = "viridis") + 
  scale_color_viridis_d(option = "viridis") +
  labs(tag = "(i)") +
  ylab("Temperature")+
  xlab("Year")+
  expand_limits(x = 2022) + # or some other arbitrarily large number
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size = 12,
                                  face = "bold",
                                  colour = "black"),  axis.text = element_text(size = 10),axis.text.x = element_text (angle = 45))


HMSCFig2draftlinear <- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,nrow=3,ncol=3)
HMSCFig2draftlinear
ggsave("HMSCFig2draftlinear.jpg",HMSCFig2draftlinear,dpi=600,width=8.5,height=11)
dev.off()



###END
