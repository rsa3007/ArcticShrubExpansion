
# Script:  NMDS & PERMANOVAs Species Composition ----

#  Robin Satter                          

## Required Packages ----

library(ggpubr)
library(tidyverse)
library(readxl)
library(reshape2)
library(vegan)
library(patchwork)


## Preparing Data ---- 

a <- read_xlsx("Inv_Comp.xlsx")

shrubs<-read_xlsx("ShrubCover.xlsx")

c <- a %>% filter(Site == "C" )
s  <- a %>% filter(Site == "S")

s<-merge(s, shrubs, by="Sample_ID")


## Calculate NMDS Axis  ----

# Meadow Streams
nmds_c <- metaMDS(comm = c[ , 10:42], k = 2,  
                  distance = "bray",
                  trymax = 100, autotransform=T)

# Add NMDS values in data frame
data.c = as.data.frame(scores(nmds_c)$sites)

data.c$Code = c$Code
data.c$Number =c$Number
data.c$Station = c$Station

# Tall Shrub Streams
nmds_s <- metaMDS(comm = s[ , 10:42], k = 2,  
                  distance = "bray",
                  trymax = 100, autotransform=T)

# Add NMDS values in data frame
data.s = as.data.frame(scores(nmds_s)$sites)

data.s$Code = s$Code
data.s$Shrub = s$Percent20
data.s$Number =s$Number
data.s$Station = s$Station

### NMDS Plot (Figure 6) ----

p1 <-ggplot() +
  
  stat_conf_ellipse(data = data.c, aes(x=NMDS1, y=NMDS2, color=Code),  level=0.95)+
  
  geom_point(data = data.c, aes(x = NMDS1, y = NMDS2, 
                                color = Code, shape=Code) ,size = 1.5) +
  
  annotate(geom = "label", x = 1.5, y = 0.6, size = 3,
           label = paste("2D Stress: ", round(nmds_c$stress, digits = 2))) +
  
  scale_color_manual(values=c("#e41a1c", "#377eb8"),
                     labels=c("Downstream", "Upstream"))+
  
  
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream", "Upstream"))+
  
  scale_shape_manual(values=c(15,19),
                     labels=c("Downstream","Upstream"))+
  
  labs(colour="Meadow", shape="Meadow", fill="Meadow")+
  
  theme_bw()+
  
  theme(legend.position=c(0.12,0.8), 
        legend.background = element_rect(fill='transparent', colour="transparent"),
        legend.box.background = element_rect(fill='transparent', colour="transparent"),
        legend.text=element_text(size=7),
        legend.key.size=unit(0.5,'cm'),
        axis.text=element_text(size=10, colour="black"),
        axis.title.y = element_text(size=10, face ="bold"),
        axis.text.x = element_blank(),
        panel.border = element_rect(size=1.2),
        plot.tag.position = c(0.04, 0.99),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold"))+
  
  ylab("NMDS2")+
  xlab("")+
  
  coord_fixed(ratio = 1, xlim = c(-1.7,1.8), ylim = NULL, expand = TRUE, clip = "on")

p2 <-ggplot() +
  
  stat_conf_ellipse(data = data.s, aes(x=NMDS1, y=NMDS2, color=Code),  level=0.95)+
  
  geom_point(data = data.s, aes(x = NMDS1, y = NMDS2, 
                                fill = Code, shape=Code, color=Code), 
             alpha=(2*data.s$Shrub) ,size = 2) +
  
  annotate(geom = "label", x = 1.5, y = 0.6, size = 3,
              label = paste("2D Stress: ", round(nmds_s$stress, digits = 2))) +
  
  scale_color_manual(values=c("#4daf4a", "#984ea3"),
                     labels=c("Downstream", "Upstream"))+
  
  scale_fill_manual(values=c("#4daf4a", "#984ea3"),
                    labels=c("Downstream", "Upstream"))+
  
  scale_shape_manual(values=c(17,18),
                     labels=c("Downstream","Upstream"))+
  
  labs(colour="Tall Shrub", shape="Tall Shrub", fill="Tall Shrub")+
  
  theme_bw()+
  
  theme(legend.position=c(0.12,0.8), 
        legend.background = element_rect(fill='transparent', colour="transparent"),
        legend.box.background = element_rect(fill='transparent',
                                             colour="transparent", size=2),
        legend.key=element_blank(),
        legend.text=element_text(size=7),
        legend.key.size=unit(0.4,'cm'),
        axis.text=element_text(size=10, colour="black"),
        axis.title = element_text(size=10, face="bold"),
        panel.border = element_rect(size=1.2),
        plot.tag.position = c(0.04, 0.99),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold"))+
  
  ylab("NMDS2")+
  xlab("NMDS1")+
  
  coord_fixed(ratio = 1, xlim = c(-1.7,1.8), ylim = NULL, expand = TRUE, clip = "on")


#### Patchwork ----

p1/p2+
  plot_annotation(tag_levels = 'A')


## PERMANOVA ----

## Create Bray-Curtis distance matrix

dist_c <- vegdist(c[, 10:42],  method = "bray")
dist_s <- vegdist(s[, 10:42],  method = "bray") 

# Meadow Streams
adonis2(dist_c ~ as.factor(data.c$Number) + data.c$Station, method = "bray", na.action=na.exclude)

# Tall Shrub Streams
adonis2(dist_s ~ as.factor(data.s$Number) + data.s$Station, method = "bray", na.action=na.exclude)
