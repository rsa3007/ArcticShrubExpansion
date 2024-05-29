
# Script:  NMDS & PERMANOVAs Functional Composition ----

#  Robin Satter                          

## Required Packages ----

library(readxl)
library(tidyverse)
library(devtools)
library(patchwork)
library(writexl)
library(reshape2)
library(vegan)
library(ggpubr)

## Preparing Data ----

a <-read_xlsx("cwm_nmds.xlsx")

b <-read_xlsx("fddata.xlsx")

c <- cbind(a, b)

shrubs <- read_xlsx("ShrubCover.xlsx")


## Calculate NMDS Axis ----

# Meadow Streams
m <- c %>% filter(Site=="C")
nmds_m <- metaMDS(comm = m[1:4], k = 2, 
                       distance = "bray",
                       trymax = 100)

# Add NMDS values in data frame
data.m = as.data.frame(scores(nmds_m)$sites)

data.m$Number = m$Number
data.m$Code = m$Code
data.m$Station = m$Station

# Tall Shrub Streams
sh <- c %>% filter(Site=="S")
sh<-merge(sh, shrubs, by="Sample_ID")
nmds_sh <- metaMDS(comm = sh[2:5], k = 2, 
                   distance = "bray",
                   trymax = 100)

# Add NMDS values in data frame
data.sh = as.data.frame(scores(nmds_sh)$sites) # Extracting Sites

data.sh$Number = sh$Number
data.sh$Code = sh$Code
data.sh$Station = sh$Station
data.sh$Shrub = sh$Percent20

### NMDS Plot (Figure 8)----


p1 <- ggplot() +
  
  stat_conf_ellipse(data = data.m, aes(x=NMDS1, y=NMDS2, color=Code),
                    level=0.95)+
  annotate(geom = "label", x = 0.95, y = 0.53, size = 3,
           label = paste("2D Stress: ", round(nmds_m$stress, digits = 2))) +
  
  geom_point(data = data.m, aes(x = NMDS1, y = NMDS2, 
                                     fill = Code, shape=Code, 
                                     colour=Code), size = 1.5)+
  
  scale_color_manual(values=c("#e41a1c", "#377eb8"),
                     labels=c("Downstream", "Upstream"))+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  scale_shape_manual(values=c(15,19),
                     labels=c("Downstream", "Upstream"))+
  labs(colour="Meadow", shape="Meadow",fill="Meadow")+
  xlab("")+
  
  theme_bw()+
  
  theme(legend.position=c(0.16,0.77), 
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour="transparent"),
        legend.text=element_text(size=7), 
        axis.text.y=element_text(size=10, colour="black"),
        axis.title = element_text(size=10, face="bold"),
        axis.text.x = element_blank(),
        panel.border = element_rect(size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold"))+
  
  coord_fixed(ratio = 1, xlim = c(-1,1.2), ylim = c(-0.7,0.6), expand = TRUE, clip = "on")


p2 <-ggplot() +

  
  stat_conf_ellipse(data = data.sh, aes(x=NMDS1, y=NMDS2, color=Code),
                    level=0.95)+
  annotate(geom = "label", x = 0.92, y = 0.46, size = 3,
           label = paste("2D Stress: ", round(nmds_sh$stress, digits = 2))) +
  
  geom_point(data = data.sh, aes(x = NMDS1, y = NMDS2, 
                                     fill = Code, shape=Code, 
                                     colour=Code),alpha=(2*data.sh$Shrub), size = 2)+
  
  scale_color_manual(values=c("#4daf4a", "#984ea3"),
                     labels=c("Downstream", "Upstream"))+
  scale_fill_manual(values=c("#4daf4a", "#984ea3"),
                    labels=c("Downstream","Upstream"))+
  scale_shape_manual(values=c(17,18),
                     labels=c("Downstream", "Upstream"))+
  labs(colour="Tall Shrub", shape="Tall Shrub",fill="Tall Shrub")+
  
  theme_bw()+
  
  theme(legend.position=c(0.16,0.77),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour="transparent"),
        legend.text=element_text(size=7), 
        axis.text=element_text(size=10, colour="black"),
        axis.title = element_text(size=10, face="bold"),
        panel.border = element_rect(size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold"))+
  
  coord_fixed(ratio = 1, xlim = c(-1, 1.2), ylim = NULL, expand = TRUE, clip = "on")

#### Patchwork ----

p1/p2+
  plot_annotation(tag_levels = 'A')

## PERMANOVA ----

dist_m <- vegdist(m[, 1:4],  method = "bray") # Create distance matrix.
dist_sh <- vegdist(sh[, 2:5],  method = "bray")

adonis2(dist_m ~ as.factor(data.m$Number) + data.m$Station, na.action=na.exclude)
adonis2(dist_sh ~ as.factor(data.sh$Number) + data.sh$Station, na.action=na.exclude)

## NMDS on full CWM data ----

nmds_cwm <- metaMDS(comm = a, k = 2,
                    distance = "bray",
                    trymax = 100) 

data.cwm = as.data.frame(scores(nmds_cwm)$sites)

data.cwm$Sample = b$Sample_ID#
data.cwm$Number = b$Number
data.cwm$Code = b$Code
data.cwm$Station =b$Station
data.cwm$Site = b$Site#


species.cwm = as.data.frame(scores(nmds_cwm)$species) # Extracting Species
species.cwm$species <- rownames(species.cwm)

#### Creating Mean NMDS Scores for each station ----
centr <- data.cwm %>%
  dplyr::group_by(Number, Code) %>%
  dplyr::summarize(nmds1= mean(NMDS1),
                   nmds2 = mean(NMDS2)) 

#### NMDS Plot (Appendix 10) ----

ggplot() +
  
  geom_text(data = species.cwm, aes(x = NMDS1, y = NMDS2, label = species),
            alpha = 0.9, size = 3, fontface="bold")+
  
  stat_conf_ellipse(data = data.cwm, aes(x=NMDS1, y=NMDS2, color=data.cwm$Code),
                    level=0.95)+
  
  geom_point(data = data.cwm, aes(x = NMDS1, y = NMDS2, 
                                  fill = data.cwm$Code, shape=data.cwm$Code, 
                                  colour=data.cwm$Code), size = 1.5)+
  
  geom_point(data=centr, aes(x=nmds1, y=nmds2), shape=centr$Number)+
  
  scale_color_manual(values=c("#e41a1c", "#377eb8","#4daf4a", "#984ea3"),
                     labels=c("Control Downstream","Control Upstream",
                              "Shrub Downstream", "Shrub Upstream"))+
  scale_fill_manual(values=c("#e41a1c", "#377eb8","#4daf4a", "#984ea3"),
                    labels=c("Control Downstream","Control Upstream",
                             "Shrub Downstream", "Shrub Upstream"))+
  scale_shape_manual(values=c(15,19,17,18),
                     labels=c("Control Downstream","Control Upstream",
                              "Shrub Downstream", "Shrub Upstream"))+
  labs(colour="", shape="",fill="")+
  
  #expand_limits(x=-1.0:1.0, y=-1)+
  
  theme_bw()+
  
  theme(legend.position="none", 
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', colour="transparent"),
        legend.text=element_text(size=8, face="bold"), 
        axis.text=element_text(size=10, colour="black",face="bold", family="Arial"),
        axis.title = element_text(size=10, face="bold", family="Arial"),
        panel.border = element_rect(size=1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold", family="Arial"))+
  
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
