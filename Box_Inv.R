
# Script:  Boxplots - Biodiversity Metrics ----

#  Robin Satter                          

## Required Packages ----

library(tidyverse)
library(readxl)
library(patchwork)


## Preparing Data ----

# Invertebrate metrics data frame (a)
a<- read_xlsx("Inv_Comp.xlsx")

# Dataset with functional dispersion (fdisp)
fdisp <- read_xlsx("fddata.xlsx")


## Boxplots ----


### Abundance ----
ab <- a%>%
  filter(a$Sample_ID!="KO_1_S_D")%>%
  ggplot(aes(x=Code, y=Abundance, fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  xlab("  Meadow     Tall Shrub")+
  labs(fill="", y="")+  
  ggtitle("Abundance")+

  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))

### Richness ----
r <- a%>%
  filter(a$Sample_ID!="KO_1_S_D")%>%
  ggplot(aes(x=Code, y=N.species, fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  xlab("  Meadow     Tall Shrub")+
  labs(fill="", y="")+  
  ggtitle("Richness")+
  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))

### Diversity ----

d <- a%>%
  filter(a$Sample_ID!="KO_1_S_D")%>%
  ggplot(aes(x=Code, y=H, fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  xlab("  Meadow     Tall Shrub")+
  labs(fill="", y="")+  
  ggtitle("Shannon's Diversity")+

  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0.5), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))

### Evenness ----
e <-a%>%
  filter(a$Sample_ID!="KO_1_S_D")%>%
  ggplot(aes(x=Code, y=Eveness, fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  xlab("  Meadow     Tall Shrub")+
  labs(fill="", y="")+  
  ggtitle("Evenness")+
  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))

### Functional Dispersion ----

f <- ggplot(fdisp, aes(x=Code, y=FDis, fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  xlab("  Meadow     Tall Shrub")+
  labs(fill="", y="")+  
  ggtitle("Functional Dispersion")+
  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0.5,0,0,0.5), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))



## Patchwork ----

ab+r+d+e+f
