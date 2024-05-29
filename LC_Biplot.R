
# Script:  Stable Isotope Analyis: Biplot ----

#  Robin Satter                          

## Required packages ----

library(tidyverse)
library(readxl)
library(yarrr)
library(patchwork)
library(plyr)

## Preparing Data ----

LipCor <-read_xlsx("SI_LipidCorrected.xlsx")

### Subset Tall Shrub Streams ----
LC <- LipCor %>% filter(Site=="S")

# Calculating means and SD of resources for plot
LC_W <- LC %>% filter(Name == "Willow")
LC_W <- ddply (LC_W, c("Name"),summarise,
              count = length (Name),
              mC = mean(d13C_lip), sdC = sd(d13C_lip),
              mN = mean(d15N), sdN = sd(d15N))

LC_B <- LC %>% filter(Name == "Birch")
LC_B <- ddply (LC_B, c("Name"),summarise,
               count = length (Name),
               mC = mean(d13C_lip), sdC = sd(d13C_lip),
               mN = mean(d15N), sdN = sd(d15N))

LC_M <- LC %>% filter(Name == "Moss")
LC_M <- ddply (LC_M, c("Name"),summarise,
               count = length (Name),
               mC = mean(d13C_lip), sdC = sd(d13C_lip),
               mN = mean(d15N), sdN = sd(d15N))

LC_G <- LC %>% filter(Name == "Grass")
LC_G <- ddply (LC_G, c("Name"),summarise,
               count = length (Name),
               mC = mean(d13C_lip), sdC = sd(d13C_lip),
               mN = mean(d15N), sdN = sd(d15N))

LC_F <- LC %>% filter(Name == "Filamentous")
LC_F <- ddply (LC_F, c("Name"),summarise,
               count = length (Name),
               mC = mean(d13C_lip), sdC = sd(d13C_lip),
               mN = mean(d15N), sdN = sd(d15N))

LC_P <- LC %>% filter(Name=="Periphyton")
LC_P <- LC_P %>% filter(N2_ampl>=0.2)
LC_P <- ddply (LC_P, c("Name"),summarise,
               count = length (Name),
               mC = mean(d13C_lip), sdC = sd(d13C_lip),
               mN = mean(d15N), sdN = sd(d15N))

LC_I <- LC %>% filter(Group == "A" & Name != "Reindeer")
LC_I <- ddply (LC_I, c("Name", "Station"),summarise,
               count = length (Name),
               mC = mean(d13C_lip), sdC = sd(d13C_lip),
               mN = mean(d15N), sdN = sd(d15N))
LC_P$Name <- factor(LC_P$Name, levels =c ("Periphyton"))

### Subset Meadow Streams ----
LC2 <- LipCor %>% filter(Site=="C")

# Calculating means and SD of resources for plot
LC2_W <- LC2 %>% filter(Name == "Willow")
LC2_W <- ddply (LC2_W, c("Name"),summarise,
                count = length (Name),
                mC = mean(d13C_lip), sdC = sd(d13C_lip),
                mN = mean(d15N), sdN = sd(d15N))

LC2_B <- LC2 %>% filter(Name == "Birch")
LC2_B <- ddply (LC2_B, c("Name"),summarise,
                count = length (Name),
                mC = mean(d13C_lip), sdC = sd(d13C_lip),
                mN = mean(d15N), sdN = sd(d15N))

LC2_M <- LC2 %>% filter(Name == "Moss")
LC2_M <- ddply (LC2_M, c("Name"),summarise,
                count = length (Name),
                mC = mean(d13C_lip), sdC = sd(d13C_lip),
                mN = mean(d15N), sdN = sd(d15N))

LC2_G <- LC2 %>% filter(Name == "Grass")
LC2_G <- ddply (LC2_G, c("Name"),summarise,
                count = length (Name),
                mC = mean(d13C_lip), sdC = sd(d13C_lip),
                mN = mean(d15N), sdN = sd(d15N))

LC2_P <- LC2 %>% filter(Name=="Periphyton")
LC2_P <- LC2_P %>% filter(N2_ampl>=0.2)
LC2_P <- ddply (LC2_P, c("Name"),summarise,
                count = length (Name),
                mC = mean(d13C_lip), sdC = sd(d13C_lip),
                mN = mean(d15N), sdN = sd(d15N))

LC2_I <- LC2 %>% filter(Group == "A" & Name != "Reindeer")
LC2_I <- ddply (LC2_I, c("Name", "Station"),summarise,
                count = length (Name),
                mC = mean(d13C_lip), sdC = sd(d13C_lip),
                mN = mean(d15N), sdN = sd(d15N))


## Biplot C13 & N15 ----

# Meadow Streams
LC2_P$Name <- factor(LC2_P$Name, levels =c ("Periphyton"))
LC2_I$Name <- factor(LC2_I$Name, levels = c("Halesus","Nemoura","Limnephilidae","Simuliidae","Ameletus","Baetis", "Arcynopteryx","Dicranota"))
cols.a <- c("Arcynopteryx" = "#a65628", "Ameletus" = "#ff7f00", "Baetis" = "#ffff33", "Dicranota" = "#f781bf", "Halesus" = "#e41a1c",
            "Limnephilidae" = "#4daf4a", "Nemoura" = "#377eb8","Simuliidae" = "#984ea3")


shapes.a <- c("D"= 22, "U" = 21)#, "CD" = 22, "CU"= 21)
A <- ggplot()+
  
  geom_errorbar(data=LC2_B,aes(x=mC,ymax = mN + sdN, 
                               ymin = mN - sdN), alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC2_B,aes(y=mN,xmax = mC + sdC, 
                                xmin = mC - sdC),alpha=0.2, height=0.1)+
  
  geom_errorbar(data=LC2_G,aes(x=mC,ymax = mN + sdN, 
                               ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC2_G,aes(y=mN,xmax = mC + sdC, 
                                xmin = mC - sdC), alpha=0.2, height=0.1)+
  
  geom_errorbar(data=LC2_M,aes(x=mC,ymax = mN + sdN, 
                               ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC2_M,aes(y=mN,xmax = mC + sdC, 
                                xmin = mC - sdC), alpha=0.2, height=0.1)+
  
  
  geom_errorbar(data=LC2_I,aes(x=mC,ymax = mN + sdN, 
                               ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC2_I,aes(y=mN,xmax = mC + sdC, 
                                xmin = mC - sdC), alpha=0.2, height=0.1)+
  
  geom_errorbar(data=LC2_P,aes(x=mC,ymax = mN + sdN, 
                               ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC2_P,aes(y=mN,xmax = mC + sdC, 
                                xmin = mC - sdC), alpha=0.2, height=0.1)+
  
  geom_point(data = LC2_I, aes(x=mC, y=mN, shape=Station,fill=Name), alpha=1, size=4)+
  geom_text(data = LC2_P, aes(x=mC, y=mN, label= "Biofilm"),color="black",size=4)+
  geom_text(data=LC2_M, aes(x=mC, y=mN, label = "Moss"),color="black",size=4, vjust=0.4)+
  geom_text(data=LC2_G, aes(x=mC, y=mN, label = "Grass"),color="black",size=4, hjust=0.6,vjust=0.4)+
  geom_text(data=LC2_B, aes(x=mC, y=mN, label = "Dwarf birch"),color="black",size=4 ,vjust=0.4)+
  
  
  scale_fill_manual(values = cols.a, drop=FALSE)+
  scale_color_manual(values=c("Periphyton"="black"), drop=FALSE)+
  scale_shape_manual(values = shapes.a, labels=c("Downstream", 
                                               "Upstream"
  ), drop = FALSE)+
  
  guides(shape=guide_legend(order=1,override.aes=list(fill="black", size=3)),
         col=guide_legend(order=3,override.aes=list(shape=21, fill="gray50",size=4)),
         fill=guide_legend(order=2,override.aes=list(shape=21, size=4)))+
  labs(color= "Stream Producers",fill="Invertebrates", shape="",
       y=expression(bold(paste(delta^{15}, "N(???)"))),
       x=expression(bold(paste(delta^{13}, "C(???)"))))+
  theme_bw()+
  ggtitle("Meadow")+
  theme(legend.position=c(0.13,0.49), 
        plot.title= element_text(hjust=0.04,vjust=-9),
        legend.background = element_rect(fill='transparent'),
        legend.box = "vertical",
        legend.box.background = element_rect(fill='transparent',
                                             colour="transparent", size=2),
        legend.key=element_blank(),
        legend.text=element_text(size=9),
        legend.text.align = 0,
        legend.key.size=unit(0.4,'cm'),
        axis.text=element_text(size=10, colour="black"),
        axis.title = element_text(size=12, face="bold"),
        panel.border = element_rect(size=1.2),
        plot.tag.position = c(0.04, 0.99),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()
  )+
  coord_fixed(xlim = c(-45,-22), ylim = c(-7,7) )

# Tall Shrub Streams
LC_I$Name <- factor(LC_I$Name, levels = c("Halesus","Nemoura","Limnephilidae","Simuliidae","Ameletus","Baetis", "Arcynopteryx","Dicranota"))
cols.b <- c("Arcynopteryx" = "#a65628", "Ameletus" = "#ff7f00", "Baetis" = "#ffff33", "Dicranota" = "#f781bf", "Halesus" = "#e41a1c",
          "Limnephilidae" = "#4daf4a", "Nemoura" = "#377eb8","Simuliidae" = "#984ea3")

shapes.b <- c("D"= 24, "U" = 23)#, "CD" = 22, "CU"= 21)


B <- ggplot()+
  geom_point(data=LC_F, aes(x=mC, y=mN), shape=3, alpha=0.3)+
  geom_errorbar(data=LC_F,aes(x=mC,ymax = mN + sdN, 
                              ymin = mN - sdN), alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC_F,aes(y=mN,xmax = mC + sdC, 
                               xmin = mC - sdC), alpha=0.2, height=0.1)+
  
  geom_errorbar(data=LC_W,aes(x=mC,ymax = mN + sdN, 
                              ymin = mN - sdN), alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC_W,aes(y=mN,xmax = mC + sdC, 
                               xmin = mC - sdC), alpha=0.2, height=0.1)+
  
  geom_errorbar(data=LC_B,aes(x=mC,ymax = mN + sdN, 
                              ymin = mN - sdN), alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC_B,aes(y=mN,xmax = mC + sdC, 
                              xmin = mC - sdC),alpha=0.2, height=0.1)+
  
  geom_errorbar(data=LC_G,aes(x=mC,ymax = mN + sdN, 
                              ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC_G,aes(y=mN,xmax = mC + sdC, 
                               xmin = mC - sdC), alpha=0.2, height=0.1)+

  geom_errorbar(data=LC_M,aes(x=mC,ymax = mN + sdN, 
                              ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC_M,aes(y=mN,xmax = mC + sdC, 
                               xmin = mC - sdC), alpha=0.2, height=0.1)+
   
  
  geom_errorbar(data=LC_I,aes(x=mC,ymax = mN + sdN, 
                              ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC_I,aes(y=mN,xmax = mC + sdC, 
                               xmin = mC - sdC), alpha=0.2, height=0.1)+
  
  geom_errorbar(data=LC_P,aes(x=mC,ymax = mN + sdN, 
                              ymin = mN - sdN),alpha=0.2, width=0.1)+
  geom_errorbarh(data=LC_P,aes(y=mN,xmax = mC + sdC, 
                               xmin = mC - sdC), alpha=0.2, height=0.1)+

  geom_point(data = LC_I, aes(x=mC, y=mN, shape=Station,fill=Name), alpha=1, size=4)+
  geom_text(data = LC_P, aes(x=mC, y=mN, label = "Biofilm"), color="black",size=4)+
  geom_text(data=LC_F, aes(x=mC, y=mN, label = "Filamentous algae"),color="black",size=4, vjust=1.3, hjust=0.4)+
  geom_text(data=LC_M, aes(x=mC, y=mN, label = "Moss"),color="black",size=4, vjust=0.4)+
  geom_text(data=LC_G, aes(x=mC, y=mN, label = "Grass"),color="black",size=4,hjust=0.0, vjust=0.1)+
  geom_text(data=LC_B, aes(x=mC, y=mN, label = "Dwarf birch"),color="black",size=4 ,vjust=0.4)+
  geom_text(data=LC_W, aes(x=mC, y=mN, label = "Willow"),color="black",size=4, vjust=0.4)+
  
  scale_fill_manual(values = cols.b, drop=FALSE)+
  scale_color_manual(values=c("Periphyton"="black"), drop=FALSE)+
  scale_shape_manual(values = shapes.b, labels=c("Downstream", 
                                               "Upstream"
  ), drop = FALSE)+
  
  guides(shape=guide_legend(order=1,override.aes=list(fill="black", size=3)),
         col=guide_legend(order=3,override.aes=list(shape=21, fill="gray50",size=4)),
          fill=guide_legend(order=2,override.aes=list(shape=21, size=4)))+
  labs(color= "Stream Producers",fill="Invertebrates", shape="",
       y=expression(bold(paste(delta^{15}, "N(???)"))),
       x=expression(bold(paste(delta^{13}, "C(???)"))))+
  theme_bw()+
  ggtitle("Tall Shrub")+
  theme(legend.position=c(0.13,0.49), 
        plot.title= element_text(hjust=0.04,vjust=-9),
        legend.background = element_rect(fill='transparent'),
        legend.box = "vertical",
        legend.box.background = element_rect(fill='transparent',
                                             colour="transparent", size=2),
        legend.key=element_blank(),
        legend.text=element_text(size=9),
        legend.text.align = 0,
        legend.key.size=unit(0.4,'cm'),
        axis.text=element_text(size=10, colour="black"),
        axis.title = element_text(size=12, face="bold"),
        panel.border = element_rect(size=1.2),
        plot.tag.position = c(0.04, 0.99),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold"))+
  coord_fixed(xlim = c(-45,-22), ylim = c(-7,7))

(A/B)+
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'A')
theme(plot.tag = element_text(size=13))


