
# Script:  Stable Isotope Analysis: Barplots ----

#  Robin Satter                          

## Required packages ----

library(tidyverse)
library(readxl)
library(plyr)

## Preparing Data ----

# Invertebrates Isotopic Data
inv<-read_xlsx("inv.xlsx")

# Resources Isotopic Data
res<-read_xlsx("res.xlsx")


## Invertebrate Barplot C13 & N15 (Figure 9) ----

inv$Name <- factor(inv$Name, levels = c("Halesus","Nemoura","Limnephilidae","Simuliidae","Ameletus","Baetis", "Arcynopteryx","Dicranota"))
cols <- c("Arcynopteryx" = "#a65628", "Ameletus" = "#ff7f00", "Baetis" = "#ffff33", "Dicranota" = "#f781bf", "Halesus" = "#e41a1c",
          "Limnephilidae" = "#4daf4a", "Nemoura" = "#377eb8","Simuliidae" = "#984ea3")

inv$Code <- factor(inv$Code, levels=c("CU", "CD", "SU", "SD"))
inv$Code <- factor(inv$Code, labels=c("Meadow\nUpstream","Meadow\nDownstream","Tall Shrub\nUpstream","Tall Shrub\nDownstream"))


A<-ggplot(inv, aes(x=Name, group=Name, y=d13C_lip, fill=Name))+
  geom_boxplot(alpha=0.7)+
  stat_summary(geom="point",fun="mean", color="black",fill="black", shape=22)+
  scale_fill_manual(values=cols)+
  facet_grid(. ~ Code)+
  theme_bw()+
  labs(fill="", x="",y=expression(bold(paste(delta^{13}, "C(???)"))))+
  theme_bw()+
  theme(strip.text = element_text(face="bold", size=9,lineheight=1),
        legend.position="none",
        legend.text=element_text(size=10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour="black", size = 10),
        strip.background = element_rect(fill="lightblue"),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

B<-ggplot(inv, aes(x=Name, group=Name, y=d15N, fill=Name))+
  geom_boxplot(alpha=0.7)+
  stat_summary(geom="point",fun="mean", color="black",fill="black", shape=22)+
  scale_fill_manual(values=cols, expand = expansion(mult = c(0, 0.1)))+
  facet_grid(. ~ Code)+
  theme_bw()+
  labs(fill="", x="",y=expression(bold(paste(delta^{15}, "N(???)"))))+
  theme_bw()+
  theme(strip.text = element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=10),
        axis.text.x = element_text(angle=45 , hjust = 1,colour="black", size=10),
        axis.text.y = element_text(colour="black", size = 10),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

(A/B)&
  plot_layout()

## Resources  Barplot C13 & N15 (Figure 10) ----

res$Name <- factor(res$Name, levels = c("Filamentous","Moss", "Birch","Willow","Grass","Periphyton"))
res$Name <- factor(res$Name, labels = c("Filamentous algae","Moss", "Dwarf birch","Willow","Grass","Biofilm"))
res_col <- c("Filamentous algae"= "#e41a1c","Moss"="#377eb8", "Dwarf birch"="#4daf4a","Willow"="#984ea3","Grass"="#ff7f00","Biofilm"= "#ffff33")
          

res$Code <- factor(res$Code, levels=c("CU", "CD", "SU", "SD"))
res$Code <- factor(res$Code, labels=c("Meadow\nUpstream","Meadow\nDownstream","Tall Shrub\nUpstream","Tall Shrub\nDownstream"))

C<-ggplot(res, aes(x=Name, group=Name, y=d13C_lip, fill=Name))+
  geom_boxplot(alpha=0.7)+
  #  geom_point(position="jitter")+
  stat_summary(geom="point",fun="mean", color="black",fill="black", shape=22)+
  scale_fill_manual(values=res_col)+
  facet_grid(. ~ Code, scales = "free")+
  theme_bw()+
  labs(fill="", x="",y=expression(bold(paste(delta^{13}, "C(???)"))))+
  theme_bw()+
  theme(strip.text = element_text(face="bold", size=9,lineheight=1),
        legend.position="none",
        legend.text=element_text(size=10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour="black", size = 10),
        strip.background = element_rect(fill="lightblue"),
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

D<-ggplot(res, aes(x=Name, group=Name, y=d15N, fill=Name))+
  geom_boxplot(alpha=0.7)+
  #  geom_point(position="jitter")+
  stat_summary(geom="point",fun="mean", color="black",fill="black", shape=22)+
  scale_fill_manual(values=res_col)+
  facet_grid(. ~ Code, scale="free")+
  theme_bw()+
  labs(fill="", x="",y=expression(bold(paste(delta^{15}, "N(???)"))))+
  theme_bw()+
  theme(strip.text = element_blank(),
        legend.position="bottom",
        legend.text=element_text(size=10),
        axis.text.x = element_text(angle=45 , hjust = 1,colour="black", size=10),
        axis.text.y = element_text(colour="black", size = 10),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

(C/D)&
  plot_layout()
