# Script:  Mixed Effect Models & Boxplots - Algal Biomass ----

#  Robin Satter                          

## Required Packages ----

library(readxl)
library(lmerTest)
library(tidyverse)
library(reshape2)
library(patchwork)

## Preparing Data ----

# Benthotorch dataframe (BT)
BT<-read_xlsx("BT.xlsx")

# Descriptive data of replicates (desc)
desc<-read_xlsx("Replicate_Descriptives.xlsx")


bt_l<-merge(desc,BT, by= "Sample_ID")

bt_l<-bt_l %>% distinct(Sample_ID,.keep_all = TRUE)

BT_Long <- melt(bt_l,
                id.vars = c("Sample_ID", "Site","Station","River", "Number"),
                measure.vars = c("Algae_1","Algae_2", "Algae_3", "Algae_4", "Algae_5", "Cyano_1", "Cyano_2", "Cyano_3"
                                 ,"Cyano_4","Cyano_5","Diatom_1", "Diatom_2", "Diatom_3", "Diatom_4", "Diatom_5"),
                variable.name = "PP", value.name = "Value"
)

names <- c(rep("Algae",100),rep("Cyano",100),rep("Diatom",100))

BT_Long$Prod<-names

BT_Long <- within(BT_Long, Site <- relevel(as.factor(Site), ref = "S"))
BT_Long <- within(BT_Long, Station <- relevel(as.factor(Station), ref="U"))


## Linear Mixed Effect Models ----

### Algae ----
algae <- filter(BT_Long, Prod == "Algae")

# LMER with interaction
algae.mod.i <- lmer(Value ~ Site * Station + (1 |factor(algae$Number)), data=algae) # Clearly no interaction

# No interaction = exclude from LMER
algae.mod <- lmer(Value ~ Site + Station + (1 |factor(algae$Number)), data=algae) # Clearly no interaction


### Diatoms ----
diatom <- filter(BT_Long, Prod == "Diatom")

# LMER with interaction
diatom.mod.i <- lmer(Value ~ Site * Station + (1 |factor(diatom$Number)), data=diatom) # Possible interaction

# No interaction = exclude from LMER
diatom.mod <- lmer(log(Value+1) ~ Site + Station + (1 |factor(diatom$Number)), data=diatom) # Possible interaction


### Cyano ----
cyano <- filter(BT_Long, Prod == "Cyano")

# LMER with interaction
cyano.mod.i <- lmer(Value ~ Site * Station + (1 |factor(cyano$Number)), data=cyano) # Clearly no interaction

# No interaction = exclude from LMER
cyano.mod <- lmer(Value ~ Site + Station + (1 |factor(cyano$Number)), data=cyano)



## Boxplots ----

BT_Long$Code<- paste(BT_Long$Site, BT_Long$Station)

a <- BT_Long%>%
  filter(BT_Long$Prod!="Algae")%>%
  ggplot(aes(x=Code, y=Value, fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                  labels=c("Downstream","Upstream"))+
  xlab("  Meadow                   Tall Shrub")+
  ylab(expression("Biomass (" * mu ~ "g/"*"cm"^2*")"))+
  labs(fill="")+  
  ggtitle("Green Algae")+
  
  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))

b <- BT_Long%>%
  filter(BT_Long$Prod!="Cyano")%>%
  ggplot(aes(x=Code, y=Value, fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  xlab("  Meadow                   Tall Shrub")+
  ylab(expression("Biomass (" * mu ~ "g/"*"cm"^2*")"))+
  labs(fill="")+  
  ggtitle("Cyanobacteria")+
  
  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0.3,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))

c <- BT_Long%>%
  filter(BT_Long$Prod!="Diatom")%>%
  ggplot(aes(x=Code, y=log(Value+1), fill=Station))+
  geom_boxplot(width=0.5, alpha=0.6)+
  stat_summary(geom="point",fun="mean", fill="black", shape=22)+
  scale_fill_manual(values=c("#e41a1c", "#377eb8"),
                    labels=c("Downstream","Upstream"))+
  xlab("  Meadow                   Tall Shrub")+
  ylab(expression("Biomass (" * mu ~ "g/"*"cm"^2*")"))+
  labs(fill="")+  
  ggtitle("Diatoms")+
  
  
  theme_bw()+
  
  theme(axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        legend.position="none", 
        axis.line= element_line(size=0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0.58,0), "cm"),
        plot.tag = element_text(size = rel(1)),
        plot.title = element_text(size=10, face="bold"))

a/b|c/guide_area()

