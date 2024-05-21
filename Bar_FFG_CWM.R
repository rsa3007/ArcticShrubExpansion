
# Script:  Functional Groups Stacked Barplot ----

#  Robin Satter                          

## Required Packages ----

library(tidyverse)
library(reshape2)
library(readxl)


## Preparing Data ----

# Community Weighted Means (cwm)
cwm <- read_xlsx("cwm_nmds.xlsx")

# Functional disperion dataframe (c)
c <- read_xlsx("fddata.xlsx")

cwm_long <- cbind (cwm, c)

cwm_long <- melt(cwm_long,
                 id.vars = c(1,6,7,8,9,10,12),
                 measure.vars = c("SHR","COL","GRA","PRE" ),
                 variable.name = "Feeding", value.name = "CWM")


order1 <- c("CU", "CD", "SU", "SD")
order2 <- c("PRE", "GRA","COL","SHR")

cwmSUM <- cwm_long %>%
  dplyr::group_by(Feeding, Code) %>%
  dplyr::summarise(CWM=mean(CWM))


## Stacked Barplot of Proportional FFG ----

ggplot(cwmSUM) + 
  
  geom_bar(aes(fill=Feeding, y=CWM, x=factor(Code, order1)),colour="black",
           stat="identity",position="fill", width=0.5, alpha=0.6)+
  
  scale_fill_manual(values=c("#984ea3","#377eb8","#4daf4a", "#e41a1c"),
                    breaks=c("SHR", "COL","GRA","PRE"),
                    labels=c("Shredder","Collector",
                             "Grazer", "Predator"))+
  
  scale_x_discrete(labels=c("CU"="Meadow\nUpstream","CD"="Meadow\nDownstream",
                            "SU"= "Tall Shrub\nUpstream","SD"="Tall Shrub\nDownstream"))+
  
  scale_y_continuous(breaks=c(0, 0.25, 0.50, 0.75, 1),
                     labels=c(0,25,50,75,100))+
  
  theme(legend.position="bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent', 
                                             colour="transparent"),
        legend.text=element_text(size=10), 
        axis.text.y=element_text(angle=0),
        axis.text=element_text(size=10, 
                               colour="black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line= element_line(size=0.5),
        panel.background = element_blank(),
        plot.margin = unit(c(0,0,0.5,0), "cm"),
        plot.tag = element_text(size = rel(1)),
        axis.title.x=element_text(margin=margin(5,6,0,0), face="bold", size=10, vjust=0.001))+
  
  coord_flip()+
  
  labs(y="Percentage of Functional Trait Weighted Means", x="")
