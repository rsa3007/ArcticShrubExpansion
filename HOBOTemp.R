# Script: HOBO Temperature Loggers ----

#  Robin Satter                          

## Required Packages ----

library(tidyverse)
library(dplyr)

## Preparing Data ----

D9<-read.csv("20834709.csv")
D9$col<-"KO9SD"
D16<-read.csv("21478707.csv")
D16$col<-"KO16SD"
D11<-read.csv("21478686.csv")
D11$col<-"KO11CD"
D5<-read.csv("21478714.csv")
D5$col<-"KO5CD"
D8<-read.csv("21478708.csv")
D8$col<-"KO8SD"
U16<-read.csv("20834714.csv")
U16$col<-"KO16SU"
U8<-read.csv("21478696.csv")
U8$col<-"KO8SU"
D4<-read.csv("20834703.csv")
D4$col<-"KO4SD"
U15<-read.csv("21478704.csv")
U15$col<-"KO15SU"
U9<-read.csv("20834715.csv")
U9$col<-"KO9SU"
D6<-read.csv("20834698.csv")
D6$col<-"KO6CD"

# Combine data of all loggers in one data frame
hobofull<-rbind(D11,D16,D4,D5,D6,D8,D9,U15,U16,U8,U9)

# Separate date from time
hobofull <- tidyr::separate(hobofull, 'Date.Time..CET.CEST.',
                                    into = c('longdate', 'time'),
                                    sep= ' ') 

# Create daily means for every logger 
hobofullmean <- hobofull %>%
  dplyr::group_by(longdate, col)%>%
  dplyr::summarise(meantemp = mean(Temp),
            meanlight=mean(Light))

# Select timeframe
plothobo<-hobofullmean[hobofullmean$longdate >= "07/07/2023" &
                         hobofullmean$longdate <= "09/14/2023", ]


plothobo$col <- factor(plothobo$col, levels = c("KO4SD", "KO5CD", "KO6CD", "KO8SD",
                                          "KO8SU" ,"KO9SD", "KO9SU","KO11CD",
                                          "KO15SU","KO16SD","KO16SU"))

cols <- c("KO4SD" = "#e41a1c", "KO5CD" = "#377eb8", "KO6CD" = "#4daf4a", "KO8SD" = "#984ea3", "KO8SU" = "#ff7f00",
          "KO9SD" = "#ffff33", "KO9SU" = "#a65628","KO11CD" = "#f781bf","KO15SU"="gray50","KO16SD"="black","KO16SU"="#66c2a5")

ggplot(data=plothobo,aes(x=as.Date(longdate, format= "%m / %d / %Y"), y=meantemp, color=col))+
  geom_line(size=1.2)+
  scale_color_manual(values=cols)+
  theme_bw()+
  ylab("Mean Daily Temperature (\u00B0C)")+
  xlab("Date")+
  labs(colour="Location")+
  guides(color=guide_legend(ncol=2))+
  theme(legend.position=c(0.25,0.85), 
        plot.title= element_text(hjust=0.05,vjust=-11),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent',
                                             colour="transparent", size=2),
        legend.text=element_text(size=10),
        legend.text.align = 0,
        legend.key.size=unit(0.4,'cm'),
        axis.text=element_text(size=10, colour="black"),
        axis.title = element_text(size=12, face="bold"),
        panel.border = element_rect(size=1.2),
        plot.tag.position = c(0.04, 0.99),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.tag = element_text(size = rel(1), face="bold"))
