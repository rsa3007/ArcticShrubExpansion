# Script:  Shrub Estimation in leaflet ----

#  Robin Satter                          

## Required Packages ----


library(leaflet)
library(tidyverse)

## Preparing Data ----

Stations<-read.csv("Stations.csv", header=TRUE, sep=";")

# Radius of 20m (Area = 1257m2)

MapKomag20<-leaflet(Stations)%>%
  addTiles()%>%
  addMeasure(position="topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters")%>%  
  addCircles(lng= ~lon, 
             lat= ~lat, 
             weight=2, 
             radius=20, 
             color = yarrr::transparent(orig.col="chocolate1", 
                                        trans.val=0.3), 
             fillColor = yarrr::transparent(orig.col="lightskyblue1", 
                                            trans.val=0.4))%>%
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addMarkers(lng = ~lon,
             lat= ~lat,
             label = ~Sample_ID)%>%
  addMiniMap()

MapKomag20

# Radius of 40m (Area = 5027m2)

MapKomag40<-leaflet(Stations)%>%
  addTiles()%>%
  addMeasure(position="topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters")%>%  
  addCircles(lng= ~lon, 
             lat= ~lat, 
             weight=2, 
             radius=40, 
             color = yarrr::transparent(orig.col="chocolate1", 
                                        trans.val=0.3), 
             fillColor = yarrr::transparent(orig.col="lightskyblue1", 
                                            trans.val=0.4))%>%
  addProviderTiles(providers$Esri.WorldImagery)%>%
  addMarkers(lng = ~lon,
             lat= ~lat,
             label = ~Sample_ID)%>%
  addMiniMap()

MapKomag40
