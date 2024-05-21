
# Script:  Mixed Effect Models & Anova - Invertebrate Metrics ----

#  Robin Satter                          

## Required Packages ----

library(readxl)
library(lmerTest)
library(tidyverse)


## Preparing Data ----

# Invertebrate metrics dataframe (a)
a <-read_xlsx("Inv_Comp.xlsx")

a  <- a %>% filter(Number != "1") # Deselect stream number 1

# Set reference levels for lmer
a <- within(a, Site <- relevel(as.factor(Site), ref = "S")) 
a <- within(a, Station <- relevel(as.factor(Station), ref="U"))

# Deselect value 0 of H due to only 1 species present for lmer on H and Evenness
b <- a %>% filter(H !="0") 

# Set reference levels for lmer
b <- within(b, Site <- relevel(as.factor(Site), ref = "S"))
b <- within(b, Station <- relevel(as.factor(Station), ref="U"))

# Functional disperion dataframe (c)
c <- read_xlsx("fddata.xlsx")
d <- c[!(is.na(c$FDis)), ] # Remove NA values from the dataframe

# Set reference levels for lmer
d <- within(d, Site <- relevel(as.factor(Site), ref = "S"))
d <- within(d, Station <- relevel(as.factor(Station), ref="U"))


## Linear Mixed Effect Models ----

### Abundance ----

# LMER with interaction
abu.int <-lmer(log(Abundance+1) ~ Site * Station + (1 | factor(a$Number)), data=a)

# No interaction = exclude from LMER
abu <-lmer(log(Abundance+1) ~ Site + Station + (1 | factor(a$Number)), data=a)


### Richness ----

# LMER with interaction
r.int <- lmer(N.species ~ Site * Station + (1 |factor(a$Number)), data=a)

# No interaction = exclude from LMER
r <- lmer(N.species ~ Site + Station + (1 |factor(a$Number)), data=a)


### Diversity ----

# LMER with interaction
div.int <- lmer(H ~ Site * Station  + (1|factor(b$Number)), data=b)

# No interaction = exclude from LMER
div <- lmer(H ~ Site + Station + (1|factor(b$Number)), data=b)


### Evenness ----

# LMER with interaction
e.int <- lmer(Eveness ~ Station * Site  + (1|factor(b$Number)), data=b)

# No interaction = exclude from LMER
e <- lmer(Eveness ~ Station + Site  + (1| factor(b$Number)), data=b)


### Functional Dispersion ----

# LMER with interaction
f.int<- lmer(FDis ~ Site * Station + (1 |factor(d$Number)), data=d) 

# No interaction = exclude from LMER
f <- lmer(FDis ~ Site + Station + (1 |factor(d$Number)), data=d)


## Analyses of Variance ----

### Abundance ----

a_siteS <- a %>% filter(Site == "S")
a_stationD <- a %>% filter(Station == "D")

aaov1 <- lm(Abundance ~ Station, data = a_siteS)

aaov2 <- lm(Abundance ~ Site, data = a_stationD)


### Richness ----

raov1 <- lm(N.species ~ Station, data = a_siteS)

raov2 <- lm(N.species ~ Site, data = a_stationD)


### Diversity ----

b_siteS <- b %>% filter(Site == "S")
b_stationD <- b %>% filter(Station == "D")

daov1 <- lm(H ~ Station, data = b_siteS)

daov2 <- lm(H ~ Site, data = b_stationD)


### Evenness ----

eaov1 <- lm(Eveness ~ Station, data = b_siteS)

eaov2 <- lm(Eveness ~ Site, data = b_stationD)


### Functional Dispersion ----

f_siteS <- d %>% filter(Site == "S")
f_stationD <- d %>% filter(Station == "D")

faov1 <- lm(FDis ~ Station, data = f_siteS)

faov2 <- lm(FDis ~ Site, data = f_stationD)

