
# Script:  Functional Diversity ----

#  Robin Satter                         

## Required Packages ----

library(readxl)
library(FD)
library(tidyverse)
library(writexl)
library(lmerTest)
library(reshape2)


## Preparing Data ----

#   Functional trait matrix (ffg)
ffg <- read_xlsx("FFG_fd.xlsx")

ffg <- ffg[order(ffg$species),]
ffg <- column_to_rownames(ffg, var = "species")

#   Species abundance matrix (a)
a <- read_xlsx("Abundance_Matrix_fd.xlsx")

a <- a %>% dplyr::select(Replicate_ID, sort(tidyselect::peek_vars()))
b <- a %>% dplyr::select (c(1))
a <- a %>% dplyr::select(-c(1))

#   Descriptive data of replicates (desc)
desc <- read_xlsx("Replicate_Descriptives.xlsx")


## Functional Diversity Metrics ----

# Check if the rownames in ffg and colnames in the abundance matrix are identical
identical(rownames(ffg[1:28,]), colnames(a[1:28]))

fd <- dbFD(ffg, a, stand.x = FALSE)

cwm <- fd$CWM # Extracting the Community Weighted Means

c <- b # New Data Frame for Output Variables dbFD

c <- merge(c, desc, by= "Replicate_ID")

c$FDis <- fd$FDis  # Functional Dispersion


## Export ----

write_xlsx(cwm, "cwm_nmds.xlsx")
write_xlsx(c,"fddata.xlsx")



