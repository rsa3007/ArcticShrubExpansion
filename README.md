This repository contains all the datasets (.xlsx & .csv) and R scripts that are necessary to reproduce the figures and analysis used in my thesis. 

23 datasets can be found in the repository:

- [Replicate_Descriptives.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485227/Replicate_Descriptives.xlsx)
  The basic classification of every replicate into sample id's, stream number, site (tall shrub **S**, meadow **C**) station (upstream **U**/downstream **D**), replicate (A,B, or C) and a code that combines site and station information.
- [Stations.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485308/Stations.csv)
  Latitude, longitude measurements from a Garmin GPS for every location sampled.
- [BT.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485457/BT.xlsx)
  Measurements of Standing Biomass of Green Algae, Diatoms and Cyanobacteria. Five measurements were taken per location.
- [Inv_Comp.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485280/Inv_Comp.xlsx)
  The data on invertebrate taxa abundance that was found in each replicate. This also contains the calculations of total abundance, species richness, Shannon's diversity & evenness in the same dataframe.
- [FFG_fd.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485337/FFG_fd.xlsx)
  The classification of invertebrate taxa into the functional groups: Shredders, Collectors, Grazers, and Predators.
- [Abundance_Matrix_fd.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485346/Abundance_Matrix_fd.xlsx)
  A matrix of species abundances, derived from the Inv_Comp.xlsx.
- [fddata.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485369/fddata.xlsx)
  The functional dispersion index calculated per replicate.
- [cwm_nmds.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485395/cwm_nmds.xlsx)
  Community Weighted Means calculated per replicate.
- [ShrubCover.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485479/ShrubCover.xlsx)
  Calculated cover of Shrubs in a radius of 20 meters around the sampling location.
- [inv.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485502/inv.xlsx)
  Lipid corrected isotope values used for making boxplots of invertebrates.
- [res.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485513/res.xlsx)
  Lipid corrected isotope values used for making boxplots for all the sampled resources.
- [SI_LipidCorrected.xlsx](https://github.com/rsa3007/ArcticShrubExpansion/files/15485520/SI_LipidCorrected.xlsx)
  Complete dataframe of lipid corrected stable isotope data from invertebrates and resources.
- [21478704.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485537/21478704.csv), [21478707.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485538/21478707.csv),
[21478696.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485548/21478696.csv), [21478686.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485546/21478686.csv), 
[20834715.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485545/20834715.csv), 
[20834714.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485544/20834714.csv), 
[20834709.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485543/20834709.csv), 
[20834703.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485542/20834703.csv), 
[20834698.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485541/20834698.csv), 
[21478714.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485540/21478714.csv), 
[21478708.csv](https://github.com/rsa3007/ArcticShrubExpansion/files/15485539/21478708.csv)
HOBO Temperature logger data from 11 different locations in the river.

11 R scripts can be found in the repository:

- **FunDiv_CWM**: Calculations of Functional Dispersion & Community Weighted Means.
- **Box_Inv**: Boxplot five metrics (abundance, richness, diversity, evenness, and functional dispersion)
- **LMER_AoV_Inv**: Linear mixed effect models and ANOVAs of the five metrics above 
- **Bar_FFG_CWM**: Barplot of Functional Feeding Groups Community Weighted Means.
- **Shrub_Estimation**: Leaflet map in which the shrub cover was estimated.
- **Spec_Comp**: NMDS & Permanova on Species Composition.
- **Func_Comp**: NMDS & Permanova on Functional Composition.
- **Box_Sta_Iso**: Boxplots of Invertebrate and Resource Isotope values.
- **LC_Biplot**: Biplot C13 * N15 of Invertebrates and Resources. 
- **HOBOTemp**: Temperature plot of 11 HOBO temperature loggers.
- **Lmer_Box_Alg**: Linear mixed effect models of algal benthic standing biomass.









  



