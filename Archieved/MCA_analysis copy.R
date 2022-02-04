library(FactoMineR)
library(gtools)
library(ggplot2)
library(MASS)

###MCA analysis but it is too big for it to run on my PC
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")
#data(tea)

#This document will create a document that let's 4 foodchain coreMLST data 
caecal_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/clustering/caecal_coreMLST.csv")
carcass_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/clustering/carcass_coreMLST.csv")
food_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/clustering/food_coreMLST.csv")
humanfeces_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/clustering/Newcastleyear1dd_coreMLST.csv")

foodchain_raw <- smartbind(caecal_raw,carcass_raw,food_raw,humanfeces_raw)

gene_data = grepl("CAMP",names(foodchain_raw))
other_useful = names(foodchain_raw) %in% c("species","source_tab","source","cgST..C..jejuni...C..coli.cgMLST.v1.0.")

new_foodchain = foodchain_raw[,gene_data | other_useful]

######################################################################################################
#drop <-c("aliases","country","receive_data","received_date","penner",
#         "ENA_accession","private_project","comments","sender","curator",
#         "date_entered","datestamp","aminoglycoside_genotypes_1" , "Anon_patient_ID",
#         "erythromycin_phenotype","macrolide_genotypes_1", "macrolide_genotypes_2",
#         "tetracycline_genotypes_1","tetracycline_genotypes_2", 
#         "tetracycline_phenotype","isolation_date","age_yr","age_mth","disease")

#newfoodchain=foodchain_raw[,!names(foodchain_raw)%in%drop]
######################################################################################################

#Checking if this will work = trying to only use the 1:100 
new_foodchain = new_foodchain[,1:100]

#Change the list to a factor
new_foodchain=sapply(new_foodchain, factor)

#Apply the MCA
mca1testfoodchain = MCA(new_foodchain)

write.csv(mca1testfoodchain,file = 'mca1foodchain_100values')
# table of eigenvalues
mca1testfoodchain$eig

# Extract category list from the MCA result
# number of categories per variable
categories_testfoodchain = apply(mca1testfoodchain$call$X, 2,function(x)nlevels(as.factor(x)))
mca1_vars_df_foodchain = data.frame(mca1testfoodchain$var$coord, 
                                    Variable = rep(names(categories_testfoodchain), categories_testfoodchain))
mca1_obs_df_foodchain = data.frame(mca1testfoodchain$ind$coord)

# plot of variable categories
ggplot(data = mca1_vars_df_foodchain, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df_foodchain))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0,colour = "gray70") + 
  geom_text(aes(colour = Variable)) + theme(legend.position = "none") +
  ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca1_obs_df_foodchain, aes(x = Dim.1, y = Dim.2)) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_point(colour = "gray50", alpha = 0.7) + 
  geom_density2d(colour = "gray80") + 
  geom_text(data = mca1_vars_df_foodchain,aes(x = Dim.1, y = Dim.2, 
                                              label = rownames(mca1_vars_df_foodchain), colour = Variable)) + 
  ggtitle("MCA plot of variables using R package FactoMineR") + theme(legend.position = "none") +
  scale_colour_discrete(name = "Variable")

###################################################################################################

# default biplot in FactoMineR
plot(mca1testfoodchain)
theme

