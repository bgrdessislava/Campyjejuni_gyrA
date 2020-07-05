library(FactoMineR)
library(gtools)
library(ggplot2)

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")

#This document tries to do MCA but it says that the file is too big and cannot actually do the analysis
caecal_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/caecal_coreMLST.csv")
carcass_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/carcass_coreMLST.csv")
food_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/food_coreMLST.csv")
humanfeces_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/Newcastleyear1dd_coreMLST.csv")

foodchain_raw <- smartbind(caecal_raw,carcass_raw,food_raw,humanfeces_raw)

gene_data = grepl("CAMP",names(foodchain_raw))
other_useful = names(foodchain_raw) %in% c("species","source_tab","source","cgST..C..jejuni...C..coli.cgMLST.v1.0.")

new_foodchain = foodchain_raw[,gene_data | other_useful]

######################################################################################################

#Checking if this will work = trying to only use the 1:100 
new_foodchain = new_foodchain[,1:1348]

#Change the list to a factor
new_foodchain=sapply(new_foodchain, factor)

#Apply the MCA
#Limited reached and cannot do this
#mca1testfoodchain = MCA(new_foodchain)

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
  geom_vline(xintercept = 0,colour = "gray70") + theme(legend.position = "none") +
  geom_text(aes(colour = Variable)) 
  #ggtitle("MCA plot of variables using R package FactoMineR")

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
