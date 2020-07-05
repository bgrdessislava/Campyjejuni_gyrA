setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

#Trying to do k-means clustering but not entirely sure what I am trying to do on this one. Maybe it does not matter...

library(dplyr)
library(VIM)
library(ggfortify)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(FactoMineR)
library(factoextra)


caecal_raw <- read.csv('/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/caecal_coreMLST.csv')

aggr(caecal_raw)
summary(caecal_raw)
results <- kmeans(caecal_raw,3)


mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)],center = TRUE,scale. = TRUE)

summary(mtcars.pca)

