library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)

#This is a very cool work looking at potentially which factors is most important in terms of which factors might be strongly related to resistance

#Creating tables that summarises what kind of values are found for each factor
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
long_86_position<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list_86_position.csv")

#######Year Resistance Starts HERE############################################
#makes it into a table format
year_resistance <-table(long_86_position$year,long_86_position$base)
#CHooses only I and T
year_resistance <- year_resistance[,c(2,4)]
#Choosing isolates above certain number
year_resistance = year_resistance[rowSums(year_resistance) > 100,]
#Makes it into a dataframe
year_resistance = data.frame(year_resistance)
#Functions to check if an object is a data frame, or coerce it if possible not needed 
#year_resistance = as.data.frame.matrix(year_resistance) 

#dcast leads long to wide
year_resistance = dcast(year_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(year_resistance) = year_resistance[,1]
#Gets rid of the first column
year_resistance[,1] = NULL


#3dmap that is interactive

#dataframe making
dfyear_resistance <- data.frame(year_resistance)

#normalising
dfyear_resistance_normalised = sweep(dfyear_resistance,1,rowSums(dfyear_resistance), FUN = '/')

#visualisng
dfyear_resistance3dmap<- d3heatmap(t(dfyear_resistance_normalised), color = "viridis", dendrogram = 'column',
                                legend = TRUE,
                                # Number of groups in rows
                                 k_col = 5 # Number of groups in columns
)
#running
dfyear_resistance3dmap


#######Source Resistance Starts HERE#############################################
#################################################################################
source_resistance <-table(long_86_position$source,long_86_position$base)
#CHooses only I and T
source_resistance <- source_resistance[,c(2,4)]

#Makes it into a dataframe
source_resistance = data.frame(source_resistance)

#dcast leads long to wide
source_resistance = dcast(source_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(source_resistance) = source_resistance[,1]
#Gets rid of the first column
source_resistance[,1] = NULL

#dfsource_resistance <- data.frame(source_resistance)

source_resistance_normalised = sweep(source_resistance,1,rowSums(source_resistance), FUN = '/')

#visualisng
source_resistance3dmap<- d3heatmap(t(source_resistance_normalised), color = "viridis", dendrogram = 'none',
                                 legend = TRUE # Number of groups in columns
)

source_resistance3dmap

#######ST_resistance Starts HERE#############################################
#################################################################################
ST_resistance <-table(long_86_position$ST,long_86_position$base)

#CHooses only I and T
ST_resistance <- ST_resistance[,c(2,4)]
ST_resistance = ST_resistance[rowSums(ST_resistance) > 100,]

#Makes it into a dataframe
ST_resistance = data.frame(ST_resistance)

#dcast leads long to wide
ST_resistance = dcast(ST_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(ST_resistance) = ST_resistance[,1]
#Gets rid of the first column
ST_resistance[,1] = NULL

#dfsource_resistance <- data.frame(ST_resistance)

ST_resistance_normalised = sweep(ST_resistance,1,rowSums(ST_resistance), FUN = '/')

#visualisng
ST_resistance3dmap<- d3heatmap(t(ST_resistance_normalised), color = "viridis", dendrogram = 'column',
                                 legend = TRUE,
                                 k_col = 5# Number of groups in columns
)

ST_resistance3dmap

#######Clonal Complex    Starts HERE#############################################
#################################################################################
clonalcomplex_resistance <-table(long_86_position$clonal_complex,long_86_position$base)

#CHooses only I and T
clonalcomplex_resistance <- clonalcomplex_resistance[,c(2,4)]
clonalcomplex_resistance = clonalcomplex_resistance[rowSums(clonalcomplex_resistance) > 100,]

#Makes it into a dataframe
clonalcomplex_resistance = data.frame(clonalcomplex_resistance)

#dcast leads long to wide
clonalcomplex_resistance = dcast(clonalcomplex_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(clonalcomplex_resistance) = clonalcomplex_resistance[,1]
#Gets rid of the first column
clonalcomplex_resistance[,1] = NULL

#dfsource_resistance <- data.frame(ST_resistance)

clonalcomplex_resistance_normalised = sweep(clonalcomplex_resistance,1,rowSums(clonalcomplex_resistance), FUN = '/')

#visualisng
clonalcomplex_resistance3dmap<- d3heatmap(t(clonalcomplex_resistance_normalised), color = "viridis", dendrogram = 'column',
                               legend = TRUE,
                               k_col = 5# Number of groups in columns
)

clonalcomplex_resistance3dmap


#######coreMLST Starts HERE#############################################
#################################################################################
coreMLST_resistance <-table(long_86_position$cgMLST.coli_jejuni.,long_86_position$base)

#CHooses only I and T
coreMLST_resistance <- coreMLST_resistance[,c(2,4)]
coreMLST_resistance = coreMLST_resistance[rowSums(coreMLST_resistance) > 5,]

#Makes it into a dataframe
coreMLST_resistance = data.frame(coreMLST_resistance)

#dcast leads long to wide
coreMLST_resistance = dcast(coreMLST_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(coreMLST_resistance) = coreMLST_resistance[,1]
#Gets rid of the first column
coreMLST_resistance[,1] = NULL

coreMLST_resistance_normalised = sweep(coreMLST_resistance,1,rowSums(coreMLST_resistance), FUN = '/')

#visualisng
coreMLST_resistance3dmap<- d3heatmap(t(coreMLST_resistance_normalised), color = "viridis", dendrogram = 'column',
                                          legend = TRUE,
                                          k_col = 5# Number of groups in columns
)

coreMLST_resistance3dmap

#######rMLST Resistance Starts HERE#############################################
#################################################################################
rMLST_resistance <-table(long_86_position$rMLST,long_86_position$base)

#CHooses only I and T
rMLST_resistance <- rMLST_resistance [,c(2,4)]
rMLST_resistance  = rMLST_resistance [rowSums(rMLST_resistance) > 50,]

#Makes it into a dataframe
rMLST_resistance  = data.frame(rMLST_resistance)

#dcast leads long to wide
rMLST_resistance  = dcast(rMLST_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(rMLST_resistance) = rMLST_resistance [,1]
#Gets rid of the first column
rMLST_resistance[,1] = NULL

rMLST_resistance_normalised = sweep(rMLST_resistance,1,rowSums(rMLST_resistance), FUN = '/')

#visualisng
rMLST_resistance3dmap<- d3heatmap(t(rMLST_resistance_normalised), color = "viridis", dendrogram = 'column',
                                     legend = TRUE,
                                     k_col = 5# Number of groups in columns
)

rMLST_resistance3dmap


#Run This to see all results
#year_resistance
dfyear_resistance3dmap

#source_resistance
source_resistance3dmap

#clonalcomplex_resistance
clonalcomplex_resistance3dmap

#ST_resistance
ST_resistance3dmap

#rMLST_resistance
rMLST_resistance3dmap

#coreMLST
coreMLST_resistance3dmap

