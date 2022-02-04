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

rotate <- function(x) t(apply(x,2,rev))
year_resistance = rotate(year_resistance)
#3dmap that is interactive

pheatmap(scale(year_resistance))

year_resistance3dmap<- d3heatmap(scale(year_resistance,center=FALSE), colors = "viridis", dendogram = 'none',
                                  k_row = 2, # Number of groups in rows
                                  k_col = 5 # Number of groups in columns
)

year_resistance3dmap

#######Source Resistance Starts HERE#############################################
#################################################################################
source_resistance <-table(long_86_position$source,long_86_position$base)
#CHooses only I and T
source_resistance <- source_resistance[,c(2,4)]
#Choosing isolates above certain number
#source_resistance = source_resistance[rowSums(source_resistance) > 100,]
#Makes it into a dataframe
source_resistance = data.frame(source_resistance)

#dcast leads long to wide
source_resistance = dcast(source_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(source_resistance) = source_resistance[,1]
#Gets rid of the first column
source_resistance[,1] = NULL

rotate <- function(x) t(apply(x,2,rev))
source_resistance = rotate(source_resistance)
#3dmap that is interactive

pheatmap(scale(source_resistance))

source_resistance3dmap<- d3heatmap(scale(source_resistance,center=FALSE), colors = "viridis", dendogram = 'none',
                                 k_row = 2, # Number of groups in rows
                                 k_col = 1 # Number of groups in columns
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
ST_resistance = as.data.frame.matrix(ST_resistance) 
#dcast leads long to wide
ST_resistance = dcast(ST_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(ST_resistance) = ST_resistance[,1]
#Gets rid of the first column
ST_resistance[,1] = NULL
rotate <- function(x) t(apply(x,2,rev))
ST_resistance = rotate(ST_resistance)
#3dmap that is interactive

pheatmap(scale(ST_resistance))

ST_resistance3dmap<- d3heatmap(ST_resistance, colors = "viridis", dendogram = 'none',
                                   k_row = 2, # Number of groups in rows
                                   k_col = 6 # Number of groups in columns
)

ST_resistance3dmap

#######Clonal Complex    Starts HERE#############################################
#################################################################################
clonalcomplex_resistance <-table(long_86_position$clonal_complex,long_86_position$base)

#Choosing only I and T
clonalcomplex_resistance <- clonalcomplex_resistance[,c(2,4)]
#Choosing samples above this threshold
clonalcomplex_resistance = clonalcomplex_resistance[rowSums(clonalcomplex_resistance) > 100,]

#Makes it into a dataframe
clonalcomplex_resistance = data.frame(clonalcomplex_resistance)
clonalcomplex_resistance = as.data.frame.matrix(clonalcomplex_resistance) 
#dcast leads long to wide
clonalcomplex_resistance = dcast(clonalcomplex_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(clonalcomplex_resistance) = clonalcomplex_resistance[,1]
#Gets rid of the first column
clonalcomplex_resistance[,1] = NULL


rotate <- function(x) t(apply(x,2,rev))
clonalcomplex_resistance = rotate(clonalcomplex_resistance)
#3dmap that is interactive

pheatmap(scale(clonalcomplex_resistance))

clonalcomplex_resistance3dmap<- d3heatmap(scale(clonalcomplex_resistance,center=FALSE), colors = "viridis", dendogram = 'none',
                               k_row = 2, # Number of groups in rows
                               k_col = 4 # Number of groups in columns
)

clonalcomplex_resistance3dmap


#######coreMLST Starts HERE#############################################
#################################################################################
coreMLST_resistance <-table(long_86_position$cgMLST.coli_jejuni.,long_86_position$base)

#Choosing only I and T
coreMLST_resistance <- coreMLST_resistance[,c(2,4)]
#Choosing samples above this threshold
coreMLST_resistance = coreMLST_resistance[rowSums(coreMLST_resistance) > 5,]

#Makes it into a dataframe
coreMLST_resistance = data.frame(coreMLST_resistance)
coreMLST_resistance = as.data.frame.matrix(coreMLST_resistance) 
#dcast leads long to wide
coreMLST_resistance = dcast(coreMLST_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(coreMLST_resistance) = coreMLST_resistance[,1]
#Gets rid of the first column
coreMLST_resistance[,1] = NULL


rotate <- function(x) t(apply(x,2,rev))
coreMLST_resistance = rotate(coreMLST_resistance)
#3dmap that is interactive

pheatmap(scale(coreMLST_resistance))

coreMLST_resistance3dmap<- d3heatmap(scale(coreMLST_resistance,center=FALSE), colors = "viridis", dendogram = 'none',
                                          k_row = 2, # Number of groups in rows
                                          k_col = 2 # Number of groups in columns
)

coreMLST_resistance3dmap

#######rMLST Resistance Starts HERE#############################################
#################################################################################
rMLST_resistance <-table(long_86_position$rMLST,long_86_position$base)

#Choosing only I and T
rMLST_resistance <- rMLST_resistance[,c(2,4)]
#Choosing samples above this threshold
rMLST_resistance = rMLST_resistance[rowSums(rMLST_resistance) > 70,]

#Makes it into a dataframe
rMLST_resistance = data.frame(rMLST_resistance)
rMLST_resistance = as.data.frame.matrix(rMLST_resistance) 
#dcast leads long to wide
rMLST_resistance = dcast(rMLST_resistance, Var1 ~ Var2, value.var = "Freq")
#Choose all the year values
rownames(rMLST_resistance) = rMLST_resistance[,1]
#Gets rid of the first column
rMLST_resistance[,1] = NULL


rotate <- function(x) t(apply(x,2,rev))
rMLST_resistance = rotate(rMLST_resistance)
#3dmap that is interactive

pheatmap(scale(rMLST_resistance))

rMLST_resistance3dmap<- d3heatmap(scale(rMLST_resistance,center=FALSE), colors = "viridis", dendogram = 'none',
                                     k_row = 2, # Number of groups in rows
                                     k_col = 3 # Number of groups in columns
)

rMLST_resistance3dmap


#Run This to see all results
#year_resistance
year_resistance3dmap

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


