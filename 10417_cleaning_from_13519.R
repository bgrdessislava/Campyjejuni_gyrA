library(ggplot2)
library(stats)
library(gplots)
library(d3heatmap)
library(reshape2)
library(pheatmap)
library(dplyr)
library(tidyverse)
library(viridis)

#This code is used to subset clonal-complex data from the big data set

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
year_df_old<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list_allmeta.csv")
#year_10417_df <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/10417_dataframe_meta.csv")
year_10369_df <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CleanMeta_for_Tableau_10369.csv")
merged_df = merge(year_10369_df, year_df_old[, c('ID', 'base')],
                  by.x='id', by.y='ID')

write.csv(merged_df, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/10369_df_allMeta_toUSE.csv")
#CC353
cc_meta353 <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC353_of10362.csv")
merged_df = merge(cc_meta353 , year_df_old[, c('ID', 'base')],
                  by.x='id', by.y='ID')
write.csv(merged_df, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC353_481_df_allMeta_toUSE.csv")

#CC21
cc_meta21 <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC21_of10362.csv")
merged_df = merge(cc_meta21 , year_df_old[, c('ID', 'base')],
                  by.x='id', by.y='ID')
write.csv(merged_df, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC21_2855isolate_df_allMeta_toUSE.csv")

#CC48
cc_meta48 <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC48_of10362.csv")
merged_df = merge(cc_meta48 , year_df_old[, c('ID', 'base')],
                  by.x='id', by.y='ID')
write.csv(merged_df, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC48_1053isolate_df_allMeta_toUSE.csv")

#CC45
cc_meta45 <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC45_of10362.csv")
merged_df = merge(cc_meta45 , year_df_old[, c('ID', 'base')],
                  by.x='id', by.y='ID')
write.csv(merged_df, file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/CC45_859isolate_df_allMeta_toUSE.csv")




      