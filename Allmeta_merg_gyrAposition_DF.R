library(tidyverse)

#This is a golden file
#This file has all 16413 isolates and I am combining the metadata to gyrase positions

setwd("~/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

meta<- read.csv("~/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/Allmeta_data_1990_2018.txt", 
                sep ='\t')

meta %>% 
  rename(
    ID = id)

gyrAposition_raw<- read.csv("~/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/long_list_86_position.csv")

names(meta)[names(meta) == "id"] <- "ID"

gyrAposition_final <- gyrAposition_raw[,c("ID","base")]

gyrA_meta_combined<- merge(meta,gyrAposition_final,by="ID")


names(gyrA_meta_combined)[names(gyrA_meta_combined) == "base"] <- "gyrA_base"

write.csv(gyrA_meta_combined,file = "~/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/gyrA_metadata_combined_1990_2018.csv")
