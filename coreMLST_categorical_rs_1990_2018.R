library(readbulk)
library(dplyr)
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")

#Adding all the files in one place - Read and combine multiple data files. 
#The files will be merged into one data.frame.
raw_data <- read_bulk(extension = "1990_2018_*.csv")

meta <- read.csv("gyrA_metadata_combined_1990_2018.csv")
meta <- meta %>% 
  select(ID,gyrA_base)

combined <- merge(raw_data,meta)

write.csv(combined,file="coreMLST_categorical_gyrA_1990_2018")
