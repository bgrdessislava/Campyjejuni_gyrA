setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

#This code does not give any ggplot and seems to not be useful in any of the final graphs

library(seqinr)
library(stringr)
library(dplyr)
library(rowr)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(scales)
library(ggpubr)
library(plotly)
library(gtools)
library(tidyr)
library(reshape2)
library(FactoMineR)
##############################################################################################################
#This document will create a document that let's 4 foodchain coreMLST data 
caecal_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/data/caecal_coreMLST_copy.csv")
carcass_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/data/carcass_coreMLST.csv")
food_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/data/food_coreMLST.csv")
humanfeces_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/data/Newcastleyear1dd_coreMLST.csv")

foodchain_raw <- smartbind(caecal_raw,carcass_raw,food_raw,humanfeces_raw)


test_practice <- foodchain_raw[,c("id","source_tab","species","CAMP0001","CAMP0002","CAMP0950")]
test_practice <- data.frame(test_practice)


out_testpracti_factor <- transform(test_practice,species=unlist(species))

for (i in 1:6) {
  plot(test_practice[,i], main=colnames(test_practice)[i],
       ylab = "Count", col="steelblue", las = 2)}

categorie_number = apply(foodchain_raw, 2, function(x) nlevels(as.factor(x)))
categorie_number 

mca1 = MCA(test_practice, graph = FALSE)

dimnames(test_practice)

##############################################################################################################
#Cleaning dataset to be able to rbind them
caecal_clean <- caecal_raw %>% select("id","source_tab","year","region","species","fluoroquinolone_genotypes_1","fluoroquinolone_genotypes_2","CAMP0950")
carcass_clean <- carcass_raw %>% select("id","source_tab","year","region","species","fluoroquinolone_genotypes_1","fluoroquinolone_genotypes_2","CAMP0950")
food_clean <- food_raw %>% select("id","source_tab","year","region","species","fluoroquinolone_genotypes_1","fluoroquinolone_genotypes_2","CAMP0950")
humanfeces_clean <- humanfeces_raw %>% select("id","source_tab","year","region","species","fluoroquinolone_genotypes_1","fluoroquinolone_genotypes_2","CAMP0950")

foodchain_raw <- rbind(caecal_clean,carcass_clean,food_clean,humanfeces_clean)

##############################################################################################################

#Loading all the foodchain data 
foodchain4 <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/data/food_chain4_aminoacid_aligned.fasta_copy.txt")

#first row of the foodchain4 listed to be called a header = The column is currently called names 
header = names(foodchain4)

#read a file into table format and create a data frame from it. Since they are all divided by ; we can make them into 3 rows
table_food4 <- read.table(text = header, sep = ";", colClasses = "character")
#lapply returns a list of the same length as x, By doing [[ it seems that it allows to access the number of element inside]]
position_gyrA86 <- unlist(lapply(foodchain4, "[[", 86))

#Take a sequence of vector, matrix or data-frame arguments and combine by columns 
bind_86 <- cbind.data.frame(table_food4,position_gyrA86)
#Changing the column name to is,isolate,origin,position86
colnames(bind_86) <- c("id", "isolate","origin","position_86")

#The source had weird names where it had _1 after the source so decided to change this
bind_86$origin <- gsub("_1","",bind_86$origin)

colnames(bind_86)[3]<-"source_tab"

meta_clonal_4foodchain <- merge(bind_86,foodchain_raw,by = 'id')

#What I have to do now is see whether all corrMLST are also I in the 86th position, Make some kind of bar plot will be good! 

check <- table(meta_clonal_4foodchain$position_86,meta_clonal_4foodchain$CAMP0950)
# Making it into a data frame
check <- as.data.frame(check)

# r subset data frame multiple conditions
resistance <- subset(check, check$Var1=='i')
# subset command in r
endpoints <-ChickWeight[(ChickWeight$Time < 3) | (ChickWeight$Time > 20),]

check2 <- table(meta_clonal_4foodchain$CAMP0950,meta_clonal_4foodchain$position_86)

p = ggplot(resistance,
           aes(x = Var2,
               y = Freq)) +
  geom_bar(stat="identity", width=0.5)
p
summary(resistance$Freq)

morethan1_resistance <- resistance[resistance$Freq > 1,]

resistance_morethan1 = ggplot(morethan1_resistance,
           aes(x = reorder(Var2,-Freq),
               y = Freq)) +
  geom_bar(stat="identity", width=0.5)

resistance_morethan1

