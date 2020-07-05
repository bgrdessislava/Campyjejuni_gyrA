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

#This file is to combine caecal carss food and both oxford and newcastle fsa data and trying to find out the genotype of the gyrA positioning
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#########################################################################################################################################################################
#finding the 86th position and loading that into a data frame

newcastle_oxford_fasta <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/oxford_newcastle_aminoacid_aligned.fas.txt")
caecal_carcass_food_fasta<- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/caecal_carcass_food_aminoacid_aligned.fas.txt")


#first row of the foodchain4 listed to be called a header = The column is currently called names 
header = names(newcastle_oxford_fasta)
header_chicken = names(caecal_carcass_food_fasta)

#read a file into table format and create a data frame from it. Since they are all divided by ; we can make them into 3 rows
a <- read.table(text = header, sep = ";", colClasses = "character",fill = TRUE)
#lapply returns a list of the same length as x, By doing [[ it seems that it allows to access the number of element inside]]
position_gyrA86 <- unlist(lapply(newcastle_oxford_fasta, "[[", 86))
a_chicken <- read.table(text = header_chicken, sep = ";", colClasses = "character",fill = TRUE)
position_gyrA86_chicken <- unlist(lapply(caecal_carcass_food_fasta, "[[", 86))

#Take a sequence of vector, matrix or data-frame arguments and combine by columns 
#bind_86 <- cbind.fill(a,position_gyrA86)
bind_86 <- cbind.data.frame(a,position_gyrA86)
bind_86_chicken <- cbind.data.frame(a_chicken,position_gyrA86_chicken)

#Changing the column name to is,isolate,origin,position86
colnames(bind_86) <- c("id", "position_86")
colnames(bind_86_chicken) <- c("id", "position_86")

all_foodchain <- rbind(bind_86,bind_86_chicken)

caecal_ID <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/caecal_ID.csv")
carcass_ID <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/carcass_ID.csv")
food_ID <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/food_ID.csv")
human_feces_ID <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/human_feces_ID.csv")

source_tab_merge <- rbind(caecal_ID,carcass_ID,food_ID,human_feces_ID)

total = merge(all_foodchain,source_tab_merge,by = 'id')
write.csv(total,'sourcetab_gyrA86position.csv')

