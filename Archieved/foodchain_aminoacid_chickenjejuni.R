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

#This file seems to find the mismatched phenotype to genotype but it looks like the code is not working when
#I am choosing the 86th position. --> This is because it was not aligned. = ARCHIEVED

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

#########################################################################################################################################################################
#finding the 86th position and loading that into a data frame
#Loading all the foodchain data 
foodchain4 <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/foodchain_aminoacid_notaogned.fas.txt")


#first row of the foodchain4 listed to be called a header = The column is currently called names 
header = names(foodchain4)

#read a file into table format and create a data frame from it. Since they are all divided by ; we can make them into 3 rows
a <- read.table(text = header, sep = "|", colClasses = "character",fill = TRUE)
#lapply returns a list of the same length as x, By doing [[ it seems that it allows to access the number of element inside]]
position_gyrA86 <- unlist(lapply(foodchain4, "[[", 86))


#Take a sequence of vector, matrix or data-frame arguments and combine by columns 
#bind_86 <- cbind.fill(a,position_gyrA86)
bind_86 <- cbind.data.frame(a,position_gyrA86)

#bind_86 <- merge(a,position_gyrA86)
#Changing the column name to is,isolate,origin,position86
colnames(bind_86) <- c("id", "isolate","origin","position_86")
#########################################################################################################################################################################
#Creatinbg a data frame that includes all data from clonal complex to 86 position into one data frame 

#Foodchain meta data has all the clonal complex data in
newcastle_phenotype_meta<- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/newcastle_phenotype_meta.csv")

#The source had weird names where it had _1 after the source so decided to change this
#bind_86$origin <- gsub("_1","",bind_86$origin)

#merg allows us to combine two datagrames by using identical identifier
#Foodchain meta data has all the clonal complex data in
c <- merge(bind_86,newcastle_phenotype_meta,by = 'id')
c[,"check_mismatch"] <- NA

write.csv(c,'mismatch_newcastle_phenotype.csv')

if (c$ciprofloxacin_phenotype == 'S'){
  c$check_mismatch = 'S'
}else {(c$ciprofloxacin_phenotype == 'R')
  c$check_mismatch = 'R'  }

