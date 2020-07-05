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

#This file is to compare Newcastle FSA for phenotype vs genotype 
#Looks like a good code that can be used to see the mis-match isolates to show which one has been mis-represented.
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")

#########################################################################################################################################################################
#finding the 86th position and loading that into a data frame
#Loading all the foodchain data 
newcastle_fasta <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/newcastle_aminoacid_aligned.fasta.txt")


#first row of the foodchain4 listed to be called a header = The column is currently called names 
header = names(newcastle_fasta)

#read a file into table format and create a data frame from it. Since they are all divided by ; we can make them into 3 rows
a <- read.table(text = header, sep = ";", colClasses = "character",fill = TRUE)
#lapply returns a list of the same length as x, By doing [[ it seems that it allows to access the number of element inside]]
position_gyrA86 <- unlist(lapply(newcastle_fasta, "[[", 86))



#Take a sequence of vector, matrix or data-frame arguments and combine by columns 
#bind_86 <- cbind.fill(a,position_gyrA86)
bind_86 <- cbind.data.frame(a,position_gyrA86)

#Changing the column name to is,isolate,origin,position86
colnames(bind_86) <- c("id", "position_86")

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

