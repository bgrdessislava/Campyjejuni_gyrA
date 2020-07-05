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
library(tidyverse)
library(reshape)
library(tidyr)


#ARCHIEVED
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")
#Loading all the foodchain data 
foodchain4 <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/food_chain4_aminoacid_aligned.fasta.txt")

#first row of the foodchain4 listed to be called a header = The column is currently called names 
header = names(foodchain4)

#read a file into table format and create a data frame from it. Since they are all divided by ; we can make them into 3 rows
df_foodchain4 <- read.table(text = header, sep = ";", colClasses = "character")
#lapply returns a list of the same length as x, By doing [[ it seems that it allows to access the number of element inside]]
position_gyrA86 <- unlist(lapply(foodchain4, "[[", 86))

#Take a sequence of vector, matrix or data-frame arguments and combine by columns 
bind_86 <- cbind.data.frame(df_foodchain4,position_gyrA86)
#Changing the column name to is,isolate,origin,position86
colnames(bind_86) <- c("id", "isolate","origin","position_86")

#########################################################################################################################################################################
#Creatinbg a data frame that includes all data from clonal complex to 86 position into one data frame 

#Foodchain meta data has all the clonal complex data in
foodchain4_metadata <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/food_chain4_merge.csv")

#The source had weird names where it had _1 after the source so decided to change this
bind_86$origin <- gsub("_1","",bind_86$origin)

#merg allows us to combine two datagrames by using identical identifier
all <- merge(bind_86,foodchain4_metadata,by = 'id')

#########################################################################################################################################################################









#########################################################################################################################################################################


#Creating dataframe by aggregating them

#A data frame is a list of variables of the same number of rows with unique row names, given class "data.frame"
# it counts the numbers of origin. 
total_counts_by_type = data.frame(table(all$origin))
#changed the name of total_counts_by_type to origin and total
colnames(total_counts_by_type) = c('origin', 'total')
#Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
#It looks at whether exact combination has been found before, id by
summary = aggregate(data = all,
                    id ~ origin + clonal_complex..MLST. + position_86,
                    length)

#d = data.frame(table(summary_origin_clonal$clonal_complex))

linear_4food <- summary_origin_clonal %>% select(clonal_complex, prop,origin)

linearplot4 <- ggplot(linear_4food,aes(x=origin,y=prop,colour=clonal_complex,label=prop)) + 
  geom_point() + 
  geom_line(aes(group = clonal_complex))  + 
  ggtitle("descriptive plot of 4 data points") #+ geom_text(check_overlap = TRUE)

path = "~/Documents/OneDrive - Nexus365/Campy/R/"
for (cc in unique(linear_4food$clonal_complex))
  {
  linearplot4 <- ggplot(linear_4food[linear_4food$clonal_complex == cc,],aes(x=origin,y=prop,colour=position_gyrA86,label=prop,colour = position_86)) + 
  geom_point() + 
  geom_line(aes(group = clonal_complex))  + 
  ggtitle(paste(cc,"descriptive plot of 4 data points"))
  ggsave(paste(path,cc,'.png',sep = ''))
}

path = "~/Documents/OneDrive - Nexus365/Campy/R/"
for (cc in unique(summary$clonal_complex))
{
  linearplot4 <- ggplot(summary[summary$clonal_complex == cc,],aes(x=origin,y=prop,label=prop)) + 
    geom_point() + 
    geom_line(aes(group = clonal_complex))  + 
    ggtitle(paste(cc,"descriptive plot of 4 data points"))
  ggsave(paste(path,cc,'.png',sep = ''))
}

path = "~/Documents/OneDrive - Nexus365/Campy/R/"
for (cc in unique(summary$clonal_complex))
{
  linearplot4 <- ggplot(summary[summary$clonal_complex == cc,],aes(x=origin,y=prop,label=prop,fill=position_86)) + 
    geom_bar(stat = "identity",position = position_dodge()) + 
    ggtitle(paste(cc,"descriptive plot of 4 data points"))
  ggsave(paste(path,cc,'boxplot.png',sep = ''))
}


linearplot4


#########################################################################################################################################################################

 colnames(summary) = c('origin','clonal_complex','position_86','origin_clonal_position86_pattern')

summary_drawn = ggplot(summary,
           aes(x = reorder(clonal_complex,position_86), 
               y = origin_clonal_position86_pattern)) +
  labs(y = "Percentage by origin",
       x = "Clonal complex",
       subtitle = "4 Steps of Food Chain and Clonal Complex, differences from the average") +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~origin,nrow = 1) +
  scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))


caecal_df <- subset.data.frame(summary,summary$origin == 'caecal')




plot(caecal_df$origin,caecal_df$clonal_complex)


caecal = ggplot(caecal_df,
                aes(x = clonal_complex,position_86), 
                    y = origin_clonal_position86_pattern) +
  labs(y = "Percentage by origin",
       x = "Clonal complex",
       subtitle = "???") #+
  geom_bar(stat = "identity") +
  #coord_flip() +
  #facet_wrap(~origin,nrow = 1) +
  #scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))











#########################################################################################################################################################################

caecal = ggplot(summary,
              aes(x = reorder(clonal_complex,position_86), 
                  y = origin_clonal_position86_pattern)) +
  labs(y = "Percentage by origin",
       x = "Clonal complex",
       subtitle = "4 Steps of Food Chain and Clonal Complex, differences from the average") +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~origin,nrow = 1) +
  scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))
caecal
