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
library(BiodiversityR)
library(vegan)
library(grid)

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")
#This file contains the frequency graph of all the 4 foodchains and whether it inclused the resistance or not. Must check!!! 
#########################################################################################################################################################################
#finding the 86th position and loading that into a data frame
#Loading all the foodchain data 
foodchain4 <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/food_chain4_aminoacid_aligned.fasta.txt")
#jejuni_chicken_foodchain <- read.fasta("~/Documents/OneDrive - Nexus365/PhD/R/alighnment_translation_clustalo/jejuni_chicken/jejuni_chicken_alighnment_correctheader.fasta.txt")

#first row of the foodchain4 listed to be called a header = The column is currently called names 
header = names(foodchain4)

#read a file into table format and create a data frame from it. Since they are all divided by ; we can make them into 3 rows
a <- read.table(text = header, sep = ";", colClasses = "character",fill = TRUE)
#lapply returns a list of the same length as x, By doing [[ it seems that it allows to access the number of element inside]]
position_gyrA86 <- unlist(lapply(foodchain4, "[[", 86))

#Take a sequence of vector, matrix or data-frame arguments and combine by columns 
bind_86 <- cbind.data.frame(a,position_gyrA86)
#Changing the column name to is,isolate,origin,position86

colnames(bind_86) <- c("id","additional","origin","position_86")

#########################################################################################################################################################################
#Creatinbg a data frame that includes all data from clonal complex to 86 position into one data frame 

#Foodchain meta data has all the clonal complex data in
foodchain4_metadata <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/alighnment_translation_clustalo/food_chain4_merge.csv")

#The source had weird names where it had _1 after the source so decided to change this
bind_86$origin <- gsub("_1","",bind_86$origin)

#merg allows us to combine two datagrames by using identical identifier
c <- merge(bind_86,foodchain4_metadata,by = 'id')

camp0950 = c %>% select(origin, position_86,CAMP0950)

totals = summarise(group_by(camp0950,position_86,CAMP0950),total = n())
write.csv(totals,file = "gyrase_sequence_table.csv")

gyrase <- ggplot(camp0950, aes(position_86))  + geom_bar(aes(fill = camp0950)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
gyrase

campylobacter_jejuni_chicken_df_86position = c %>% filter(c$id %in% id_campyjejuni_chikenonly)

resistant<-subset(campylobacter_jejuni_chicken_df_86position, position_86 == 'i')
plot(resistant$clonal_complex..MLST.,resistant$source)

g <- ggplot(resistant, aes(clonal_complex..MLST.))  + geom_bar(aes(fill = source)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

table_camp0950_resistant <- campylobacter_jejuni_chicken_df_86position %>% select(origin,CAMP0950)

count_camp0950 = aggregate(table_camp0950_resistant,
                  by = list(table_camp0950_resistant$CAMP0950,table_camp0950_resistant$origin),
                FUN = length)

count_camp0950_mlst = aggregate(table_camp0950_resistant,
                           by = list(table_camp0950_resistant$CAMP0950),
                           FUN = length)

barplot(count_camp0950_mlst$Group.1,count_camp0950_mlst$CAMP0950) 

p<-ggplot(data=count_camp0950_mlst, aes(x=Group.1, y=CAMP0950)) +
  geom_bar(stat="identity")
p

# Number of cars in each class:


gyr86_chicken_raw <- bind_86[,c("id","origin","position_86")]
write.csv(gyr86_chicken_raw, file = "gyr86_chicken_raw.csv")


#########################################################################################################################################################################
#Creating dataframe by aggregating them

#Taking out the campylobacter coli and other sources than chicken or human stool!
campylobacter_jejuni_chicken_df <- c %>% filter(species == 'Campylobacter jejuni',source == 'chicken'|source =='chicken offal or meat'|source =='human stool')
write.csv(campylobacter_jejuni_chicken_df,"~/Documents/OneDrive - Nexus365/PhD/R/campylobacter_jejuni_chicken_df_foodchain.csv")

test2 = campylobacter_jejuni_chicken_df %>%
  filter(position_86 =="t")

campylobacter_jejuni_chicken_df <- test2


id_campyjejuni_chikenonly = campylobacter_jejuni_chicken_df$id

# it counts the numbers of origin. 
total_counts_by_type = data.frame(table(campylobacter_jejuni_chicken_df$origin))
#changed the name of total_counts_by_type to origin and total
colnames(total_counts_by_type) = c('origin', 'total')
#Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
#It looks at whether exact combination has been found before, id by
summary = aggregate(data = campylobacter_jejuni_chicken_df,
                    id ~ origin + clonal_complex..MLST. + position_86 ,
                    length)

colnames(summary) = c('origin', 'clonal_complex','position_86','origin_clonal_position86_pattern')

#Merging the total count_by_type to summary = we are adding the total number of each source to be another column
summary = merge(total_counts_by_type,summary)



##############################################################################
#creating porpotions of how often the pattern is found out of the total

#By doing this we find out the proportion of each values from all of it and then by *100 make sure that it is in percentage
#prop = proportion of the origin_clonal_position86 found divided by the total count
summary <- summary$position_86 == "i"

summary$prop = 100 * summary$origin_clonal_position86_pattern / summary$total

#created a new data frame that only counts the origin and clonal complex patterns
summary_origin_clonal = aggregate(data = campylobacter_jejuni_chicken_df,
                    id ~ origin + clonal_complex..MLST.,
                    length)
#Change the name of the summary_difference column names
colnames(summary_origin_clonal) = c('origin', 'clonal_complex','origin_clonal_position86_pattern')
#########################################################################################################################################################################
#Making another dataframe which does not include the 86th position

#Because we do not mind whether the isolate is resistant or suscpetible we just use the data without the 86th position
summary_origin_clonal = merge(total_counts_by_type,summary_origin_clonal)
summary_origin_clonal$prop = 100 * summary_origin_clonal$origin_clonal_position86_pattern / summary_origin_clonal$total #prop is the proportion of the pattern found / total number of id in that source


#average value for each clonal complex
mean_by_clonal = aggregate(x=summary_origin_clonal$prop,
          by=list(summary_origin_clonal$clonal_complex),
          FUN=mean)
colnames(mean_by_clonal) = c('clonal_complex','prop_all')  #prop_all is the averge number of each individual clonal complex
summary_origin_clonal = merge(summary_origin_clonal,mean_by_clonal)


#This creates a column that makes the
summary_origin_clonal$difference<-summary_origin_clonal$prop - summary_origin_clonal$prop_all

average_difference = ggplot(summary_origin_clonal,
           aes(x = reorder(clonal_complex, -difference),
               #sfill = bind_86$position_86,
               y = difference)) +
  labs(y = "Percentage by origin",
       x = "Clonal complex",
       subtitle = "Fluoroquinolone susceptible point mutation average proportion across 4 food chain") +
  geom_bar(stat = "identity", fill="blue") +
  coord_flip() +
  facet_wrap(~origin,nrow = 1) +
  scale_fill_discrete(name = "gyrA position 86", labels = "Isoleucine")

average_difference


clonalcomplex_foodchain_variety_percentage= plot(summary_origin_clonal$clonal_complex,summary_origin_clonal$difference)

rotate(clonalcomplex_foodchain_variety_percentage)
#Adds an axes for each of the clonal complex
plot(summary_origin_clonal$clonal_complex,summary_origin_clonal$difference ,  pch=3, las=2,col.axis = 'black', col.lab = 'red', cex.axis = 0.6, cex.lab = 3,main="Barplot across 4 steps in foodchain")
#########################################################################################################################################################################


#4steps of food chain with clonal complex
foodchain_frequency_clonalcomplex_barplot = ggplot(summary,
           aes(x = reorder(clonal_complex, prop),
               fill = position_86,
               y = prop)) +
  labs(y = "Percentage by origin",
       x = "Clonal complex",
       subtitle = "Campylobacter Jejuni point mutation across food Chain 4 steps and Newcastle year 1 study (2016-2017)")+
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~origin,nrow = 1) +
  scale_fill_manual(values=c("seagreen3", "red2", "blue2"), name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))  
#+
  #theme_pubr(legend = "bottom") 
foodchain_frequency_clonalcomplex_barplot
########################################################################################################################################################################################################################################################
#relative abundance plot


caecal_sample = summary %>%
  filter(str_detect(origin,"caecal"))

caecal_example_sort <-caecal_sample[order(-caecal_sample$prop),]

ggplot(data=caecal_example_sort, aes(x=clonal_complex, y=len, group=1)) +
  geom_line()+
  geom_point()


########################################################################################################################################################################################################################################################

#By interchanging the filter we can get 4 different bar charts on the graph

summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(prop)) %>% 
  filter(origin == "humanfeces") %>%
  ggplot(aes(x = reorder(clonal_complex, -ordering_val),
           fill = position_86, 
           y = prop)) +
  labs(y = "Percentage by origin",
       x = "Clonal complex",
       subtitle = "Human Feces Clonal Complex") +
  geom_bar(stat = "identity") +
  coord_flip() 
  #scale_fill_discrete(name = "gyrA position 86", labels = c("Isoleucine", "Threonine"))
  #facet_wrap(~origin, nrow = 1) +
  #scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))


# Without filling whether it is resistant or notso just in one color
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(prop)) %>% 
  filter(origin == "humanfeces") %>%
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -ordering_val),
             y = prop)) +
  labs(y = "Percentage by origin",
       x = "Clonal complex",
       subtitle = "Human Feces Clonal Complex _resistant") +
  geom_bar(stat = "identity",fill = 'green4' )+ 
  coord_flip() 



