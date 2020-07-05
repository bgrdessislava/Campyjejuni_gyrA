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
#THIS IS GOLD
#This file makes the community dynamics graphs, Made the comparing food chain across the clonal complex graph
#FLuoroquinolone suscpetible and resistance change across food chain analysis
#But it is using one of the early formats that do not include couple of the factors. 
#Use this code onto the biggest whole data frame so we get this across time as well. 


dir = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data"
setwd(dir)
# This file contains the frequency graph of all the 4 foodchains and whether it
# inclused the resistance or not. 
##################################################################################
#finding the 86th position and loading that into a data frame
fasta = paste(dir,"4food_chain_aminoacid_aligned.fasta",sep='/')
foodchain4 <- read.fasta(fasta)

# Making the vriaable of header to be called foodchain4
header = names(foodchain4)

#read a file into table format and create a data frame from it. 
# This makes the header into 3 columns 
#Since they are all divided by ; we can make them into 3 rows
foodchain4_header <- read.table(text = header, sep = ";", 
                colClasses = "character", fill = TRUE)
#lapply returns a list of the same length as x, By doing [[ it seems that it a
#llows to access the number of element inside]]
gyrA86 <- unlist(lapply(foodchain4, "[[", 86))

#Take a sequence of vector, matrix or data-frame arguments and combine by columns 
gyrA86 <- cbind.data.frame(foodchain4_header,gyrA86)
#Changing the column name to is,isolate,source,position86

colnames(gyrA86) <- c("id","additional","origin","position_86")

##################################################################################
#Combining 86th position and meta data

#Foodchain meta data has all the clonal complex data in
metadata <- read.csv(paste(dir,"food_chain4_merge.csv",sep='/'))
# Update level name of clonal complex factor where level is ''
levels(metadata$clonal_complex..MLST.)[levels(metadata$clonal_complex..MLST.) == ''] = 'unknown'

#The source had weird names where it had _1 after the source so decided to change this
gyrA86$origin <- gsub("_1","",gyrA86$origin)

#merg allows us to combine two datagrames by using identical identifier
gyrA86 <- merge(gyrA86,metadata,by = 'id')

#Just picking up the gyrase allele - Because there were two sources one became source.x and other one source.y
camp0950 = gyrA86 %>% select(origin, position_86,CAMP0950)

gyrA86_summary = summarise(group_by(camp0950,position_86,CAMP0950),total = n())
write.csv(gyrA86_summary,file = paste(dir,"gyrASeqType.csv",sep=''))


###################################################################################

#Taking out the campylobacter coli and other sources than chicken or human stool!
gyrA86 <- gyrA86 %>% 
  filter(species == 'Campylobacter jejuni',
         source == 'chicken'| source =='chicken offal or meat' | source == 'human stool')
write.csv(gyrA86, paste(dir, "gyrA86_jejuni_foodchain4.csv", sep=''))

#This is to filter out just with 'T' samples
#suscpetible = gyrA86 %>%
#  filter(position_86 =="t")

#gyrA86 <- susceptible


# it counts the numbers of source. 
gyrA86_type = data.frame(table(gyrA86$origin))

gyrA86_suscpetible = 
  data.frame(table(
    gyrA86[gyrA86$position_86 == 't','origin']))

gyrA86_resistant = 
  data.frame(table(
    gyrA86[gyrA86$position_86 == 'i','origin']))


#changed the name of gyrA86_type to source and total
colnames(gyrA86_type) = c('origin', 'total')
#Splits the data into subsets, computes summary statistics for each, 
#and returns the result in a convenient form.
#It looks at whether exact combination has been found before, id by
summary = aggregate(data = gyrA86,
                    id ~ origin + clonal_complex..MLST. + position_86 ,
                    length)

colnames(summary) = c('origin', 'clonal_complex','position_86','source_clonal_position86_pattern')

#Merging the total count_by_type to summary 
#we are adding the total number of each source to be another column
summary = merge(gyrA86_type,summary)


##############################################################################
#creating porpotions of how often the pattern is found out of the total

#By doing this we find out the percentage of each values from all of it and then 
#by *100 make sure that it is in percentage
#percentage = proportion of the source_clonal_position86 found divided by the total count
#summary = summary %>% filter(position_86 == 'i')

summary$percentage = 100 * summary$source_clonal_position86_pattern / summary$total

#created a new data frame that only counts the source and clonal complex patterns
summary_source_clonal = gyrA86 %>% 
  group_by(source,clonal_complex..MLST.) %>% 
  tally() 

#Change the name of the summary_difference column names
colnames(summary_source_clonal) = c('source', 'clonal_complex','source_clonal_position86_pattern')
#########################################################################################################################################################################
#Making another dataframe which does not include the 86th position

#Because we do not mind whether the isolate is resistant or suscpetible 
#we just use the data without the 86th position
summary_source_clonal = merge(gyrA86_type,summary_source_clonal)
summary_source_clonal$percentage = 100 * summary_source_clonal$source_clonal_position86_pattern / 
  summary_source_clonal$total 
#percentage is the percentage of the pattern found / total number of id in that source


#average value for each clonal complex
mean_by_clonal = aggregate(x=summary_source_clonal$percentage,
          by=list(summary_source_clonal$clonal_complex),
          FUN=mean)
colnames(mean_by_clonal) = c('clonal_complex','percentage_all')  #percentage_all is the averge number of each individual clonal complex
summary_source_clonal = merge(summary_source_clonal,mean_by_clonal)


#This creates a column that makes the
summary_source_clonal$difference<-summary_source_clonal$percentage - summary_source_clonal$percentage_all

average_difference = ggplot(summary_source_clonal,
           aes(x = reorder(clonal_complex, -difference),
               fill = gyrA86$position_86,
               y = difference)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Fluoroquinolone susceptible point mutation average percentageortion across 4 food chain Newcastle and Oxford") +
  geom_bar(stat = "identity", fill="blue") +
  coord_flip() +
  facet_wrap(~origin,nrow = 1) 
 # scale_fill_discrete(name = "gyrA position 86", labels = "Isoleucine")

average_difference


clonalcomplex_foodchain_variety_percentage= plot(summary_source_clonal$clonal_complex,summary_source_clonal$difference)

rotate(clonalcomplex_foodchain_variety_percentage)
#Adds an axes for each of the clonal complex
plot(summary_source_clonal$clonal_complex,summary_source_clonal$difference ,  pch=3, las=2,col.axis = 'black', col.lab = 'red', cex.axis = 0.6, cex.lab = 3,main="Barplot across 4 steps in foodchain")
#########################################################################################################################################################################


#4steps of food chain with clonal complex
foodchain_frequency_clonalcomplex_barplot = ggplot(summary,
           aes(x = reorder(clonal_complex, percentage),
               fill = position_86,
               y = percentage)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Campylobacter Jejuni point mutation across food Chain 4 steps and Newcastle year 1 study (2016-2017)")+
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~origin,nrow = 1) +
  scale_fill_manual(values=c("seagreen3", "red2", "black",'blue'), name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Lysine","Threonine"))  
#+
  #theme_pubr(legend = "bottom") 
foodchain_frequency_clonalcomplex_barplot
########################################################################################################################################################################################################################################################
#relative abundance plot


caecal_sample = summary %>%
  filter(str_detect(origin,"caecal"))

caecal_example_sort <-caecal_sample[order(-caecal_sample$percentage),]

#ggplot(data=caecal_example_sort, aes(x=clonal_complex, y=len, group=1)) +
 # geom_line()+
  #geom_point()


########################################################################################################################################################################################################################################################

#Resistant and Suscpetible biological diversity for 4 food chain stage - Human Faeces 

summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "humanfeces") %>%
  ggplot(aes(x = reorder(clonal_complex, -ordering_val),
           fill = position_86, 
           colour = position_86,
           y = percentage,
           group = 1))+
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Human Feces Clonal Complex and gyrA position") +
  geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(position_86 ~ .)
  #scale_fill_discrete(name = "gyrA position 86", labels = c("Isoleucine", "Threonine"))
  #facet_wrap(~source, nrow = 1) +
  #scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))


#Resistant and Suscpetible biological diversity for 4 food chain stage - Caecal
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "caecal") %>%
  ggplot(aes(x = reorder(clonal_complex, -ordering_val),
             fill = position_86, 
             colour = position_86,
             y = percentage,
             group = 1))+
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Caecal Clonal Complex and gyrA position") +
  geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(position_86 ~ .)
#scale_fill_discrete(name = "gyrA position 86", labels = c("Isoleucine", "Threonine"))
#facet_wrap(~source, nrow = 1) +
#scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))


#Resistant and Suscpetible biological diversity for 4 food chain stage - Human Faeces 

summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "carcass") %>%
  ggplot(aes(x = reorder(clonal_complex, -ordering_val),
             fill = position_86, 
             colour = position_86,
             y = percentage,
             group = 1))+
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Carcass Clonal Complex and gyrA position") +
  geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(position_86 ~ .)
#scale_fill_discrete(name = "gyrA position 86", labels = c("Isoleucine", "Threonine"))
#facet_wrap(~source, nrow = 1) +
#scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))

#Resistant and Suscpetible biological diversity for 4 food chain stage - Human Faeces 

summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "food") %>%
  ggplot(aes(x = reorder(clonal_complex, -ordering_val),
             fill = position_86, 
             colour = position_86,
             y = percentage,
             group = 1))+
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Food Clonal Complex and gyrA position") +
  geom_bar(stat = "identity", position = "dodge",alpha = 0.3) +
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_grid(position_86 ~ .)
#scale_fill_discrete(name = "gyrA position 86", labels = c("Isoleucine", "Threonine"))
#facet_wrap(~source, nrow = 1) +
#scale_fill_discrete(name = "gyrA position 86", labels = c("Alanine", "Isoleucine", "Threonine"))




# Without filling whether it is resistant or notso just in one color - Human Feces
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "humanfeces") %>%
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -percentage),
             y = percentage,
             group = 1)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Human Feces Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity",fill = 'green4' ,alpha = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_line()



# Caecal
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "caecal") %>%
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -percentage),
             y = percentage,
             group = 1)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Caecal Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity",fill = 'green4' ,alpha = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_line()

#Experimenting with this one

summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(source_clonal_position86_pattern)) %>% 
  filter(origin == "caecal") %>%
  ggplot(aes(x = reorder(clonal_complex,-source_clonal_position86_pattern),
             y = source_clonal_position86_pattern,
             group = 1)) +
  labs(y = "Abundance",
       x = "Clonal complex",
       subtitle = "Caecal Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity",fill = 'green4' ,alpha = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 




#All 4 foodchain but not in order

summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -ordering_val),
             y = percentage,
             fill = origin,
             colour = origin,
             group = 1)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Food chain_oxford_newcastle Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity" ,alpha = 0.2, position = 'dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_line() +
  facet_wrap(origin ~ . , scales = 'free', nrow = 2)


# Caecal
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "caecal") %>%
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -percentage),
             y = percentage,
             group = 1)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Caecal Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity",fill = 'green4' ,alpha = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_line()

# Carcass
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "carcass") %>%
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -percentage),
             y = percentage,
             group = 1)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Carcass Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity",fill = 'green4' ,alpha = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_line()

# Food
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "food") %>%
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -percentage),
             y = percentage,
             group = 1)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Food Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity",fill = 'green4' ,alpha = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_line()

# Human Feces
summary %>% 
  group_by(clonal_complex, origin) %>%
  mutate(ordering_val = sum(percentage)) %>% 
  filter(origin == "humanfeces") %>%
  filter(position_86 == 'i') %>%
  ggplot(aes(x = reorder(clonal_complex, -percentage),
             y = percentage,
             group = 1)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Human Feces Clonal Complex Community Dynamics") +
  geom_bar(stat = "identity",fill = 'green4' ,alpha = 0.2) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +geom_line()



#Creating graph for each clonal compplex across the foodchain
summary_source_clonal[]
