library(seqinr)
library(stringr)
library(dplyr)
library(rowr)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(scales)
library(ggpubr)

################### This dil contains the caecal carcass and human data but not executed well. It works but not for the end result ########

#This graph creates figure between the caecal, carcass and human frequency on the point mutation that is available. 
#g shows a bar chart that shows the amount of i and t and a found in caecal, carcass and human 

#Do not change it to read.delim because it reads it wrong!These code reads the file and places it inside
caecal <- read.fasta("~/Documents/OneDrive - Nexus365/PhD/R/chicken_caecal.fas.txt")
carcass <- read.fasta("~/Documents/OneDrive - Nexus365/PhD/R/chicken_carcass.fas.txt")
newcastleyear1_human <- read.fasta("~/Documents/OneDrive - Nexus365/PhD/R/Newcastle/Newcastle1styearallighned.fas.txt")
food_retail <- read.fasta("~/Documents/OneDrive - Nexus365/PhD/R/retailsurveyFSA.fas.txt")

caecal_st_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/chicken_caecal.csv")
carcass_st_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/chicken_carcass.csv")
human_st_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/Newcastle/Newcastle1styearaligned.csv")
food_st_raw <- read.csv("~/Documents/OneDrive - Nexus365/PhD/R/retailSurveyFAA.csv")
#Uploading files with ST and clonal complex types

caecal_st <-caecal_st_raw[,c("id","species","source","comments","ST..MLST.","clonal_complex..MLST.")]
carcass_st <-carcass_st_raw[,c("id","species","source","comments","ST..MLST.","clonal_complex..MLST.")]
human_st <-human_st_raw[,c("id","species","source","comments","ST..MLST.","clonal_complex..MLST.")]
food_st <-food_st_raw[,c("id","species","source","comments","ST..MLST.","clonal_complex..MLST.")]

#Pick the first 5 numbers and make that to be named 'names'
names(caecal) <- str_sub(names(caecal), 1, 5)
names(carcass) <- str_sub(names(carcass), 1, 5)
names(newcastleyear1_human) <- str_sub(names(newcastleyear1_human), 1, 5)
names(food_retail) <- str_sub(names(food_retail), 1, 5)

#This means check the 86th position out of the values , [[ lets u access the element in the list
gyrA86_caecal <- unlist(lapply(caecal, "[[", 86))
gyrA86_carcass <- unlist(lapply(carcass, "[[", 86))
gyrA86_human <- unlist(lapply(newcastleyear1_human, "[[", 86))
gyrA86_food_retail <- unlist(lapply(food_retail, "[[", 86))

#Create a data frame which one of the column shows the isolate names from gyrA86_caecal and another column with what position 86 is
#It also adds one column with same value
amino_86_caecal <- data.frame(id = names(gyrA86_caecal), #By putting comma we move to the second column assighnment
                       Position_86 = gyrA86_caecal, type = 'caecal')
amino_86_carcass <- data.frame(id = names(gyrA86_carcass), 
                       Position_86 = gyrA86_carcass,type = 'carcass')
amino_86_human <- data.frame(id = names(gyrA86_human), 
                               Position_86 = gyrA86_human,type = 'human')
amino_86_food_retail <- data.frame(id = names(gyrA86_food_retail), 
                             Position_86 = gyrA86_food_retail,type = 'food_')

#Merging twice using the mege comment for all 3 dataframes. 

caecal_merge <- merge(caecal_st,amino_86_caecal,by = "id")

human_merge <- merge(human_st,amino_86_human,by = "id")
      
carcass_merge <- merge(carcass_st,amino_86_carcass,by = "id")

################################################################################################################################################################################################
chicken_comparison <- bind_rows(caecal_merge, carcass_merge)
human_chicken_comparison <- bind_rows(caecal_merge, carcass_merge,human_merge)
human_chicken_comparison[human_chicken_comparison$Position_86 == 'i',]

summary = aggregate(data = human_chicken_comparison,
                    id ~ type + clonal_complex..MLST. + Position_86,
                    length)
total_counts_by_type = data.frame(table(human_chicken_comparison$type))
colnames(total_counts_by_type) = c('type', 'total')
colnames(summary) = c('type', 'clonal_complex','position_86','total_id')
summary = merge(total_counts_by_type,summary)
summary$prop = 100 * summary$total_id / summary$total


#g shows a bar chart that shows the amount of i and t and a found in caecal, carcass and human 
g = ggplot(summary,
       aes(x = clonal_complex,
           fill = position_86,
           y = prop)) +
  labs(y = "Percentage by source",
       x = "Clonal complex",
       subtitle = "Subtitle") +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~type) +
  theme_pubr(legend = "bottom") 
g

write.csv(chicken_comparison,file = "~/Documents/OneDrive - Nexus365/chicken/chicken_comparison.csv")
write.csv(human_chicken_comparison,file = "~/Documents/OneDrive - Nexus365/chicken/human_chicken_comparison.csv")
write.csv(amino_86_caecal,file = "~/Documents/OneDrive - Nexus365/chicken/gyrA86_caecal.csv")
write.csv(amino_86_carcass,file = "~/Documents/OneDrive - Nexus365/chicken/gyrA86_carcass.csv")
# Create a file that has one row that states the isolate name and another column just have the 86th position. 



############################################################################################################################

# This section is looking exlcusively at a subset of chicken_comparison dataset in which the position 86 is positive

# Subset all twos in which Position_86 = i:
Pos_86_i_df <- subset(chicken_comparison, chicken_comparison$Position_86 == "i")
nrow(Pos_86_i_df)

Pos_86_i_human_chicken_df <- subset(human_chicken_comparison, human_chicken_comparison$Position_86 == "i")
nrow(Pos_86_i_human_chicken_df)

# Check how many unique values for clonal complex are in Pos_86_i_df
res_clonal_complexes <- unique(Pos_86_i_df$clonal_complex..MLST.)  # just chicken
human_chicken_res_clonal_complexes <- unique(Pos_86_i_human_chicken_df$clonal_complex..MLST.)

Pos_86_i_human_chicken_df
res_clonal_complexes
human_chicken_res_clonal_complexes
length(unique(Pos_86_i_df$clonal_complex..MLST.))
length(unique(Pos_86_i_human_chicken_df$clonal_complex..MLST.))

# Further subset the data into caecal,carcus and human sources:

caecal_sources_resistant <- subset(Pos_86_i_human_chicken_df, Pos_86_i_human_chicken_df$type == "caecal")
carcas_sources_resistant <- subset(Pos_86_i_human_chicken_df, Pos_86_i_human_chicken_df$type == "carcass")
human_sources_resistant <- subset(Pos_86_i_human_chicken_df, Pos_86_i_human_chicken_df$type == "human")

# Create a unified table of clonal complexes for caecal and carcass datasets:
caecal_complexes <- unique(caecal_sources_resistant$clonal_complex..MLST.)
carcass_complexes <- unique(carcas_sources_resistant$clonal_complex..MLST.)
human_complexes <-unique(human_sources_resistant$clonal_complex..MLST.)

# Create dataframes for the frequencies of the different clonal complexes

chicken_human_Clonal_Complex_df <- as.data.frame(table(Pos_86_i_human_chicken_df$clonal_complex..MLST.))
Caecal_Clonal_Complex_df <- as.data.frame(table(caecal_sources_resistant$clonal_complex..MLST.))
Carcass_Clonal_Complex_df <- as.data.frame(table(carcas_sources_resistant$clonal_complex..MLST.))
Human_Clonal_Complex_df <- as.data.frame(table(human_sources_resistant$clonal_complex..MLST.))


# Rename the columns in the clonal complex / frequency dataframe

colnames(chicken_human_Clonal_Complex_df) <- c("Clonal_Complex", "Frequency")
colnames(Caecal_Clonal_Complex_df) <- c("Clonal_Complex", "Frequency")
colnames(Carcass_Clonal_Complex_df) <- c("Clonal_Complex", "Frequency")
colnames(Human_Clonal_Complex_df) <- c("Clonal_Complex", "Frequency")

# Combine the caecal and carcass dataframes and add NAs to missing rows / complexes
Combined_Caecal_Carcass_df <- merge(Caecal_Clonal_Complex_df, Carcass_Clonal_Complex_df, by = "Clonal_Complex", all = TRUE)
colnames(Combined_Caecal_Carcass_df) <- c("Clonal_Complex", "Caecel_Freq", "Carcass_Freq")
nrow(Combined_Caecal_Carcass_df)

Combined_Caecal_Carcass_df <- merge(Caecal_Clonal_Complex_df, Carcass_Clonal_Complex_df, by = "Clonal_Complex", all = TRUE)
Combined_chicken_human_df <- merge(Combined_Caecal_Carcass_df,  Human_Clonal_Complex_df, by = "Clonal_Complex", all = TRUE)
colnames(Combined_chicken_human_df ) <- c("Clonal_Complex", "Caecel_Freq", "Carcass_Freq","Human_Freq")
nrow(Combined_chicken_human_df)

#Will try this next time: df5 = merge(merge(df1mdf2),merge(df3,df4))

# Convert NAs to 0s in the dataframe:
Combined_Caecal_Carcass_df <- Combined_Caecal_Carcass_df
Combined_Caecal_Carcass_df[is.na(Combined_Caecal_Carcass_df)] <- 0
Combined_Caecal_Carcass_df

Combined_chicken_human_df[is.na(Combined_chicken_human_df)] <- 0
Combined_chicken_human_df

Combined_Caecal_Carcass_df$Caecel_Freq <- Combined_Caecal_Carcass_df$Caecel_Freq/sum(Combined_Caecal_Carcass_df$Caecel_Freq)
Combined_Caecal_Carcass_df$Carcass_Freq <- Combined_Caecal_Carcass_df$Carcass_Freq/sum(Combined_Caecal_Carcass_df$Carcass_Freq)

Combined_chicken_human_df$Caecel_Freq <- Combined_chicken_human_df$Caecel_Freq/sum(Combined_chicken_human_df$Caecel_Freq)
Combined_chicken_human_df$Carcass_Freq <- Combined_chicken_human_df$Carcass_Freq/sum(Combined_chicken_human_df$Carcass_Freq)
Combined_chicken_human_df$Human_Freq <- Combined_chicken_human_df$Human_Freq/sum(Combined_chicken_human_df$Human_Freq)
# Try and create a barplot:
barplot(c(Combined_Caecal_Carcass_df$Caecel_Freq, Combined_Caecal_Carcass_df$Carcass_Freq), beside = T)
barplot(Combined_Caecal_Carcass_df, beside = TRUE)

barplot(c(Combined_chicken_human_df$Caecel_Freq, Combined_chicken_human_df$Carcass_Freq), beside = T)
barplot(Combined_chicken_human_df, beside = TRUE)

# Use the melt function to re-organise dataframe into a ggplot-friendly format:
head(melt(Combined_Caecal_Carcass_df))
Amended_df <- melt(Combined_Caecal_Carcass_df)
colnames(Amended_df) <- c("Clonal_Complex", "Source", "Freq")
head(Amended_df)

head(melt(Combined_chicken_human_df))
Amended_chicken_human_df <- melt(Combined_chicken_human_df)
colnames(Amended_chicken_human_df) <- c("Clonal_Complex", "Source", "Freq")
head(Amended_chicken_human_df)

#o is the graph that shows the three bars frequency of the point mutation in carcass human and caecal 


o <- ggplot(data=melt(Combined_chicken_human_df), aes(x= Clonal_Complex, y = value, fill= variable)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  xlab("Clonal Complex") + 
  ylab("Frequency")+ 
  scale_fill_brewer(palette="Set1") + 
  theme(axis.text.x = element_text(angle = 80))+ 
  ggtitle("2016: Fluoroquinolone Resistant(GyrA-Thr86Ile) across food chain and its Clonal Complexes") + 
  coord_flip() + 
  ylim(0,0.3) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10), plot.title = element_text(size = 9, face = "bold"))

o
ggsave
print(p+scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 80))+ 
        ggtitle("Fluoroquinolone Resistant (GyrA-Thr86Ile) in Carcass (PHE) and Caecal poo (APHA) Clonal Complexes") + coord_flip() + ylim(0,0.3) + theme_bw() + theme(axis.text = element_text(size = 10), 
                                                                                                                                                                               axis.title = element_text(size = 10),
                                                                                                                                                                               plot.title = element_text(size = 9, face = "bold")))
final = print(o+scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 80))+ 
        ggtitle("2016: Fluoroquinolone Resistant(GyrA-Thr86Ile) across food chain and its Clonal Complexes") + coord_flip() + ylim(0,0.3) + theme_bw() + theme(axis.text = element_text(size = 10),axis.title = element_text(size = 10),
                                                                                                                                                                      plot.title = element_text(size = 9, face = "bold")))


###########################################################################################################################


