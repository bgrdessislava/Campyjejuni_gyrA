library(seqinr)
library(stringr)
library(dplyr)
library(rowr)
library(ggplot2)
library(reshape2)
library(Hmisc)

#Experimented with using caecal and carcass data to see which clonal complex has been shown the most
#Images are interesting but they have not been standarlised to be able to be used to show the significance in changes

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

#Do not change it to read.delim because it reads it wrong!
caecal <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/chicken_caecal.fas.txt")
carcass <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/chicken_carcass.fas.txt")

caecal_st_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/chicken_caecal.csv")
carcass_st_raw <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/chicken_carcass.csv")
human_st_raw <- read.csv('/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/amino_86_firstyear_Newcastle.csv')
#Uploading files with ST and clonal complex types

caecal_st <-caecal_st_raw[,c("id","species","comments","ST..MLST.","clonal_complex..MLST.")]
carcass_st <-carcass_st_raw[,c("id","species","comments","ST..MLST.","clonal_complex..MLST.")]

caecal_st$source <- rep('caecal',nrow(caecal_st)) 
carcass_st$source <- rep('carcass',nrow(carcass_st)) 
#add a column named caecal or carcass

names(caecal) <- str_sub(names(caecal), 1, 5)
names(carcass) <- str_sub(names(carcass), 1, 5)
#Pick the first 5 numbers and make that to be named 'names'

gyrA86_caecal <- unlist(lapply(caecal, "[[", 86))
gyrA86_carcass <- unlist(lapply(carcass, "[[", 86))
#This means check the 86th position out of the values , [[ lets u access the element in the list

amino_86_caecal <- data.frame(ID = names(gyrA86_caecal), 
                       Position_86 = gyrA86_caecal, type = 'caecal')
amino_86_carcass <- data.frame(Isolates = names(gyrA86_carcass), 
                       Position_86 = gyrA86_carcass,type = 'carcass')
#Create a data frame which one of the column shows the isolate names from gyrA86_caecal and another column with what position 86 is
#It also adds one column with same value

names(caecal_st)[names(caecal_st) == "id"] <- "Isolates"
# If caecal_st = isolate than change the header to 'Isolates' This function allows the code to find the code with 'isolate' rather than showing the location
names(amino_86_caecal)[names(amino_86_caecal) == "ID"] <- "Isolates"

caecal_merge <- merge(caecal_st,amino_86_caecal,by = "Isolates")

names(carcass_st)[names(carcass_st) == "id"] <- "Isolates"
carcass_merge <- merge(carcass_st,amino_86_carcass,by = "Isolates")

################################################################################################################################################################################################
#Another way to combine the columns since Rob said the method merge is not always th ebest
#by writing as.integer -> you make the factor number to be changed to be an integer
#caecal_st$id <- as.integer(caecal_st$id)
#amino_86_caecal$Isolates <- as.integer(as.character(amino_86_caecal$ID))

#binding_caecal <- left_join(amino_86_caecal, caecal_st, by = c("Isolates" = "ID"))
#binding_caecal <- cbind(amino_86_caecal, caecal_st) -> this will only work if every column is correct and it will not always work
#carcass_st$id <- as.integer(carcass_st$id)
#amino_86_carcass$Isolates <- as.integer(as.character(amino_86_carcass$Isolates))
#binding_carcass <- cbind(amino_86_carcass, carcass_st)
#binding_carcass <- left_join(amino_86_carcass, carcass_st, by = c("Isolates" = "id"))

chicken_comparison <- bind_rows(caecal_merge, carcass_merge)
human_chicken_comparison <- bind_rows(caecal_merge, carcass_merge,amino_86)

write.csv(chicken_comparison,file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/chicken_comparison.csv")

write.csv(amino_86_caecal,file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/gyrA86_caecal.csv")
write.csv(amino_86_carcass,file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/gyrA86_carcass.csv")
# Create a file that has one row that states the isolate name and another column just have the 86th position. 



############################################################################################################################

# This section is looking exlcusively at a subset of chicken_comparison dataset in which the position 86 is positive
# for the resistance allele / strain

# Subset all twos in which Position_86 = i:
Pos_86_i_df <- subset(chicken_comparison, chicken_comparison$Position_86 == "i")
nrow(Pos_86_i_df)

# Check how many unique values for clonal complex are in Pos_86_i_df
res_clonal_complexes <- unique(Pos_86_i_df$clonal_complex..MLST.)
res_clonal_complexes
length(unique(Pos_86_i_df$clonal_complex..MLST.))

# Further subset the data into caecal and carcus sources:
caecal_sources <- subset(Pos_86_i_df, Pos_86_i_df$source == "caecal")
carcas_sources <- subset(Pos_86_i_df, Pos_86_i_df$source == "carcass")

# View the independent dataframes:
head(caecal_sources); length(unique(caecal_sources$clonal_complex..MLST.))
head(carcas_sources); length(unique(carcas_sources$clonal_complex..MLST.))

# Create a histogram to check the frequency of each clonal complex for each of the sources (carcass and caecal)
# Barplot / frequency plot for caecal sources
barplot(table(caecal_sources$clonal_complex..MLST.), col=rainbow(20), las=2, cex.names=.6)
table(caecal_sources$clonal_complex..MLST.)

# Barplot / frequency plot for carcass sources
barplot(table(carcas_sources$clonal_complex..MLST.), col=rainbow(20), las=2, cex.names=.6)

# Create a unified table of clonal complexes for caecal and carcass datasets:
caecal_complexes <- unique(caecal_sources$clonal_complex..MLST.)
carcass_complexes <- unique(carcas_sources$clonal_complex..MLST.)

# Create dataframes for the frequencies of the different clonal complexes
Total_Clonal_Complex_df <- as.data.frame(table(Pos_86_i_df$clonal_complex..MLST.))
Caecal_Clonal_Complex_df <- as.data.frame(table(caecal_sources$clonal_complex..MLST.))
Carcass_Clonal_Complex_df <- as.data.frame(table(carcas_sources$clonal_complex..MLST.))



# Rename the columns in the clonal complex / frequency dataframes
colnames(Total_Clonal_Complex_df) <- c("Clonal_Complex", "Frequency")
colnames(Caecal_Clonal_Complex_df) <- c("Clonal_Complex", "Frequency")
colnames(Carcass_Clonal_Complex_df) <- c("Clonal_Complex", "Frequency")

# Combine the caecal and carcass dataframes and add NAs to missing rows / complexes
Combined_Caecal_Carcass_df <- merge(Caecal_Clonal_Complex_df, Carcass_Clonal_Complex_df, by = "Clonal_Complex", all = TRUE)
colnames(Combined_Caecal_Carcass_df) <- c("Clonal_Complex", "Caecel_Freq", "Carcass_Freq")
nrow(Combined_Caecal_Carcass_df)



# Convert NAs to 0s in the dataframe:
Combined_Caecal_Carcass_df <- Combined_Caecal_Carcass_df
Combined_Caecal_Carcass_df[is.na(Combined_Caecal_Carcass_df)] <- 0
Combined_Caecal_Carcass_df

Combined_Caecal_Carcass_df$Caecel_Freq <- Combined_Caecal_Carcass_df$Caecel_Freq/sum(Combined_Caecal_Carcass_df$Caecel_Freq)
Combined_Caecal_Carcass_df$Carcass_Freq <- Combined_Caecal_Carcass_df$Carcass_Freq/sum(Combined_Caecal_Carcass_df$Carcass_Freq)

# Try and create a barplot:
barplot(c(Combined_Caecal_Carcass_df$Caecel_Freq, Combined_Caecal_Carcass_df$Carcass_Freq), beside = T)
barplot(Combined_Caecal_Carcass_df, beside = TRUE)

# Use the melt function to re-organise dataframe into a ggplot-friendly format:
head(melt(Combined_Caecal_Carcass_df))
Amended_df <- melt(Combined_Caecal_Carcass_df)
colnames(Amended_df) <- c("Clonal_Complex", "Source", "Freq")
head(Amended_df)

# Bar graph, time on x-axis, color fill grouped by sex -- use position_dodge()
p <- ggplot(data=melt(Combined_Caecal_Carcass_df), aes(x= Clonal_Complex, y = value, fill= variable)) +
  geom_bar(stat="identity", position=position_dodge()) + xlab("Clonal Complex") + ylab("Frequency")




print(p+scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 80))+ 
        ggtitle("Fluoroquinolone Resistant (GyrA-Thr86Ile) in Carcass (PHE) and Caecal poo (APHA) Clonal Complexes") + coord_flip() + ylim(0,0.3) + theme_bw() + theme(axis.text = element_text(size = 10), 
                                                                                                                                                                               axis.title = element_text(size = 10),
                                                                                                                                                                               plot.title = element_text(size = 9, face = "bold")))


p
ggplot(data=melt(Amended_df), aes(x= Clonal_Complex, y = Freq, fill= Source)) +
  geom_bar(stat="identity", position=position_dodge()) + xlab("Clonal Complex") + ylab("Frequency")


############################################################################################################################