library(seqinr)
library(stringr)

#This file has a code that finds the 86th position in gyrase from newcastle 1st year project
#This code only works if we have an aligned file and if the ID is equal to 5 digits
#We have made another file that uses the whole information and extracts all meta data so not as useful. 

setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")
#Do not change it to read.delim because it reads it wrong!
firstyear_amino <- read.delim("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/Newcastleyear1_R_ciprofloxcin_aligned_translated.txt",header = FALSE)
firsteyar_amino_fasta_new <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/Ciprofloxcin_non_aligned_newcastleyear1.fas.txt")

firstyear_amino_test <- read.fasta("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/Newcastleyear1_R_ciprofloxcin_aligned_translated.txt")

names(firstyear_amino_test) <- str_sub(names(firstyear_amino_test), 1, 5)
#Pick the first 5 numbers and make that to be named 'names'
names(firsteyar_amino_fasta_new) <- str_sub(names(firsteyar_amino_fasta_new), 1, 5)

the_positions <- unlist(lapply(firstyear_amino_test, "[[", 86))
#This means check the 86th position out of the values
the_positions_new <- unlist(lapply(firsteyar_amino_fasta_new, "[[", 86))

amino_86 <- data.frame(Isolates = names(firstyear_amino_test), 
                   Position_86 = the_positions)
amino_86 <- data.frame(Isolates = names(firsteyar_amino_fasta_new), 
                       Position_86 = the_positions_new)

write.csv(amino_86,file = "/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/amino_86_firstyear_Newcastle.csv")

