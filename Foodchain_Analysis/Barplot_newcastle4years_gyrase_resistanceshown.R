setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")

library(dplyr)
library(ggplot2)

#This code could be useful to track again what kind of genotype we currently have. 
# #This file contains all the 4 year projects from Newcastle and its resistant mutation found in the isolates. 
# I was also trying to see whether this mutation is found in all the clonal complexes or just in some of them. 
# Results has shown that almost all the clonal complex have this point mutation and some of the clonal complex are especially focused on campylobacter coli. 


NewcastleR_S_Both <- read.delim("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/NewcastleR_S_Both.txt")
cleaned <-NewcastleR_S_Both[,c("id","isolate","year","month","sex","species","ciprofloxacin_phenotype","fluoroquinolone_genotypes_1","ST..MLST.","clonal_complex..MLST.")]

resistance <- cleaned[(cleaned$ciprofloxacin_phenotype=='R'),]

#counting resistance in 2016
count(resistance$year == "2016")
#counting how many females had the resistance
length(resistance$sex=='female')
#counting how many of them have threonine to isoleucine mutation
count(resistance$fluoroquinolone_genotypes_1 == "gyrA_CJ[86:T-I]")
#which shows you both TRUE and FALSE arguments
length(which(resistance$fluoroquinolone_genotypes_1=='gyrA_CJ[86:T-I]'))      

#Other type of mutations that come up are listed
fluoro_resistant = aggregate(resistance,
                by = list(resistance$fluoroquinolone_genotypes_1),
                FUN = length)

Months = aggregate(resistance,
                             by = list(resistance$month),
                             FUN = length)
  

#####What is going on here?
mutation_months <- resistance %>%
  group_by(fluoroquinolone_genotypes_1,clonal_complex..MLST.,year) %>% 
  summarise(S = n()) 

mutation_months %>% 
  ggplot(aes(x = fluoroquinolone_genotypes_1, y = S, fill = as.factor(year)))+
  geom_bar(stat = "identity", position = position_dodge())

clonal_complex_resistance_species <- resistance %>%
  group_by(fluoroquinolone_genotypes_1,sex,clonal_complex..MLST.,species) %>% 
  summarise(P = n()) 

clonal_complex_resistance_species %>% 
  ggplot(aes(x = clonal_complex..MLST., y = P, fill = as.factor(species)))+
  geom_bar(stat = "identity", position = position_dodge())

