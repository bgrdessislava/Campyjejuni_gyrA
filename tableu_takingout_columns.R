library(tidyverse)
setwd("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Code")

raw_data <- read.csv("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/gyrA_metadata_combined_1990_2018.csv")
raw_data_rmlst_mlst <- read.table("/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/tableu_df_rmlst_mlst.txt", sep="\t",header = TRUE)

df_tableu_mlst_clonal <- raw_data_rmlst_mlst %>% select(id,ST..MLST.,clonal_complex..MLST.,
                                                        rST..Ribosomal.MLST.,aspA,gltA,pgm,uncA)

##Need to change the id to ID DONE
##Then combine the dataframe together and save that then send it  to margaret


df_tableu_mlst_clonal = df_tableu_mlst_clonal %>% 
  rename(ID = id)

df_tableu <- raw_data %>% select(ID,isolate,region,year,month,age_yr,age_mth,source,
aminoglycoside_genotypes_1,Anon_patient_ID,ciprofloxacin_phenotype,erythromycin_phenotype,
fluoroquinolone_genotypes_2,macrolide_genotypes_1,macrolide_genotypes_2,tetracycline_genotypes_1,tetracycline_genotypes_2,
tetracycline_phenotype,cgST..C..jejuni...C..coli.cgMLST.v1.0.,Cjc_cgc_200,Cjc_cgc_100,Cjc_cgc_50,Cjc_cgc_25
,Cjc_cgc_10,Cjc_cgc_5,gyrA_base)

total <- merge(df_tableu_mlst_clonal,df_tableu,all=TRUE,by="ID")

write.csv(total,file="/Users/user/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/tableu_df.csv")
