setwd("~/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data")

#Standard dataframe that has the phenotype and all other
pheno_df <- read.csv(file = 'phenotype_2010_2018.csv')
#df of interest of specific categorical values
pheno_df_chisquare <- subset(pheno_df, select = c(id,country,region,year,month,age_yr,sex,ciprofloxacin_phenotype))
#pheno and sex
pheno_df_sex <- subset(pheno_df,select = c(sex,ciprofloxacin_phenotype))
#pheno and month
pheno_df_month <- subset(pheno_df,select = c(month,ciprofloxacin_phenotype))
#pheno and year
pheno_df_year <- subset(pheno_df,select = c(year,ciprofloxacin_phenotype))


#Making the summary() into table format to do chi-square test
pheno_df_sex <- as.data.frame(table(pheno_df_sex), responseName="nums")
pheno_df_month <- as.data.frame(table(pheno_df_month), responseName="nums")
pheno_df_year <- as.data.frame(table(pheno_df_year), responseName="nums")



##########################################################ARC and Machine learning Prep##############

#This is a df for ID and coreMLST and phenotype which is for the machine learning
pheno_df <-  subset(pheno_df, select = -c(country,age_yr,source,aminoglycoside_genotypes_1,Anon_patient_ID,erythromycin_phenotype,
                                          macrolide_genotypes_1,fluoroquinolone_genotypes_1,fluoroquinolone_genotypes_2,
                                          macrolide_genotypes_1,macrolide_genotypes_2,macrolide_genotypes_2,tetracycline_genotypes_1,
tetracycline_genotypes_2,tetracycline_phenotype,region,year,month,sex))
#Saving this df for machine learning
write.csv(pheno_df, file = 'phenotype_coreMLST_df.csv')

#Saving df for id and phenotype only for ARC analysis taking only these samples tahn all 16815 samples
pheno_df_id_sr <- subset(pheno_df,select = c(id,ciprofloxacin_phenotype))
write.csv(pheno_df_id_sr,file = 'phenotype_id_only.csv')
#Choosing just the ID's that have phenotype information
pheno_df_idonlt <- subset(pheno_df_id_sr, select = (id))
write.table(pheno_df_idonlt,file = 'id_only_phenotypeNone.csv',col.names = FALSE,row.names = FALSE)
