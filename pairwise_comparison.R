library(phytools)
library(Rcpp)
library(ape)

test_aligned_fasta <- read.newik(file = "~/Documents/OneDrive - Nexus365/PhD/Campy_Analysis_ALL/Data/allele1355_1990_2018/CAMP0950_subset1000.fasta-gtr_cat.nwk")


test <- read.tree(file = "CAMP0950_subset1000.fasta-gtr_cat.nwk", text = NULL, tree.names = NULL, skip = 0,
          comment.char = "", keep.multi = FALSE)

pairwise_distance_tips <- cophenetic(test)
heatmap(pairwise_distance_tips,scale = "none")
