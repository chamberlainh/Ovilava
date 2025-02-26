# PCA for Ovilava
## Author: Heather Chamberlain
## Date: February 2025

# Set working directory  
setwd("/lisc/scratch/anthropology/Pinhasi_group/Ovilava/PCA")

# Load libraries
library("dplyr") # for dataframe management
library("viridis") # colours
library("ggplot2") # plotting


# Load PCA .evec file
pca_data <- read.table("MergedOvilava30k_pcaR_evec_renamed.csv", header = TRUE, sep = "\t")

# Load population files
ovilava <- read.table("cleaned_my_list.txt", header = FALSE, stringsAsFactors = FALSE)
published <- read.table("pub_list.txt", header = FALSE, stringsAsFactors = FALSE)
modern <- read.table("modern_list_cleaned.txt", header = FALSE, stringsAsFactors = FALSE)


# Prepare pca_data with pop files
  
pca_data$Population <- ifelse(pca_data$ID %in% ovilava$ID, "Ovilava",
                              ifelse(pca_data$ID %in% published$ID, "Published",
                                     ifelse(pca_data$ID %in% modern$ID, "Modern", "Unknown")))



# Plot

ggplot(pca_data, aes(x = PC2, y = PC1, color = Population)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")  # Removes the legend

