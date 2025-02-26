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






# Define all eigenvalues from the MergedOvilava30k_trim..evec header
eigvals <- c(6.876, 3.297, 2.512, 2.327, 2.087, 2.036, 1.996, 1.983, 1.969, 1.954)

# Calculate percentage of variance explained
percent_var <- (eigvals / sum(eigvals)) * 100

# Extract percentages for PC1 and PC2
pc1_var <- round(percent_var[1], 2)
pc2_var <- round(percent_var[2], 2)





# Add a Broad_Populations column to pca_data
pca_data$Broad_Populations <- ifelse(pca_data$ID %in% modern$V1, "Modern", "Ancient")

# Reassign "Ovilava" where applicable
pca_data$Population[pca_data$ID == "I35407.TW"] <- "Ovilava"
pca_data$Broad_Populations <- ifelse(pca_data$ID %in% ovilava$V1, "Ovilava", pca_data$Broad_Populations)
pca_data$Broad_Populations[pca_data$Population == "Ovilava"] <- "Ovilava"

unique_populations_ancient <-unique(pca_data$Population[pca_data$Broad_Populations == "Ancient"])
pca_data$Ancient_Site <- sub("_.*", "", pca_data$Population)

coded_populations_ancient <- unique(pca_data$Ancient_Site)
subset(pca_data, grepl("Ovilava", Population))
pca_data$Ancient_Site[pca_data$Ancient_Site == "Ovilava_Published"] <- "Austria_Ovilava"


## Color Assignments

# Define population colors
pca_data$Colour <- NA  # Initialize column

# Get unique Ancient_Site values, excluding Ovilava
unique_sites <- unique(pca_data$Ancient_Site[pca_data$Ancient_Site != "Ovilava"])

# Generate a viridis (option D) color palette
site_colors <- setNames(viridis(length(unique_sites), option = "D"), unique_sites)



# Assign colors based on Ancient_Site
pca_data$Colour <- site_colors[as.character(pca_data$Ancient_Site)]

# Assign different colors to Ovilava and Austria_Ovilava
pca_data$Colour[pca_data$Ancient_Site == "Ovilava"] <- "#5DC863FF"  # Green from viridis
pca_data$Colour[pca_data$Ancient_Site == "Austria_Ovilava"] <- "#F89441"  # Different color
pca_data$Colour[pca_data$Broad_Populations == "Modern"] <- "grey50"




## Plot
# Plot Modern, Ovilava, All Ancient
ggplot(pca_data, aes(x = PC2, y = PC1, color = Colour)) +
  geom_point() +
  theme_minimal() +
  scale_color_identity() +  # Uses pre-defined colors
  theme(legend.position = "none") +  
  labs(x = paste0("PC2 (", pc2_var, "%)"),
       y = paste0("PC1 (", pc1_var, "%)"))



# Create PCA plot MODERN v ANCIENT
ggplot(pca_data, aes(x = PC2, y = PC1, color = Broad_Populations)) +
  # Plot points
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("Modern" = "grey", "Ancient" = "blue")) +  # Adjust colors as needed
  theme(legend.position = "none") +  
  labs(x = paste0("PC2 (", pc2_var, "%)"),
       y = paste0("PC1 (", pc1_var, "%)"))




# Create plot with percentage labels PERCENTS
ggplot(pca_data, aes(x = PC2, y = PC1, color = Population)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none") +  # Removes the legend
  labs(x = paste0("PC2 (", pc2_var, "%)"),
       y = paste0("PC1 (", pc1_var, "%)"))



# Plot SIMPLE

ggplot(pca_data, aes(x = PC2, y = PC1, color = Population)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "none")  # Removes the legend

