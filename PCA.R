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
pca_data$Colour[pca_data$Ancient_Site == "Ovilava"] <- "#F89441"  # Green from viridis
pca_data$Colour[pca_data$Ancient_Site == "Austria_Ovilava"] <-"#5DC863FF"  # Different color
pca_data$Colour[pca_data$Broad_Populations == "Modern"] <- "red"




# Define transparency levels using alpha
pca_data$Alpha <- "Other"  # Default transparency for all sites
pca_data$Alpha[pca_data$Ancient_Site == "Ovilava"] <- "Ovilava"
pca_data$Alpha[pca_data$Broad_Populations == "Modern"] <- "Modern"

# Convert to factor to ensure mapping works in ggplot
pca_data$Alpha <- factor(pca_data$Alpha, levels = c("Ovilava", "Modern", "Other"))





## Plot

# This is almost perfect, BUT I noticed Modern is being called Ancient
ggplot() +
  # Plot Modern as unfilled gray circles with transparency 0.1
  geom_point(data = subset(pca_data, Ancient_Site == "Modern"), 
             aes(x = PC2, y = PC1, color = Ancient_Site), alpha = 0.1, size = 3) +
  # Plot Ancient Sites with transparency 0.2
  geom_point(data = subset(pca_data, Ancient_Site != "Modern" & Ancient_Site != "Ovilava"), 
             aes(x = PC2, y = PC1, color = Ancient_Site), alpha = 0.2, size = 3) +
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Ancient_Site == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Ancient_Site), alpha = 1, size = 3) +
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Ancient_Site),
    name = "Ancient Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase main title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)# Increase legend text size
  ) +
  labs(
    title = "PCA of Ovilava Individuals with Published Ancient and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )





ggplot(pca_data, aes(x = PC2, y = PC1, color = Ancient_Site, alpha = Alpha)) +
  geom_point(size = 3) +
  scale_alpha_manual(
    values = c("Ovilava" = 1, "Modern" = 0.1, "Other" = 0.2),
    guide = "none"  # This removes the Population Type legend
  ) +
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Ancient_Site),
    name = "Ancient Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase main title size
    axis.title = element_text(size = 15)  # Increase axis title size
  ) +
  labs(
    title = "PCA of Ovilava Individuals with Published Ancient and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )





# Plot All with Only Ancient Site Legend NOT MODERN LEGEND
ggplot(pca_data, aes(x = PC2, y = PC1, color = Ancient_Site, alpha = Alpha)) +
  geom_point() +
  scale_alpha_manual(
    values = c("Ovilava" = 1, "Modern" = 0.1, "Other" = 0.2),
    guide = "none"  # This removes the Population Type legend
  ) +
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Ancient_Site),
    name = "Ancient Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()  # Remove grid
  ) +
  labs(
    title = "PCA of Ovilava Individuals with Published Ancient and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )



# Plot Modern, Ovilava, All Ancient with Transparency and Legend
ggplot(pca_data, aes(x = PC2, y = PC1, color = Ancient_Site, alpha = Alpha)) +
  geom_point() +
  scale_alpha_manual(
    values = c("Ovilava" = 1),
  ) +
  scale_alpha_manual(
    values = c("Modern" = 0.1),
    name = "Modern Population v62_1240K"
  ) +
  scale_alpha_manual(
    values = c("Other" = 0.2),
  ) +
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Ancient_Site),
    name = "Ancient Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank()  # Remove grid
  ) +
  labs(
    title = "PCA of Ovilava Individuals with Published Ancient and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )




# Plot Modern, Ovilava, All Ancient
ggplot(pca_data, aes(x = PC2, y = PC1, color = Colour)) +
  geom_point() +
  theme_minimal() +
  scale_color_identity() +  # Uses pre-defined colors
  theme(legend.position = "none") +  
  labs(x = paste0("PC2 (", pc2_var, "%)"),
       y = paste0("PC1 (", pc1_var, "%)"))


ggplot(pca_data, aes(x = PC2, y = PC1, color = Colour, alpha = Ancient_Site)) +
  geom_point() +
  scale_alpha_manual(values = c("Ovilava" = 1, "Austria_Ovilava" = 0.8, "Other" = 0.5)) +
  scale_color_identity() +
  theme_minimal() +
  theme(legend.position = "none")


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

