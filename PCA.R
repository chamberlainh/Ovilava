# PCA for Ovilava
## Author: Heather Chamberlain
## Date: February 2025

## Set working directory  
setwd("/lisc/scratch/anthropology/Pinhasi_group/Ovilava/PCA")

## Load libraries
library("dplyr") # for dataframe management
library("viridis") # colours
library("ggplot2") # plotting


## Load Files
# Load PCA .evec file
pca_data <- read.table("MergedOvilava30k_pcaR_evec_renamed.csv", header = TRUE, sep = "\t")

# Load population files
ovilava <- read.table("cleaned_my_list.txt", header = FALSE, stringsAsFactors = FALSE)
published <- read.table("pub_list_cleaned.txt", header = FALSE, stringsAsFactors = FALSE)
modern <- read.table("modern_list_cleaned.txt", header = FALSE, stringsAsFactors = FALSE)

## Data Wrangle
# Prepare pca_data with pop files
  

pca_data$Data_Source <- ifelse(pca_data$ID %in% modern$V1 & 
                                !(pca_data$ID %in% ovilava$V1) & 
                                !(pca_data$ID %in% published$V1), "Modern",
                              ifelse(pca_data$ID %in% ovilava$V1, "Ovilava",
                                     ifelse(pca_data$ID %in% published$V1, "Published", "Unknown")))



table(pca_data$Data_Source)

pca_data$Site <- NA  # Add Site column with NA values


pca_data$Site <- ifelse(pca_data$Data_Source == "Ovilava", "Ovilava", # Keep Ovilava the same
                        ifelse(pca_data$Data_Source == "Modern",
                               paste0(sub("_.*", "", pca_data$Population), "_Modern"), # Extract country name and add "Modern"
                               ifelse(pca_data$Data_Source == "Published",
                                      sub("_.*", "", pca_data$Population), # Extract country name only for rest of ancient
                                      pca_data$Site)))  # Retain existing values for other cases

# Check
# table(pca_data$Site, pca_data$Data_Source)


# Define all eigenvalues from the MergedOvilava30k_trim..evec header
eigvals <- c(6.876, 3.297, 2.512, 2.327, 2.087, 2.036, 1.996, 1.983, 1.969, 1.954)

# Calculate percentage of variance explained
percent_var <- (eigvals / sum(eigvals)) * 100

# Extract percentages for PC1 and PC2
pc1_var <- round(percent_var[1], 2)
pc2_var <- round(percent_var[2], 2)





## Color Assignments

# Get unique Published sites
published_sites <- unique(pca_data$Site[pca_data$Data_Source == "Published"])

# Generate viridis colors for Published sites
published_colors <- viridis(length(published_sites), option = "D")

# Create a named vector for site-to-color mapping
published_color_map <- setNames(published_colors, published_sites)

# Assign colors
pca_data$Colour <- ifelse(pca_data$Data_Source == "Ovilava", "#F89441",  # Ovilava as black
                          ifelse(pca_data$Data_Source == "Modern", "grey",  # Modern as gray
                                 ifelse(pca_data$Data_Source == "Published",
                                        published_color_map[pca_data$Site], NA)))  # Use mapped colors for Published sites


# Check if the assignment worked
#table(pca_data$Site, pca_data$Colour)
#table(pca_data$Site)



## Get sums for each population

# Count the number of samples for each population
#population_counts <- as.data.frame(table(pca_data$Data_Source))
#colnames(population_counts) <- c("Population", "Sum")

# Count the number of samples for each unique Published site
#published_counts <- as.data.frame(table(pca_data$Site[pca_data$Data_Source == "Published"]))
#colnames(published_counts) <- c("Population", "Sum")

# Remove the "Published" row from the main count
#population_counts <- subset(population_counts, Population != "Published")

# Combine Ovilava, Modern, and detailed Published sites
#final_counts <- rbind(population_counts, published_counts)


# Save as a tab-delimited text file
#write.table(final_counts, "population_summary.txt", sep = "\t", row.names = FALSE, quote = FALSE)





## Plot

# WORKS
# Replace NA in Site with "Modern"
pca_data$Site[is.na(pca_data$Site)] <- "Modern"

# Convert Site to a factor to preserve ordering
pca_data$Site <- factor(pca_data$Site)

# Plot BEST
ggplot() +
  # Plot Modern as unfilled red circles with transparency 0.1
  geom_point(data = subset(pca_data, Data_Source == "Modern"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.1, size = 3) +
  # Plot Published sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Published"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase main title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
  ) +
  labs(
    title = "PCA of Ovilava Individuals with Published Ancient and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )




## START EXPLORING POPS

## Ovilava only - Look for outliers
library(ggrepel)
# Plot Ovilava with Sample_ID labels
ggplot() +
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  # Add Sample_ID labels
  geom_text_repel(data = subset(pca_data, Data_Source == "Ovilava"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 3, max.overlaps = 30) + 
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase main title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
  ) +
  labs(
    title = "PCA of Ovilava Individuals",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )




## Ovilava and Published - No Modern
library(ggrepel)  # Make sure you have this package loaded

ggplot() +
  # Plot Published sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Published"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  
  # Add Sample_ID labels for Ovilava samples
  geom_text_repel(data = subset(pca_data, Data_Source == "Ovilava"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 4, max.overlaps = 20) +  # Adjust size and max.overlaps as needed
  
  # Custom colors
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  
  # Theme adjustments
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
  ) +
  
  # Labels
  labs(
    title = "PCA of Ovilava and Published Ancient Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )



## Ovilava and Published - With Austria Labelled
library(ggrepel)  # Ensure the package is loaded

ggplot() +
  # Plot Published sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Published"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  
  # Add Sample_ID labels for Ovilava samples
  geom_text_repel(data = subset(pca_data, Data_Source == "Ovilava"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 4, max.overlaps = 20) +  # Adjust overlap limit as needed
  
  # Add Sample_ID labels for Austria samples within Published data
  geom_text_repel(data = subset(pca_data, Data_Source == "Published" & Site == "Austria"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 4, max.overlaps = 20, color = "red") +  # Labels in black for contrast
  
  # Custom colors
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  
  # Theme adjustments
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
  ) +
  
  # Labels
  labs(
    title = "PCA of Ovilava and Published Ancient Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )




## Ovilava and Published - Explore pops
library(ggrepel)  # Ensure the package is loaded

ggplot() +
  # Plot Published sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Published"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  
  # Add Sample_ID labels for Ovilava samples
  geom_text_repel(data = subset(pca_data, Data_Source == "Ovilava"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 4, max.overlaps = 20) +  # Adjust overlap limit as needed
  
  # Add Sample_ID labels for Austria samples within Published data
  geom_text_repel(data = subset(pca_data, Data_Source == "Published" & Site == "Croatia"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 4, max.overlaps = 20, color = "red") +  # Labels in black for contrast
  
  # Custom colors
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  
  # Theme adjustments
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
  ) +
  
  # Labels
  labs(
    title = "PCA of Ovilava and Published Ancient Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )







## Surrounding Countries - Published
library(ggrepel)  # Ensure package is loaded

# Define the Best Starting Countries
best_countries <- c("Austria", "Germany", "Czechia", "Hungary", "Slovakia", "Slovenia")

# Subset Published samples to include only these countries
filtered_published <- subset(pca_data, Data_Source == "Published" & Site %in% best_countries)

ggplot() +
  # Plot Selected Published sites with transparency 0.2
  geom_point(data = filtered_published, 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  
  # Add Sample_ID labels for Ovilava samples
  geom_text_repel(data = subset(pca_data, Data_Source == "Ovilava"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 4, max.overlaps = 20) +  # Adjust overlap limit as needed
  
  # Custom colors
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  
  # Theme adjustments
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
  ) +
  
  # Labels
  labs(
    title = "PCA of Ovilava and Neighboring Ancient Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )



## Plot one country at a time
library(ggrepel)

# Define the site to display from Published data
selected_site <- "Poland"

ggplot() +
  # Plot only the selected Published site with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Published" & Site == selected_site), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  
  # Plot Ovilava samples with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  
  # Add labels for Ovilava samples
  geom_text_repel(data = subset(pca_data, Data_Source == "Ovilava"), 
                  aes(x = PC2, y = PC1, label = ID), 
                  size = 4, max.overlaps = 20) +
  
  # Add labels for TARGET COUNTRY samples within Published data
  #geom_text_repel(data = subset(pca_data, Data_Source == "Published" & Site == "France"), 
                  #aes(x = PC2, y = PC1, label = ID), 
                  #size = 4, max.overlaps = 20, color = "red") +
  
  # Custom colors
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  
  # Theme adjustments
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  
  # Labels
  labs(
    title = paste("PCA of Ovilava and", selected_site),
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )












# WORKS
# Define the desired order of the legend
site_levels <- c("Ovilava", published_sites, "Modern")


# Convert Site column to factor with specified levels
pca_data$Site <- factor(pca_data$Site, levels = site_levels)

# Plot
ggplot() +
  # Plot Modern as unfilled red circles with transparency 0.1
  geom_point(data = subset(pca_data, Data_Source == "Modern"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.1, size = 3) +
  # Plot Published sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Published"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase main title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
  ) +
  labs(
    title = "PCA of Ovilava Individuals with Published Ancient and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )




# Plots all FINALLY
ggplot() +
  # Plot Modern as unfilled red circles with transparency 0.1
  geom_point(data = subset(pca_data, Data_Source == "Modern"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.1, size = 3) +
  # Plot Published sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Published"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_data, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  scale_color_manual(
    values = setNames(pca_data$Colour, pca_data$Site),
    name = "Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase main title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15) # Increase legend text size
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

