# PCA_Time for Ovilava
## Author: Heather Chamberlain
## Date: April 2025

## Set working directory  
setwd("/lisc/scratch/anthropology/Pinhasi_group/Ovilava/PCA_Time")

## Load libraries
library("dplyr") # for dataframe management
library("viridis") # colours
library("ggplot2") # plotting


## Load Files
# Load PCA .evec file
pca_data <- read.table("MergedOvilavaTime30k_trim.csv", header = TRUE, sep = ",")

# Load population files
ovilava <- read.table("cleaned_my_list.txt", header = FALSE, stringsAsFactors = FALSE)
roman <- read.table("roman_list_cleaned.txt", header = FALSE, stringsAsFactors = FALSE)
medieval <- read.table("medieval_list_cleaned.txt", header = FALSE, stringsAsFactors = FALSE)
modern <- read.table("modern_list_cleaned.txt", header = FALSE, stringsAsFactors = FALSE)

## Data Wrangle
# Prepare pca_data with pop files
pca_data$Data_Source <- ifelse(pca_data$ID %in% modern$V1 & 
                                 !(pca_data$ID %in% ovilava$V1) & 
                                 !(pca_data$ID %in% roman$V1) & 
                                 !(pca_data$ID %in% medieval$V1), "Modern",
                               ifelse(pca_data$ID %in% ovilava$V1, "Ovilava",
                                      ifelse(pca_data$ID %in% roman$V1, "Roman", 
                                             ifelse(pca_data$ID %in% medieval$V1, "Medieval", "Unknown"))))

# This should work now, but let's verify the distribution
table(pca_data$Data_Source)



# Add Site column with NA values
pca_data$Site <- NA


# Assign Site based on Data_Source
pca_data$Site <- ifelse(pca_data$Data_Source == "Modern", "Modern",
                        ifelse(pca_data$Data_Source == "Roman", "Roman",
                               ifelse(pca_data$Data_Source == "Medieval", "Medieval",
                                      ifelse(pca_data$Data_Source == "Ovilava", "Ovilava", 
                                             pca_data$Site))))  # Retain existing values for other cases


# Define all eigenvalues from the MergedOvilava30k_trim..evec header
eigvals <- c(6.876, 3.297, 2.512, 2.327, 2.087, 2.036, 1.996, 1.983, 1.969, 1.954)

# Calculate percentage of variance explained
percent_var <- (eigvals / sum(eigvals)) * 100

# Extract percentages for PC1 and PC2
pc1_var <- round(percent_var[1], 2)
pc2_var <- round(percent_var[2], 2)

## Color Assignments

# Get unique Site names
site_names <- unique(pca_data$Site)

# Generate viridis colors for all sites
site_colors <- viridis(length(site_names), option = "D")

# Create a named vector for site-to-color mapping
site_color_map <- setNames(site_colors, site_names)

# Assign colors based on site
pca_data$Colour <- ifelse(pca_data$Data_Source == "Modern", "grey",  # Modern as gray
                          ifelse(pca_data$Data_Source == "Roman", site_color_map["Roman"],  # Roman color
                                 ifelse(pca_data$Data_Source == "Medieval", site_color_map["Medieval"],  # Medieval color
                                        ifelse(pca_data$Data_Source == "Ovilava", "#F89441", NA))))  # Ovilava color

   
                                        
                                        
                                        
                                        
## Plot     
# Plot ALL
# Replace NA in Site with "Modern"
pca_data$Site[is.na(pca_data$Site)] <- "Modern"

# Convert Site to a factor to preserve ordering
pca_data$Site <- factor(pca_data$Site)

# Plot PCA with colors for each population and custom opacity
ggplot() +
  # Plot Modern as unfilled gray circles with transparency 0.1
  geom_point(data = subset(pca_data, Data_Source == "Modern"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.1, size = 3) +
  # Plot Roman sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Roman"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.2, size = 3) +
  # Plot Medieval sites with transparency 0.2
  geom_point(data = subset(pca_data, Data_Source == "Medieval"), 
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
    title = "PCA of Ovilava, Roman, Medieval, and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )




# Plot by Roman/Medieval Country
# Make a copy of pca_data
pca_data_copy <- pca_data

# Extract country from the Population column by taking the part before the first "_"
pca_data_copy$Country <- sub("_.*", "", pca_data_copy$Population)

# Remove rows where Country is numeric (if any)
pca_data_copy <- pca_data_copy[!grepl("^[0-9]+$", pca_data_copy$Country), ]

# Filter data for Roman and Medieval populations
roman_medieval_data <- subset(pca_data_copy, Data_Source %in% c("Roman", "Medieval"))

# Convert Country to a factor to preserve ordering
roman_medieval_data$Country <- factor(roman_medieval_data$Country)

# Check the unique countries in the Country column
unique(roman_medieval_data$Country)


# Plot PCA with Country as color for Roman and Medieval populations
ggplot(roman_medieval_data, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +  # Adjust transparency and point size
  scale_color_viridis(discrete = TRUE) +  # Use viridis colors for better distinction
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15) # Increase legend title size
  ) +
  labs(
    title = "PCA of Roman and Medieval Populations by Country",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )








## Plot by Country for Roman and Medieval (without Hungary)
# Subset data to include only Roman and Medieval populations and exclude Hungary
roman_medieval_data_no_hungary <- subset(roman_medieval_data, Data_Source %in% c("Roman", "Medieval") & Country != "Hungary")

# Convert Country to a factor to preserve the order of countries
roman_medieval_data_no_hungary$Country <- factor(roman_medieval_data_no_hungary$Country)

# Plot PCA with Country as color
ggplot(roman_medieval_data_no_hungary, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_viridis(discrete = TRUE) +  # Use viridis colors for better distinction
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15) # Increase legend title size
  ) +
  labs(
    title = "PCA of Roman and Medieval Populations by Country (Excluding Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )





# Roman, Medieval, Ovilava (without Hungary)
# Combine Ovilava data with the Roman and Medieval data (excluding Hungary)
combined_data <- rbind(roman_medieval_data_no_hungary, subset(pca_data, Data_Source == "Ovilava"))

# Plot combined data (Roman, Medieval, and Ovilava)
ggplot(combined_data, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_viridis(discrete = TRUE) + # Use viridis colors for better distinction
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15) # Increase legend title size
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava Populations by Country (Excluding Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )



# Combined with Ovilava Labels
# Combine Ovilava data with the Roman and Medieval data (excluding Hungary)
combined_data <- rbind(roman_medieval_data_no_hungary, subset(pca_data, Data_Source == "Ovilava"))

# Plot combined data (Roman, Medieval, and Ovilava)
ggplot(combined_data, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +  # Plot points
  scale_color_viridis(discrete = TRUE) + # Use viridis colors for better distinction
  geom_text(data = subset(combined_data, Data_Source == "Ovilava"),  # Add labels for Ovilava
            aes(label = ID), vjust = -1, hjust = 0.5, size = 3) +  # Adjust position and size of labels
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15) # Increase legend title size
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava Populations by Country (Excluding Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )





# Combined with Ovilava and Algeria Labels for I35390

# Combine Ovilava data with the Roman and Medieval data (excluding Hungary)
combined_data <- rbind(roman_medieval_data_no_hungary, subset(pca_data, Data_Source == "Ovilava"))

# Plot combined data (Roman, Medieval, and Ovilava)
ggplot(combined_data, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +  # Plot points
  scale_color_viridis(discrete = TRUE) + # Use viridis colors for better distinction
  # Add labels for Ovilava
  geom_text(data = subset(combined_data, Data_Source == "Ovilava"),  
            aes(label = ID), vjust = -1, hjust = 0.5, size = 5) +
  # Add labels for Algeria
  geom_text(data = subset(combined_data, Country == "Algeria"),  
            aes(label = ID), vjust = -1, hjust = 0.5, size = 5, color = "blue") +  # Adjust color for Algeria labels
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15) # Increase legend title size
  ) +
  labs(
    title = "PCA of I35390.TW and Algeria",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )




# For I35419.TW
# Combine Ovilava data with the Roman and Medieval data (excluding Hungary)
combined_data <- rbind(roman_medieval_data_no_hungary, subset(pca_data, Data_Source == "Ovilava"))

# Plot combined data (Roman, Medieval, and Ovilava)
ggplot(combined_data, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +  # Plot points
  scale_color_viridis(discrete = TRUE) + # Use viridis colors for better distinction
  # Add labels for Ovilava
  geom_text(data = subset(combined_data, Data_Source == "Ovilava"),  
            aes(label = ID), vjust = -1, hjust = 0.5, size = 3) +
  # Add labels for Serbia
  geom_text(data = subset(combined_data, Country == "Serbia"),  
            aes(label = ID), vjust = -1, hjust = 0.5, size = 3, color = "blue") +  # Adjust color for Algeria labels
  # Add labels for Austria
  geom_text(data = subset(combined_data, Country == "Austria"),  
            aes(label = ID), vjust = -1, hjust = 0.5, size = 3, color = "purple") +  # Adjust color for Algeria labels
  # Add labels for Turkey
  geom_text(data = subset(combined_data, Country == "Turkey"),  
            aes(label = ID), vjust = -1, hjust = 0.5, size = 3, color = "green") +  # Adjust color for Algeria labels
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Remove grid
    plot.title = element_text(size = 20),  # Increase title size
    axis.title = element_text(size = 15),  # Increase axis title size
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15) # Increase legend title size
  ) +
  labs(
    title = "PCA of I35419.TW, Serbia, Austria, and Turkey",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )




## AMS RADIOCARBON DATES
# Load AMSfile
ams_data <- read.table("OVILAVA_14C_data.csv", header = TRUE, sep = ",")

# Find matching IDs between the ovilava and ams datasets
# Read in your data

# Extract the IDs from both datasets
ams_ids <- ams_data$Master_ID
ovilava_ids <- gsub("\\.TW$", "", ovilava$V1)  # Remove '.TW' suffix

# Find intersection
matching_ids <- intersect(ams_ids, ovilava_ids)

# View results
# print(matching_ids)
# length(matching_ids)


# missing_ids <- setdiff(ovilava_ids, ams_data$Master_ID)
# length(missing_ids)
# missing_ids
# These missing IDs are Early Medieval that were not radiocarbon dated







