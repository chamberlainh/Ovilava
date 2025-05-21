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

   
# Modern and Ovilava only
# Filter only Ovilava and Modern samples
pca_subset <- subset(pca_data, Data_Source %in% c("Modern", "Ovilava"))

# Plot only Modern and Ovilava samples
ggplot() +
  # Plot Modern as unfilled gray circles with transparency 0.1
  geom_point(data = subset(pca_subset, Data_Source == "Modern"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 0.1, size = 3) +
  # Plot Ovilava with full opacity
  geom_point(data = subset(pca_subset, Data_Source == "Ovilava"), 
             aes(x = PC2, y = PC1, color = Site), alpha = 1, size = 3) +
  scale_color_manual(
    values = setNames(pca_subset$Colour, pca_subset$Site),
    name = "Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Ovilava and Modern Populations",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)")
  )


                                        
                                        
                                        
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



# PERFECT FOR MODERN
# Modern and Ovilava by Modern Country
# Make a copy of pca_data
pca_data_copy <- pca_data

# Extract country from the Population column by taking the part before the first "_"
pca_data_copy$Country <- sub("_.*", "", pca_data_copy$Population)

# Remove rows where Country is numeric (if any)
pca_data_copy <- pca_data_copy[!grepl("^[0-9]+$", pca_data_copy$Country), ]

# Filter data for modern populations
modern_data <- subset(pca_data_copy, Data_Source %in% c("Modern"))

# Convert Country to a factor to preserve ordering
modern_data$Country <- factor(modern_data$Country)

# Check the unique countries in the Country column
unique(modern_data$Country)


# Plot PCA with Country as color for Modern populations
ggplot(modern_data, aes(x = PC1, y = PC2, color = Country)) +
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
    title = "PCA of Modern Populations by Country",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )


# ADD OVILAVA
library(dplyr)
library(ggrepel)


# Prepare ovilava_subset: ensure same columns
ovilava_plot_data <- ovilava_subset %>%
  mutate(Country = "Ovilava") %>%
  select(ID, PC1, PC2, Population, Data_Source, Country)

# Combine the two datasets
combined_data <- bind_rows(modern_data, ovilava_plot_data)

# Assign colors: viridis for modern + orange for Ovilava
country_colors <- setNames(viridis(length(modern_countries), option = "D"), modern_countries)
country_colors["Ovilava"] <- "#F89441"

# Compute centroids for each Country
country_centroids <- combined_data %>%
  group_by(Country) %>%
  summarize(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE)
  )

# Plot PCA points, add Ovilava labels with IDs
ggplot(combined_data, aes(x = PC1, y = PC2, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = country_centroids,
    aes(x = PC1, y = PC2, label = Country),
    size = 6,
    fontface = "bold",
    color = "black",
    show.legend = FALSE
  ) +
  #ONLY OVILAVA LABELS
  #geom_text_repel(
    #data = filter(combined_data, Country == "Ovilava"), 
    #aes(label = ID),
    #color = "#F89441",
    #size = 3.5,
    #fontface = "italic"
  #) +
  scale_color_manual(values = country_colors) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Modern Populations by Country with Ovilava Samples",
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    color = "Country"
  )



# Modern, Ovilava, Labels for All
library(ggplot2)
library(ggrepel)
library(dplyr)

# 1. Calculate country centroids for labels
country_centroids <- combined_data %>%
  group_by(Country) %>%
  summarize(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE)
  )

ggplot(combined_data, aes(x = PC1, y = PC2, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +
  
  # 2. Add Ovilava sample ID labels at each Ovilava point
  geom_text_repel(
    data = filter(combined_data, Country == "Ovilava"),
    aes(label = ID),
    color = "#F89441",
    size = 3.5,
    fontface = "italic"
  ) +
  
  # 3. Add country labels at centroid positions
  geom_text_repel(
    data = country_centroids,
    aes(x = PC1, y = PC2, label = Country),
    size = 6,
    fontface = "bold",
    color = "black",
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = country_colors) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Modern Populations by Country with Ovilava Samples",
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    color = "Country"
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




# Plot all the COUNTRY roman and medieval data with Hungary removed

# Assign a Country value to Ovilava individuals (e.g., "Austria" or "Ovilava")
pca_data$Country <- ifelse(pca_data$Data_Source == "Ovilava", "Ovilava", NA)

# For the rest, extract from Population as you did
pca_data$Country <- ifelse(is.na(pca_data$Country),
                           sub("_.*", "", pca_data$Population),
                           pca_data$Country)
library(ggrepel)  # for better text labels

# Combine data
combined_data <- rbind(roman_medieval_data_no_hungary,
                       subset(pca_data, Data_Source == "Ovilava"))

# Create the PCA plot with labels for Ovilava samples
ggplot(combined_data, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +
  # Add labels to Ovilava points only
  geom_text_repel(
    data = subset(combined_data, Data_Source == "Ovilava"),
    aes(label = ID),
    size = 3,
    max.overlaps = 100,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2
  ) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava Populations by Country (Excluding Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country"
  )




## Modern, Roman, Medieval, Ovilava, NO HUNGRY

plot_data <- subset(
  pca_data,
  Country != "Hungary" & Data_Source %in% c("Modern", "Roman", "Medieval", "Ovilava")
)

library(ggplot2)
library(ggrepel)

ggplot(plot_data, aes(x = PC2, y = PC1, color = Data_Source)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = subset(plot_data, Data_Source == "Ovilava"),
    aes(label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100
  ) +
  scale_color_manual(values = c("Modern" = "gray",
                                "Roman" = "#440154FF",
                                "Medieval" = "#FDE725FF",
                                "Ovilava" = "orange")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Modern, Roman, Medieval, and Ovilava Samples (Excluding Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Data Source"
  )


------------------------------------------------------------------------------------------------------------------------

## ONLY AMS samples from Ovilava 

# Remove individual I15499.AG from Serbia who is skewing the PCA
pca_data <- pca_data %>% filter(ID != "I15499.AG")


# Filter Modern, Roman, and Medieval (excluding Hungary)
filtered_data <- subset(
  pca_data,
  Country != "Hungary" & Data_Source %in% c("Modern", "Roman", "Medieval")
)

# Filter Ovilava using only matched IDs
ovilava_subset <- subset(
  pca_data,
  Data_Source == "Ovilava" & gsub("\\.TW$", "", ID) %in% matching_ids
)

# Combine all for plotting
plot_data <- rbind(filtered_data, ovilava_subset)


ggplot(plot_data, aes(x = PC2, y = PC1, color = Data_Source)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = subset(plot_data, Data_Source == "Ovilava"),
    aes(label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100
  ) +
  scale_color_manual(values = c(
    "Modern" = "gray",
    "Roman" = "#440154FF",
    "Medieval" = "#FDE725FF",
    "Ovilava" = "orange"
  )) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Modern, Roman, Medieval, and Ovilava Samples (Excluding Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Data Source"
  )




# THIS IS WHERE I ADDED THE NON-AMS ROMAN FROM OVILAVA
## Plot based on Phase

library(dplyr)
library(ggplot2)
library(ggrepel)

# Load AMS data
ams_data <- read.csv("OVILAVA_14C_data.csv")

# Clean Ovilava IDs in pca_data
pca_data$Clean_ID <- gsub("\\.TW$", "", pca_data$ID)

# Match IDs
matching_ids <- intersect(ams_data$Master_ID, pca_data$Clean_ID[pca_data$Data_Source == "Ovilava"])

# Subset AMS data to only matched
matched_ams <- ams_data[ams_data$Master_ID %in% matching_ids, ]

# Subset and join Ovilava
ovilava_subset <- pca_data %>%
  filter(Data_Source == "Ovilava" & Clean_ID %in% matching_ids) %>%
  left_join(matched_ams, by = c("Clean_ID" = "Master_ID"))

# Add phase labels like "Phase 1", "Phase 2", etc.
ovilava_subset$Phase_Label <- paste0("Phase ", ovilava_subset$X14C_Phase)

# Subset Modern/Roman/Medieval (exclude Hungary)
other_data <- pca_data %>%
  filter(Data_Source %in% c("Modern", "Roman", "Medieval") & Country != "Hungary") %>%
  mutate(Phase_Label = Data_Source)

# Combine
plot_data <- bind_rows(other_data, ovilava_subset)

# Define colors
color_vals <- c(
  "Modern" = "gray",
  "Roman" = "#440154FF",
  "Medieval" = "#FDE725FF",
  "Phase 0" = "#D55E00",
  "Phase 1" = "#E69F00",
  "Phase 2" = "#56B4E9",
  "Phase 3" = "#009E73",
  "Phase 4" = "#CC79A7",
  "Phase 5" = "#0072B2",
  "Phase 6" = "#999999",
  "Phase 7" = "black"
)

# Plot
ggplot(plot_data, aes(x = PC2, y = PC1, color = Phase_Label)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = ovilava_subset,
    aes(label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100
  ) +
  scale_color_manual(values = color_vals) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Modern, Roman, Medieval, and Ovilava Samples (Ovilava Colored by 14C Phase)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Group / Phase"
  )



## REMOVE modern
# Subset Roman and Medieval only (exclude Hungary and Modern)






library(dplyr)
library(ggplot2)
library(ggrepel)

# Load AMS data
ams_data <- read.csv("OVILAVA_14C_data.csv")

# Clean Ovilava IDs in pca_data
pca_data$Clean_ID <- gsub("\\.TW$", "", pca_data$ID)

# Match IDs
matching_ids <- intersect(ams_data$Master_ID, pca_data$Clean_ID[pca_data$Data_Source == "Ovilava"])

# Subset AMS data to only matched
matched_ams <- ams_data[ams_data$Master_ID %in% matching_ids, ]

# Subset and join Ovilava
ovilava_subset <- pca_data %>%
  filter(Data_Source == "Ovilava" & Clean_ID %in% matching_ids) %>%
  left_join(matched_ams, by = c("Clean_ID" = "Master_ID"))

# Add phase labels like "Phase 1", "Phase 2", etc.
ovilava_subset$Phase_Label <- paste0("Phase ", ovilava_subset$X14C_Phase)

# Subset Modern/Roman/Medieval (exclude Hungary)
other_data <- pca_data %>%
  filter(Data_Source %in% c("Roman", "Medieval") & Country != "Hungary") %>%
  mutate(Phase_Label = Data_Source)

# Combine
plot_data <- bind_rows(other_data, ovilava_subset)

# Define colors
color_vals <- c(
  "Modern" = "gray",
  "Roman" = "#440154FF",
  "Medieval" = "#FDE725FF",
  "Phase 0" = "#D55E00",
  "Phase 1" = "#E69F00",
  "Phase 2" = "#56B4E9",
  "Phase 3" = "#009E73",
  "Phase 4" = "#CC79A7",
  "Phase 5" = "#0072B2",
  "Phase 6" = "#999999",
  "Phase 7" = "black"
)





# Plot
ggplot(plot_data, aes(x = PC2, y = PC1, color = Phase_Label)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = ovilava_subset,
    aes(label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100
  ) +
  scale_color_manual(values = color_vals) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava 14C Phase Samples (No Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Group / Phase"
  )






## Descriptive phase labels 

# Define Phase Labels
phase_lookup <- c(
  "0" = "Julio-Claudian dynasty (27 BCE–68 CE)\nNeronian (54–68 CE)",
  "1" = "Adoptive Emperors | Traianic (98–117 CE)",
  "2" = "Adoptive Emperors | Antonine (138–192 CE)",
  "3" = "Severan dynasty (193–235 CE)",
  "4" = "Barracks Emperors (235–284 CE)",
  "5" = "Tetrarchy (284–313 CE)",
  "6" = "Early Middle Ages (500–1100 CE)",
  "7" = "Roman Context-Dated"
)

phase_levels <- c(
  "Julio-Claudian dynasty (27 BCE–68 CE)\nNeronian (54–68 CE)",  # Phase 0
  "Adoptive Emperors | Traianic (98–117 CE)",                    # Phase 1
  "Adoptive Emperors | Antonine (138–192 CE)",                   # Phase 2
  "Severan dynasty (193–235 CE)",                                # Phase 3
  "Barracks Emperors (235–284 CE)",                              # Phase 4
  "Tetrarchy (284–313 CE)",                                      # Phase 5
  "Early Middle Ages (500–1100 CE)",                             # Phase 6
  "Roman Context-Dated",                                         # Phase 7
  "Roman",
  "Medieval"
)


# Apply labels
# Apply phase labels where possible
# First keep Phase_Label as-is (Roman, Medieval already set)
# Only update labels for actual Ovilava phases (i.e., non-NA X14C_Phase)
plot_data$Phase_Label[!is.na(plot_data$X14C_Phase)] <-
  phase_lookup[as.character(plot_data$X14C_Phase[!is.na(plot_data$X14C_Phase)])]

plot_data$Phase_Label <- factor(plot_data$Phase_Label, levels = phase_levels)


# Colors
color_vals <- c(
  "Julio-Claudian dynasty (27 BCE–68 CE)\nNeronian (54–68 CE)" = "#D55E00",
  "Adoptive Emperors | Traianic (98–117 CE)" = "#E69F00",
  "Adoptive Emperors | Antonine (138–192 CE)" = "#56B4E9",
  "Severan dynasty (193–235 CE)" = "#009E73",
  "Barracks Emperors (235–284 CE)" = "#CC79A7",
  "Tetrarchy (284–313 CE)" = "#0072B2",
  "Early Middle Ages (500–1100 CE)" = "#440154FF",
  "Roman Context-Dated" = "black",
  "Roman" = "#999999",
  "Medieval" = "#FDE725FF"
)

# Plot
ggplot(plot_data, aes(x = PC2, y = PC1, color = Phase_Label)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = ovilava_subset,
    aes(x = PC2, y = PC1, label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100,
    show.legend = FALSE
  ) +
  scale_color_manual(values = color_vals, drop = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava 14C Phase Samples (No Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Group / Phase"
  )




----------------------------------------------------------------------------------------------------
## Biological Sex

# Read in .fam file, in 5th column, 1=male, 2=female, 0=unknown


fam <- read.table("MergedOvilavaTime30k_trim.fam", header = FALSE)
colnames(fam) <- c("FID", "IID", "Father", "Mother", "Sex", "Phenotype")

# Merge sex information into plot_data
plot_data <- left_join(plot_data, fam %>% select(IID, Sex), by = c("ID" = "IID"))


# Create Sex_Label based on Sex column
plot_data$Sex_Label <- ifelse(plot_data$Sex.x == 1, "Male", 
                              ifelse(plot_data$Sex.x == 2, "Female", "Unknown"))

# Get counts for gender
head(plot_data)
table(plot_data$Sex_Label[plot_data$Data_Source == "Ovilava"])

library(dplyr)

plot_data %>%
  group_by(Phase_Label, Sex_Label) %>%
  summarise(Count = n()) %>%
  arrange(Phase_Label, Sex_Label)

plot_data %>%
  count(Phase_Label)



# PCA Plot with shapes based on biological sex
ggplot(plot_data, aes(x = PC2, y = PC1, color = Phase_Label, shape = Sex_Label)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = plot_data,
    aes(x = PC2, y = PC1, label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100,
    inherit.aes = TRUE,  # So the position is inherited
    show.legend = FALSE  # Prevent this layer from appearing in the legend
  ) +
  scale_color_manual(values = color_vals) +
  scale_shape_manual(values = c("Male" = 0, "Female" = 5, "Unknown" = 4)) +  # Shape mapping for Male, Female, and Unknown
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava 14C Phase Samples (No Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Group / Phase",
    shape = "Biological Sex"  # Legend label for biological sex
  )


# PCA Plot with shapes based on biological sex NO LABELS
ggplot(plot_data, aes(x = PC2, y = PC1, color = Phase_Label, shape = Sex_Label)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = color_vals) +
  scale_shape_manual(values = c("Male" = 15, "Female" = 17, "Unknown" = 4)) +  # Shape mapping for Male, Female, and Unknown
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava 14C Phase Samples (No Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Group / Phase",
    shape = "Biological Sex"  # Legend label for biological sex
  )




# PCA Plot with shapes based on biological sex and labels from ovilava_subset
ggplot(plot_data, aes(x = PC2, y = PC1, color = Phase_Label, shape = Sex_Label)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_text_repel(
    data = ovilava_subset,
    aes(x = PC2, y = PC1, label = Clean_ID),  # Use Clean_ID or ID
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100,
    inherit.aes = FALSE,  # Prevents the error by not inheriting Sex_Label
    show.legend = FALSE
  ) +
  scale_color_manual(values = color_vals) +
  scale_shape_manual(values = c("Male" = 15, "Female" = 17, "Unknown" = 4)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava 14C Phase Samples by Sex (No Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Group / Phase",
    shape = "Biological Sex"
  )


library(dplyr)
ovilava_subset %>%
  count(Sex_Label)
table(ovilava_subset$Sex_Label)



--------------------------------------------------------------------------------------------------------------------------

## By Country and Phase DOESNT WORK YET

  # Update Ovilava phase labels using lookup
  plot_data$Phase_Label[!is.na(plot_data$X14C_Phase)] <-
  phase_lookup[as.character(plot_data$X14C_Phase[!is.na(plot_data$X14C_Phase)])]

# Convert to factor with desired order
plot_data$Phase_Label <- factor(plot_data$Phase_Label, levels = phase_levels)


# Subset
roman_medieval_data_no_hungary <- plot_data %>%
  filter(Data_Source %in% c("Roman", "Medieval"), Country != "Hungary")

ams_data <- plot_data %>%
  filter(Data_Source == "Ovilava")

# Combine
combined_data <- rbind(roman_medieval_data_no_hungary, ams_data)

combined_data$Plot_Group <- combined_data$Phase_Label


ggplot(combined_data, aes(x = PC2, y = PC1, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = color_vals, drop = FALSE) +  # Keep all phase levels
  geom_text(
    data = subset(combined_data, Data_Source == "Ovilava"),
    aes(label = ID),
    vjust = -1, hjust = 0.5, size = 3, show.legend = FALSE
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman/Medieval Populations by Country & AMS Samples by Phase",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Phase"
  )





-------------------------------------------------------------------------------
# Phase and Country
  


# Make sure Phase_Label is a factor with defined levels
plot_data$Phase_Label[!is.na(plot_data$X14C_Phase)] <-
phase_lookup[as.character(plot_data$X14C_Phase[!is.na(plot_data$X14C_Phase)])]

plot_data$Phase_Label <- factor(plot_data$Phase_Label, levels = phase_levels)

# Remove individual I15499.AG from Serbia who is skewing the PCA
plot_data <- plot_data %>% filter(ID != "I15499.AG")

# Subset Roman + Medieval (excluding Hungary) plus Ovilava
combined_data <- rbind(
  subset(plot_data, Data_Source %in% c("Roman", "Medieval") & Country != "Hungary"),
  subset(plot_data, Data_Source == "Ovilava")
)

# Plot with color = Country, shape = Phase
# Plot with color = Country, shape = Phase
ggplot(combined_data, aes(x = PC2, y = PC1, color = Country, shape = Phase_Label)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_text_repel(
    data = subset(combined_data, Data_Source == "Ovilava"),
    aes(label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100,
    show.legend = FALSE
  ) +
  scale_color_viridis(discrete = TRUE) +
  scale_shape_manual(values = c(
    "Julio-Claudian dynasty (27 BCE–68 CE)\nNeronian (54–68 CE)" = 0,  # square
    "Adoptive Emperors | Traianic (98–117 CE)" = 1,                   # circle
    "Adoptive Emperors | Antonine (138–192 CE)" = 2,                  # triangle
    "Severan dynasty (193–235 CE)" = 3,                               # plus
    "Barracks Emperors (235–284 CE)" = 4,                             # cross
    "Tetrarchy (284–313 CE)" = 5,                                     # diamond
    "Early Middle Ages (500–1100 CE)" = 6,                            # inverted triangle
    "Roman" = 16,                                                     # filled circle
    "Medieval" = 16                                                   # filled circle w/ border
  ), drop = FALSE) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava Populations by Country and Phase",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country",
    shape = "Phase"
  )


# Change country transparency
# Set a fixed alpha level
combined_data$Alpha_Level <- 0.5

# Plot
ggplot(combined_data, aes(x = PC2, y = PC1, color = Country, shape = Phase_Label, alpha = Alpha_Level)) +
  geom_point(size = 3) +
  geom_text_repel(
    data = subset(combined_data, Data_Source == "Ovilava"),
    aes(label = ID),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.size = 0.2,
    max.overlaps = 100,
    show.legend = FALSE
  ) +
  scale_color_viridis(discrete = TRUE) +
  scale_shape_manual(values = c(
    "Julio-Claudian dynasty (27 BCE–68 CE)\nNeronian (54–68 CE)" = 0,
    "Adoptive Emperors | Traianic (98–117 CE)" = 1,
    "Adoptive Emperors | Antonine (138–192 CE)" = 2,
    "Severan dynasty (193–235 CE)" = 3,
    "Barracks Emperors (235–284 CE)" = 4,
    "Tetrarchy (284–313 CE)" = 5,
    "Early Middle Ages (500–1100 CE)" = 6,
    "Roman Context-Dated" = 7,
    "Roman" = 16,
    "Medieval" = 16
  ), drop = FALSE) +
  scale_alpha(range = c(0.5, 0.5), guide = "none") +  # Fixed transparency, no legend
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 15)
  ) +
  labs(
    title = "PCA of Roman, Medieval, and Ovilava Populations by Country and Phase (No Hungary)",
    x = paste0("PC2 (", pc2_var, "%)"),
    y = paste0("PC1 (", pc1_var, "%)"),
    color = "Country",
    shape = "Phase"
  )

