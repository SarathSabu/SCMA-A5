setwd ("~/Desktop/scma/A5")
# 1. Draw a histogram of the data to indicate the consumption district-wise.

# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("readr", "ggplot2", "dplyr", "gridExtra")

# Call the function
install_and_load(packages)

# Load the data
file_path <- 'NSSO68.csv'  
data <- read_csv(file_path)

# Inspect the first few rows of the dataset to understand its structure
print(head(data))

# Subset the data with specified columns and filter based on state_1
subset_data <- data %>%
  select(state, District, MPCE_URP, MPCE_MRP, state_1)

filtered_data <- subset_data %>%
  filter(state_1 == 'MP')

# Plot histograms for MPCE_URP and MPCE_MRP
p1 <- ggplot(filtered_data, aes(x = MPCE_URP)) +
  geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) +
  ggtitle('Histogram of MPCE_URP for MADHYAPRADESH') +
  xlab('MPCE_URP') +
  ylab('Frequency')

p2 <- ggplot(filtered_data, aes(x = MPCE_MRP)) +
  geom_histogram(bins = 30, fill = 'green', alpha = 0.7) +
  ggtitle('Histogram of MPCE_MRP for MADHYAPRADESH') +
  xlab('MPCE_MRP') +
  ylab('Frequency')

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)

# Group the data by state and sum the consumption
state_consumption <- data %>%
  group_by(state_1) %>%
  summarise(total_consumption = sum(MPCE_URP))

# Plot the bar chart
ggplot(state_consumption, aes(x = state_1, y = total_consumption)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Bar Chart of Consumption State-wise",
       x = "State",
       y = "Total Consumption") +
  theme_minimal()

# 2.Depict the consumption on the state map, showing consumption in each district.

# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("sf", "ggplot2", "dplyr", "readr","readxl","janitor","tidyverse")

# Call the function
install_and_load(packages)

# Load the shapefile
gdf_districts <- st_read('District.shp')

# Display the first few rows
head(gdf_districts)

# Plot the map
ggplot() +
  geom_sf(data = gdf_districts) +
  labs(title = "Map of Districts") +
  theme_minimal()

# Load NSSO68.csv file
nsso_data <- read_csv('NSSO68.csv')

# Subset the data with specified columns and filter based on state_1 (assuming state_1 is equivalent to state in R)
subset_data <- nsso_data %>%
  select(state_1, District, MPCE_URP, MPCE_MRP) %>%
  filter(state_1 == 'KA')  # Filter for Karnataka state

# Load district-codes.xlsx file
district_codes <- read_excel('district-codes.xlsx')
district_codes <- clean_names(district_codes)
names(district_codes)


# Filter district codes for Karnataka
karnataka_districts <- district_codes %>%
  filter(state_name == 'Karnataka')

# Create a mapping from district codes to district names
district_mapping <- karnataka_districts %>%
  select(dc, `district_name`) %>%
  deframe()

# Replace district codes in the filtered NSSO data with district names
subset_data <- subset_data %>%
  mutate(District = district_mapping[District])

# Sum all values district-wise
district_wise_sum <- subset_data %>%
  group_by(District) %>%
  summarise(MPCE_URP = sum(MPCE_URP), MPCE_MRP = sum(MPCE_MRP))

# Display the resulting DataFrame
print(district_wise_sum)

df <- district_wise_sum

# Merge the shapefile with the DataFrame
gdf_merged <- gdf_districts %>%
  left_join(df, by = c('KGISDist_1' = 'District'))


# Replace NaN values with a value lower than min for visualization purposes
gdf_merged$MPCE_URP[is.na(gdf_merged$MPCE_URP)] <- min(gdf_merged$MPCE_URP, na.rm = TRUE) - 1

# Calculate the centroids
gdf_merged$centroid <- st_centroid(gdf_merged$geometry)

# Extract the coordinates of the centroids
gdf_merged$X <- st_coordinates(gdf_merged$centroid)[, "X"]
gdf_merged$Y <- st_coordinates(gdf_merged$centroid)[, "Y"]

# Plot using ggplot2
ggplot() +
  geom_sf(data = gdf_districts, color = "black", fill = "white") +
  geom_sf(data = gdf_merged, aes(fill = MPCE_URP), color = "black") +
  scale_fill_gradientn(colors = rev(heat.colors(10)), na.value = "white", name = "MPCE_URP") +
  geom_text(data = gdf_merged, aes(x = X, y = Y, label = KGISDist_1), size = 3) +
  labs(title = "MPCE_URP in Karnataka Districts",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

