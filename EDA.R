install.packages("ggplot2")
library(ggplot2)  

# Load your datasets
olympics_data <- read_excel("Final_olympics_data.xlsx")

# Select categorical variables (columns with character or factor data type)
categorical_cols <- names(olympics_data)[sapply(olympics_data, is.character) | sapply(olympics_data, is.factor)]

# Select numerical variables (columns with numeric or integer data type)
numerical_cols <- names(olympics_data)[sapply(olympics_data, is.numeric)]

# Print the categorical and numerical variables
cat("Categorical Variables:\n")
print(categorical_cols)
cat("Numerical Variables:\n")
print(numerical_cols)

summary(olympics_data) 

length(olympics_data)

# Create a summary function for Olympics dataset
summary_stats <- function(df) {
  stats <- data.frame(
    Variance = sapply(df, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA),
    Standard_Deviation = sapply(df, function(x) if(is.numeric(x)) sd(x, na.rm = TRUE) else NA)
  )
  return(stats)
}
result <- summary_stats(olympics_data)

# View the result
print(result)

# Create a long format of the data for ggplot
numerical_data <- olympics_data[, numerical_cols, drop = FALSE]

numerical_data

long_data <- reshape2::melt(numerical_data)

# Plot the boxplots
ggplot(long_data, aes(x = value, y = variable)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot of Numerical Columns", 
       x = "Values", 
       y = "Numerical Columns") +
  theme(plot.title = element_text(hjust = 0.5))

# Load necessary libraries
library(e1071)  # For skewness calculation
library(reshape2)

# Calculate skewness for all numerical columns
skewness_values <- sapply(olympics_data[, numerical_cols, drop = FALSE], function(x) {
  round(e1071::skewness(x, na.rm = TRUE), 2)
})

# Print skewness values
cat("Skewness for each numerical column:\n")
print(skewness_values)

# Load necessary libraries
install.packages("gridExtra")
library(gridExtra)

# Create a list of categorical variables to visualize
categorical_vars <- c('Gender', 'Country', 'Continents', 
                      'Event_Type', 'Gold','Silver','Bronze','Total_Medals')

# Create a plot for each categorical variable
plots <- lapply(categorical_vars, function(col) {
  ggplot(olympics_data, aes_string(x = col)) +
    geom_bar(fill = "lightblue") +
    theme_minimal() +
    ggtitle(paste("Bar Plot for", col)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
})

# Arrange the plots for uni-variate Analysis
grid.arrange(grobs = plots[1])
grid.arrange(grobs = plots[2])
grid.arrange(grobs = plots[3])  
grid.arrange(grobs = plots[4])
grid.arrange(grobs = plots[5])
grid.arrange(grobs = plots[6]) 
grid.arrange(grobs = plots[7]) 
grid.arrange(grobs = plots[8]) 

#Bivariate Analysis

#Plot Total Gold Medals by Continent
ggplot(olympics_data, aes(x = Continents, y = Gold)) +  
  geom_bar(stat = "identity", fill = "gold", alpha = 0.7) +
  labs(title = "Total Gold Medals by Continent", x = "Continent", y = "Total Gold Medals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot Total Silver Medals by Continent
ggplot(olympics_data, aes(x = Continents, y = Silver)) +
  geom_bar(stat = "identity", fill = "grey", alpha = 0.7) +
  labs(title = "Total Silver Medals by Continent", x = "Continent", y = "Total Silver Medals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Plot Total Bronze Medals by Continent
ggplot(olympics_data, aes(x = Continents, y = Bronze)) +
  geom_bar(stat = "identity", fill = "#cd7f32", alpha = 0.7) +
  labs(title = "Total Bronze Medals by Continent", x = "Continent", y = "Total Bronze Medals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Multivariate analysis: 

# Load necessary libraries
install.packages("GGally")
library(GGally)

# Remove the 'Country' column and other high cardinality columns if necessary
medal_summary_subset <- olympics_data %>%
  select(-Country)  # Exclude 'Country' column

# Plot pair plot for the remaining numeric attributes
ggpairs(medal_summary_subset, aes(colour = Continents))

# Select only numeric columns for correlation matrix
numerical_data <- olympics_data[sapply(olympics_data, is.numeric)]

# Compute correlation matrix
cor_matrix <- cor(numerical_data, use = "complete.obs")

# Melt the correlation matrix for ggplot2
cor_matrix_melted <- melt(cor_matrix)

# Create the heatmap
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Create the desired output using dplyr
medal_summary_plot <- olympics_data %>%
  # Select the relevant columns
  group_by(Continents) %>%
  # Summarize total medals by type (Gold, Silver, Bronze)
  summarise(
    Total_Gold = sum(Gold, na.rm = TRUE),
    Total_Silver = sum(Silver, na.rm = TRUE),
    Total_Bronze = sum(Bronze, na.rm = TRUE)
  ) %>%
  # Reshape to long format using pivot_longer
  pivot_longer(cols = starts_with("Total"), names_to = "Medal_Type", values_to = "Count")

# View the resulting data
print(medal_summary_plot)

# Create the 100% stacked bar chart
ggplot(medal_summary_plot, aes(x = Continents, y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity", position = "fill") +  # Use position = "fill" for proportional bars
  labs(title = "Proportion of Medal Types by Continent",
       x = "Continent",
       y = "Proportion of Medals",
       fill = "Medal Type") +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentages
  scale_fill_manual(values = c("#cd7f32","gold", "grey")) +  # Custom colors for medals
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Set seed for reproducibility for sampling since large number od observations
set.seed(2)

# Sample 50 random countries from the medal summary data
sample_countries <- sample(unique(olympics_data$Country), 50)

# Filter the medal summary to include only the sampled countries
medal_summary_sampled <- olympics_data %>%
  filter(Country %in% sample_countries)

# View the sampled medal summary
medal_summary_sampled

# Reshape the data for the stacked bar plot (long format)
medal_summary_long <- medal_summary_sampled %>%
  pivot_longer(cols = c(Gold, Silver, Bronze), 
               names_to = "Medal", 
               values_to = "Count")

medal_summary_long

# Summarize the data by Gender and Medal
medal_summary_long %>%
  group_by(Gender, Medal) %>%
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  mutate(GenMed = interaction(Gender, Medal),
         Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = "", y = Count, fill = GenMed)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Medals by Gender and Medal Type",
       fill = "Gender and Medal Type") +
  scale_fill_manual(values = c("Female.Gold" = "darkgoldenrod1", "Male.Gold" = "darkgoldenrod2", 
                               "Female.Silver" = "lightgray", "Male.Silver" = "gray",
                               "Female.Bronze" = "saddlebrown", "Male.Bronze" = "sienna"
  )) +
  theme_void() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), position = position_stack(vjust = 0.5))

# Load the RColorBrewer package for color palettes
library(RColorBrewer)

# Create heatmap data with gender
heatmap_data <- medal_summary_sampled %>%
  pivot_longer(cols = c(Gold, Silver, Bronze), 
               names_to = "Medal", 
               values_to = "Count") %>%
  group_by(Continents, Medal, Gender) %>%
  summarise(Total_Medals = sum(Count), .groups = "drop")

heatmap_data

# Create heatmap with gender facets
ggplot(heatmap_data, aes(x = Continents, y = Medal, fill = Total_Medals)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +  # Colorblind-friendly palette
  labs(title = "Heatmap of Medal Counts by Gender, Continent, and Medal Type",
       x = "Continents",
       y = "Medal Type",
       fill = "Total Medals") +
  facet_wrap(~ Gender) +  # Add facets for gender
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

#Barchart male and female participation group by continent wise
medal_summary_long %>%
  group_by(Continents, Gender, Medal) %>%
  summarise(Count = sum(Count)) %>%
  ungroup() %>%
  mutate(GenMed = interaction(Gender, Medal),
         Percent = Count / sum(Count) * 100) %>%
  ggplot(aes(x = Continents, y = Count, fill = GenMed)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Medals by Continents, Gender, and Medal Type",
       x = "Continents", 
       y = "Count of Medals",
       fill = "Gender and Medal Type") +
  scale_fill_manual(values = c("Female.Gold" = "darkgoldenrod1", "Male.Gold" = "darkgoldenrod2",
                               "Female.Silver" = "lightgray", "Male.Silver" = "gray",
                               "Female.Bronze" = "saddlebrown", "Male.Bronze" = "sienna")) +
  theme_minimal() +
  theme(legend.position = "bottom")
