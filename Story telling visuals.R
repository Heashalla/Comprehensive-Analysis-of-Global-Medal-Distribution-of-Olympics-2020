install.packages("RColorBrewer")
library(RColorBrewer)

olympics_data <- read_excel("Final_olympics_data.xlsx")

# Distribution of athlete participation by continents
ggplot(olympics_data, aes(x = Continents, fill = Continents)) +
  geom_bar(stat = "count", color = "black") +
  labs(title = "Distribution of athlete participation by continents", x = "Continent", y = "Count")

# Number of countries won in Olympics, grouped by Continents
ggplot(medal_data, aes(x = Continents, fill = Continents)) +
  geom_bar(stat = "count", color = "black") +
  scale_fill_brewer(palette = "Set1") +  # Use colorblind-friendly palette
  labs(title = "Number of countries won in Olympics, grouped by Continents", x = "Continent", y = "Count")

# Gender Participation of all winners
# Calculate gender data and percentages
gender_data <- olympics_data %>%
  count(Gender) %>%  # Count occurrences of each gender
  mutate(Percentage = n / sum(n) * 100)  # Calculate percentage
gender_data

# Dot plot for gender distribution
ggplot(gender_data, aes(x = Gender, y = Percentage, color = Gender)) +
  geom_point(size = 5, shape = 21, fill = "white", stroke = 1.5) +  # Dot plot points
  ggtitle("Gender Participation of all athletes Distribution") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -1, color = "black")

# Summarize the total number of Gold, Silver, and Bronze medals by continent
medal_data_summary <- olympics_data %>%
  group_by(Continents) %>%
  summarise(
    Total_Gold = sum(Gold, na.rm = TRUE),
    Total_Silver = sum(Silver, na.rm = TRUE),
    Total_Bronze = sum(Bronze, na.rm = TRUE)
  )

head(medal_data_summary)

# Summarize total counts and calculate percentages
total_medals <- data.frame(
  Medal = c("Gold", "Silver", "Bronze"),
  Count = c(sum(medal_data_summary$Total_Gold),
            sum(medal_data_summary$Total_Silver),
            sum(medal_data_summary$Total_Bronze))
)

total_medals

total_sum <- sum(total_medals$Count)
total_medals$Percentage <- total_medals$Count / total_sum * 100
print(total_medals)

# Set the correct order for the Medal column
total_medals$Medal <- factor(total_medals$Medal, levels = c("Gold", "Silver", "Bronze"))

# Create the pie chart with percentage labels
ggplot(total_medals, aes(x = "", y = Count, fill = Medal)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            fontface = "bold") +
  labs(title = "Total Medals Distribution", x = NULL, y = NULL) +
  scale_fill_manual(values = c("gold", "grey", "#cd7f32")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))









# Create heatmap data
heatmap_data <- medal_summary_sampled %>%
  pivot_longer(cols = c(Gold, Silver,Bronze), 
               names_to = "Medal", 
               values_to = "Count") %>%
  group_by(Country, Medal) %>%
  summarise(Total_Medals = sum(Count), .groups = "drop")

# Create heatmap with colorblind-friendly palette
ggplot(heatmap_data, aes(x = Country, y = Medal, fill = Total_Medals)) +
  geom_tile() +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +  # Colorblind-friendly palette
  labs(title = "Heatmap of Medal Counts by Country and Medal Type",
       x = "Country",
       y = "Medal Type",
       fill = "Total Medals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

# Install and load necessary packages if not already installed
install.packages("rworldmap")
library(rworldmap)

# Merge the medal summary data with a map data
world_map <- map_data("world")
choropleth_data <- medal_summary_sampled %>%
  group_by(Country) %>%
  summarise(Total_Medals = sum(Gold + Silver + Bronze)) %>%
  right_join(world_map, by = c("Country" = "region"))

# Plot the choropleth map
ggplot(choropleth_data, aes(x = long, y = lat, group = group, fill = Total_Medals)) +
  geom_polygon(color = "black") +
  scale_fill_viridis_c() +
  labs(title = "Choropleth Map of Medal Counts by Country",
       fill = "Total Medals") +
  theme_minimal() +
  theme(legend.position = "bottom")


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
