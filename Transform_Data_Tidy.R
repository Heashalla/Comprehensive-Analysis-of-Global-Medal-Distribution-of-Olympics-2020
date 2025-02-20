install.packages("dplyr")
install.packages("readxl")
install.packages("tidyr")

library(dplyr)    
library(readxl)   
library(tidyr)

getwd()

# Load your datasets
olympics_data <- read_excel("Olympics_Tidy.xlsx")

#Check column names olympics_dataset
colnames(olympics_data)

#Check event unique values
unique(olympics_data$Event)

#Categorize events as "Team" or "Individual"
olympics_data$Event_Type <- ifelse(grepl("Team|Relay|Mixed Team|Doubles|Quadruple|Group", olympics_data$Event), 
                                   "Team", "Individual")
print(olympics_data)

# Drop the unnecessary columns by specifying them
olympics_data <- olympics_data %>%
  select(-c(Code, Name, Age, Discipline,Rank,NOC))

#Dependent variable in the right hand side
olympics_data <- olympics_data %>%
  select(-Medal, Medal)

olympics_data

medal_winners <- olympics_data %>%
  filter(!is.na(Medal) & Medal != "NA") 

medal_winners

#For team events, group by event and ensure we only count one medal per team
medal_winners_unique <- medal_winners %>%
  
  # Create a unique identifier for team events (use country, event, and medal)
  mutate(Team_Event_ID = ifelse(Event_Type == "Team", paste(Country, Continents, Event, Medal), NA)) %>%
  
  # Remove duplicates in team events based on the unique identifier
  group_by(Team_Event_ID, Country, Gender, Event_Type, Medal) %>%
  filter(ifelse(Event_Type == "Team", row_number() == 1, TRUE)) %>%
  ungroup()

medal_winners_unique

medal_summary <- medal_winners_unique %>%
  group_by(Country, Continents,Gender, Event_Type) %>%
  summarise(
    Gold = sum(Medal == "Gold", na.rm = TRUE),
    Silver = sum(Medal == "Silver", na.rm = TRUE),
    Bronze = sum(Medal == "Bronze", na.rm = TRUE),
    Total_Medals = Gold + Silver + Bronze,  # Adding total medals
    .groups = "drop"
  )

#View the medal summary result
head(medal_summary)
View(medal_summary)


#check duplication and validation
validate_data <- function(medal_summary) {
  # Check for duplicates
  duplicate_rows <- medal_summary[duplicated(medal_summary), ]
  
  # Check for missing values
  missing_counts <- colSums(is.na(medal_summary))
  
  # Return results
  list(
    Duplicates = duplicate_rows,
    MissingValues = missing_counts
  )
}

# Run validation
validation_results <- validate_data(medal_summary)
print(validation_results)


install.packages("openxlsx")
library(openxlsx)

# Save the cleaned Olympics dataset as an Excel file
write.xlsx(medal_summary, "Final_olympics_data.xlsx")
