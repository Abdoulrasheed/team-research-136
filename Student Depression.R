library(tidyverse)
data <- read_csv("dataset.csv")

# Filter students only??
data_students <- data %>% 
  filter(Profession == "Student")

print("Total observations in dataset:")
print(nrow(data))
print("Student observations:")
print(nrow(data_students))

# Clean data; Remove any rows with missing CGPA or Depression values
data_clean <- data_students %>% filter(!is.na(CGPA) & !is.na(Depression))

print("Clean observations (no missing data):")
print(nrow(data_clean))

# Overall CGPA statistics
print("Overall CGPA Summary:")
summary(data_clean$CGPA)

# CGPA by depression status
print("CGPA by Depression Status:")
cgpa_by_depression <- data_clean %>%
  group_by(Depression) %>%
  summarise(
    n = n(),
    Mean = mean(CGPA),
    SD = sd(CGPA),
    Median = median(CGPA),
    Min = min(CGPA),
    Max = max(CGPA)
  )
print(cgpa_by_depression)