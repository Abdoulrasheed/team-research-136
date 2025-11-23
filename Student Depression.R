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