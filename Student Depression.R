# Statistical Analysis: CGPA Differences Between Depressed and Non-Depressed Students
# Research Question: Is there a difference in mean CGPA between students who are depressed vs. not depressed?

library(tidyverse)
data <- read_csv("dataset.csv")

# Filter students only??
data_students <- data %>% 
  filter(Profession == "Student")

print("Total observations in dataset:")
print(nrow(data))
print("Student observations:")
print(nrow(data_students))