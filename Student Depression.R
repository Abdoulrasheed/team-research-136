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

# Check distribution (normality assessment)
# Separate CGPA by depression status using base R bracket notation
cgpa_not_depressed <- data_clean$CGPA[data_clean$Depression == 0]
cgpa_depressed <- data_clean$CGPA[data_clean$Depression == 1]
cgpa_overall <- data_clean$CGPA

# create histogram of the overall student CGPA
# Visualizations - Histograms (Distribution Check)
# Does not show a normal distribution
hist(cgpa_overall, 
     main = "CGPA Distribution among Depressed and Non-depressed students",
     xlab = "CGPA",
     ylab = "Frequency",
     col = "lightblue",
     border = "darkblue",
     breaks = 20
)

# Histogram of CGPA - non depressed students
# Does not show a normal distribution - result of histogram
hist(cgpa_not_depressed, 
     main = "CGPA Distribution - Not Depressed Students",
     xlab = "CGPA",
     ylab = "Frequency",
     col = "lightblue",
     border = "darkblue",
     breaks = 20
)

# Histogram of CGPA - depressed students
# Does not show a normal distribution - result of histogram
hist(cgpa_depressed, 
     main = "CGPA Distribution - Depressed Students",
     xlab = "CGPA",
     ylab = "Frequency",
     col = "lightcoral",
     border = "darkred",
     breaks = 20
)

# Create a boxplot - to compare CGPA of depressed and non-depressed students
# The boxplot supports the hypothesis that there may be little difference
boxplot(CGPA ~ Depression, 
        data = data_clean,
        main = "Comparison of CGPA by Depression Status",
        xlab = "Depression Status",
        ylab = "CGPA (Grade Point Average)",
        names = c("Not Depressed", "Depressed"),
        col = c("lightblue", "lightcoral"),
        border = c("darkblue", "darkred"))