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

#create a pie chart for the depressed and non-depressed students
#pie chart with proper labels
counts <- table(data_clean$Depression)
labels <- c("Non-Depressed", "Depressed")

pie(
  counts,
  main = "Distribution of Depression Status",
  col = rainbow(length(counts)),
  labels = paste(labels, "\n", counts)
)

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

# Statistical Tests
# Independent T-Test
print("Independent t-test:")
t_test_result <- t.test(CGPA ~ Depression, data = data_clean)
print(t_test_result)
 
# Wilcoxon Rank-Sum Test
print("Wilcoxon Rank-Sum Test:")
wilcox_test_result <- wilcox.test(CGPA ~ Depression, data = data_clean)
print(wilcox_test_result)