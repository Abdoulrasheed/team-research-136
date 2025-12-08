library(tidyverse)
data <- read_csv("dataset.csv")

# Filter students only??
data_students <- data %>%
  filter(Profession == "Student")

print("Total observations in dataset:")
print(nrow(data))
print("Student observations:")
print(nrow(data_students))

#find number of students with CGPA = 0
data_students %>% 
  filter(CGPA == 0)  %>% 
  nrow()

# Clean data; Remove any rows with missing CGPA or Depression values, and remove CGPA of 0
data_clean <- data_students %>% filter(!is.na(CGPA) & !is.na(Depression) & CGPA != 0)

print("Clean observations (no missing data):")
print(nrow(data_clean))

#to check for noisy values within the dataset in every column
for (col in names(data_clean)) {
  cat("-----", col, "-----\n")
  print(unique(data_clean[[col]]))
  cat("\n")
}

#check for the total number of rows removed from the dataset after data cleaning and filterng
nrow(data)          # original
nrow(data_clean)    # cleaned

rows_removed <- data %>% 
  filter(
    Profession != "Student" |
      CGPA == 0 |
      is.na(CGPA) |
      is.na(Depression) |
      !City %in% valid_cities |
      Degree == "'Class 12'"    # note the quotes around Class 12
  ) %>% 
  nrow()

rows_removed

# Overall CGPA statistics
print("Overall CGPA Summary:")
summary(data_clean$CGPA)

# CGPA by depression status
#adding Q1, Q2, IQR and add the title to the table
print("CGPA by Depression Status (descriptive statistics)")
cgpa_by_depression <- data_clean %>%
  group_by(Depression) %>%
  summarise(
    n = n(),
    Mean = mean(CGPA),
    SD = sd(CGPA),
    Median = median(CGPA),
    Q1 = quantile(CGPA, 0.25),
    Q3 = quantile(CGPA, 0.75),
    IQR = IQR(CGPA),
    Min = min(CGPA),
    Max = max(CGPA)
  )
print(cgpa_by_depression)

# Check distribution (normality assessment)
# Separate CGPA by depression status using base R bracket notation
cgpa_not_depressed <- data_clean$CGPA[data_clean$Depression == 0]
cgpa_depressed <- data_clean$CGPA[data_clean$Depression == 1]
cgpa_overall <- data_clean$CGPA

# Overall CGPA Histogram (both depressed and non-depressed students) - bell curve overlay with frequency
print("Histogram of Cumulative Grade Point Average (CGPA) of Depressed and Non-depressed University/College students in India")
h <- hist(cgpa_overall,
          main = "CGPA Distribution among Depressed and Non-depressed students",
          xlab = "CGPA (cumulative grade point average)",
          col = "lightblue",
          border = "darkblue",
          breaks = 20,
          freq = TRUE)

mean_val <- mean(cgpa_overall, na.rm = TRUE)
sd_val <- sd(cgpa_overall, na.rm = TRUE)

# Scale the bell curve to match frequency
x <- seq(min(cgpa_overall), max(cgpa_overall), length = 100)
y <- dnorm(x, mean_val, sd_val) * length(cgpa_overall) * diff(h$breaks)[1]

# Overlay curve
lines(x, y, col = "red", lwd = 2)

# Boxplot for the CGPA of depressed and non-depressed students created
# depressed students perform slightly better academically
print("Boxplot of Cumulative Grade Point Average (CGPA) of Depressed and Non-depressed University/College students in India")
boxplot(CGPA ~ Depression,
        data = data_clean,
        main = "Comparison of University Student's CGPA by Depression Status",
        xlab = "Depression Status",
        ylab = "CGPA (Cumulative Grade Point Average)",
        names = c("Not Depressed", "Depressed"),
        col = c("lightblue", "lightcoral"),
        border = c("darkblue", "darkred"))


#create a pie chart for the depressed and non-depressed students with percentages
#pie chart with proper labels
# Count depression status
counts <- table(data_clean$Depression)

# Labels
labels <- c("Non-Depressed", "Depressed")

# Calculate percentages
percentages <- round(100 * counts / sum(counts), 1)  # rounded to 1 decimal

# Combine labels with counts and percentages
labels_with_pct <- paste0(labels, "\n", counts, " (", percentages, "%)")

# Create pie chart
pie(
  counts,
  main = "Distribution of Depression Status of University Students",
  col = rainbow(length(counts)),
  labels = labels_with_pct
)

# Statistical Tests
# Wilcoxon Rank-Sum Test
print("Wilcoxon Rank-Sum Test:")
wilcox_test_result <- wilcox.test(CGPA ~ Depression, data = data_clean)
print(wilcox_test_result)
