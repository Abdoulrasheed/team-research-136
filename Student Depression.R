library(tidyverse)
data <- read_csv("dataset.csv")

# Filter students only??
data_students <- data %>%
  filter(Profession == "Student")

print("Total observations in dataset:")
print(nrow(data))
print("Student observations:")
print(nrow(data_students))

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

# find the total number of unique city values within the dataset
length(unique(data_clean$City))


#keep only valid city names and remove any noisy values
valid_cities <- c(
  "Visakhapatnam", "Bangalore", "Srinagar", "Varanasi", "Jaipur", "Pune",
  "Thane", "Chennai", "Nagpur", "Nashik", "Vadodara", "Kalyan", "Rajkot",
  "Ahmedabad", "Kolkata", "Mumbai", "Lucknow", "Indore", "Surat", "Ludhiana",
  "Bhopal", "Meerut", "Agra", "Ghaziabad", "Hyderabad", "Vasai-Virar",
  "Kanpur", "Patna", "Faridabad", "Delhi"
)

data_clean <- data_clean %>% 
  filter(City %in% valid_cities)

#find unique cities again to see whether noisy data still exists
unique(data_clean$City)

#remove 'Class 12' from Degree
data_clean <- data_clean %>%
  filter(Degree != "'Class 12'")

##find unique Degree values again to see whether noisy data still exists
unique(data_clean$Degree)

# find the total number of unique degree values within the dataset
length(unique(data_clean$Degree))

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

# create histogram of the overall student CGPA - no bell curve overlay
# Visualizations - Histograms (Distribution Check)
hist(cgpa_overall,
     main = "CGPA Distribution among Depressed and Non-depressed students",
     xlab = "CGPA cumulative grade point average",
     ylab = "Frequency",
     col = "lightblue",
     border = "darkblue",
     breaks = 20
)

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
# Independent T-Test
print("Independent t-test:")
t_test_result <- t.test(CGPA ~ Depression, data = data_clean)
print(t_test_result)



