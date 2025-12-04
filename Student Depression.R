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

# create histogram of the overall student CGPA - no bell curve overlay
# Visualizations - Histograms (Distribution Check) 
hist(cgpa_overall, 
     main = "CGPA Distribution among Depressed and Non-depressed students",
     xlab = "CGPA",
     ylab = "Frequency",
     col = "lightblue",
     border = "darkblue",
     breaks = 20
)

# Overall CGPA Histogram (both depressed and non-depressed students) - bell curve overlay with frequency
h <- hist(cgpa_overall,
          main = "CGPA Distribution",
          xlab = "CGPA",
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
boxplot(CGPA ~ Depression, 
        data = data_clean,
        main = "Comparison of CGPA by Depression Status",
        xlab = "Depression Status",
        ylab = "CGPA (Grade Point Average)",
        names = c("Not Depressed", "Depressed"),
        col = c("lightblue", "lightcoral"),
        border = c("darkblue", "darkred"))

# QQ Plot - Not Depressed
qqnorm(cgpa_not_depressed, 
       main = "Q-Q Plot: Non-Depressed Students",
       col = "darkblue",
       pch = 19)
qqline(cgpa_not_depressed, col = "red", lwd = 2)

# QQ Plot - Depressed
qqnorm(cgpa_depressed, 
       main = "Q-Q Plot: Depressed Students",
       col = "darkred",
       pch = 19)
qqline(cgpa_depressed, col = "red", lwd = 2)


# Statistical Tests
# Independent T-Test
print("Independent t-test:")
t_test_result <- t.test(CGPA ~ Depression, data = data_clean)
print(t_test_result)
 
# Wilcoxon Rank-Sum Test
print("Wilcoxon Rank-Sum Test:")
wilcox_test_result <- wilcox.test(CGPA ~ Depression, data = data_clean)
print(wilcox_test_result)
