library(tidyverse)
data <- read_csv("dataset.csv")

# filter dataset to include only students
data_students <- data %>%
  filter(Profession == "Student")

print("Total observations in dataset:")
print(nrow(data))
print("Student observations:")
print(nrow(data_students))

# count students with cgpa equal to zero
data_students %>% 
  filter(CGPA == 0)  %>% 
  nrow()

# remove rows with missing cgpa or depression values and exclude cgpa of zero
data_clean <- data_students %>% filter(!is.na(CGPA) & !is.na(Depression) & CGPA != 0)

print("Clean observations (no missing data):")
print(nrow(data_clean))

# compare row counts before and after cleaning
nrow(data)          # original
nrow(data_clean)    # cleaned


# compute overall cgpa statistics
print("Overall CGPA Summary:")
summary(data_clean$CGPA)

# calculate cgpa statistics grouped by depression status
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

# separate cgpa values by depression status for distribution analysis
cgpa_not_depressed <- data_clean$CGPA[data_clean$Depression == 0]
cgpa_depressed <- data_clean$CGPA[data_clean$Depression == 1]
cgpa_overall <- data_clean$CGPA

# create histogram showing overall cgpa distribution with normal curve overlay
print("Histogram of Cumulative Grade Point Average (CGPA) of Depressed and Non-depressed University students in India")
h <- hist(cgpa_overall,
          main = "Distribution of CGPA Among Depressed and Non-Depressed Indian University Students",
          xlab = "CGPA (Cumulative Grade Point Average)",
          ylab = "Frequency",
          col = "lightblue",
          border = "darkblue",
          breaks = 20,
          freq = TRUE)

mean_val <- mean(cgpa_overall, na.rm = TRUE)
sd_val <- sd(cgpa_overall, na.rm = TRUE)

# scale normal curve to match histogram frequency
x <- seq(min(cgpa_overall), max(cgpa_overall), length = 100)
y <- dnorm(x, mean_val, sd_val) * length(cgpa_overall) * diff(h$breaks)[1]

# add normal curve to histogram
lines(x, y, col = "red", lwd = 2)

# create boxplot comparing cgpa between depression groups
print("Boxplot of Cumulative Grade Point Average (CGPA) of Depressed and Non-depressed University students in India")
boxplot(CGPA ~ Depression,
        data = data_clean,
        main = "Comparison of Indian University Students’ CGPA by Depression Status",
        xlab = "Depression Status",
        ylab = "CGPA (Cumulative Grade Point Average)",
        names = c("Not Depressed", "Depressed"),
        col = c("lightblue", "lightcoral"),
        border = c("darkblue", "darkred"))


# create pie chart showing distribution of depression status with percentages
counts <- table(data_clean$Depression)
labels <- c("Non-Depressed", "Depressed")
percentages <- round(100 * counts / sum(counts), 1)
labels_with_pct <- paste0(labels, "\n", counts, " (", percentages, "%)"))

# generate pie chart with labeled segments

# Create pie chart
pie(
  counts,
  main = "Distribution of Depression Status Among Indian University Students",
  col = rainbow(length(counts)),
  labels = labels_with_pct
)

# perform wilcoxon rank-sum test to compare median cgpa between groups
print("Wilcoxon Rank-Sum Test:")
wilcox_test_result <- wilcox.test(CGPA ~ Depression, data = data_clean)
print(wilcox_test_result)
