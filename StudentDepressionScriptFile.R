##############################################
# Appendix A: R Code for Analysis & Visualisation
##############################################

library(tidyverse)

#--------------------------------------------------
# 1. Load dataset
#--------------------------------------------------
data <- read_csv("dataset.csv")

# Filter to include only students
data_students <- data %>% 
  filter(Profession == "Student")

cat("Total observations:", nrow(data), "\n")
cat("Student observations:", nrow(data_students), "\n")

# Number of students with CGPA = 0
cat("Students with CGPA = 0:", 
    nrow(data_students %>% filter(CGPA == 0)), "\n")

#--------------------------------------------------
# 2. Clean dataset: remove missing values + CGPA = 0
#--------------------------------------------------
data_clean <- data_students %>% 
  filter(!is.na(CGPA), 
         !is.na(Depression), 
         CGPA != 0)

cat("Clean observations:", nrow(data_clean), "\n")

#--------------------------------------------------
# 3. Descriptive Statistics
#--------------------------------------------------

# Overall CGPA summary
cat("Overall CGPA Summary:\n")
print(summary(data_clean$CGPA))

# CGPA descriptive statistics by depression status
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

cat("CGPA by Depression Status:\n")
print(cgpa_by_depression)

#--------------------------------------------------
# 4. Distribution Check (Normality Visualisation)
#--------------------------------------------------

cgpa_overall <- data_clean$CGPA

# Histogram with normal curve overlay
hist_data <- hist(
  cgpa_overall,
  main = "Distribution of CGPA Among Depressed and Non-Depressed Students",
  xlab = "CGPA",
  ylab = "Frequency",
  col = "lightblue",
  border = "darkblue",
  breaks = 20,
  freq = TRUE
)

# Normal distribution curve scaled to histogram
mean_val <- mean(cgpa_overall)
sd_val <- sd(cgpa_overall)
x_vals <- seq(min(cgpa_overall), max(cgpa_overall), length = 100)
y_vals <- dnorm(x_vals, mean_val, sd_val) * length(cgpa_overall) * diff(hist_data$breaks)[1]

lines(x_vals, y_vals, col = "red", lwd = 2)

#--------------------------------------------------
# 5. Boxplot: CGPA by Depression Status
#--------------------------------------------------

boxplot(
  CGPA ~ Depression,
  data = data_clean,
  main = "CGPA Comparison by Depression Status",
  xlab = "Depression Status (0 = Not Depressed, 1 = Depressed)",
  ylab = "CGPA",
  names = c("Not Depressed", "Depressed"),
  col = c("lightblue", "lightcoral"),
  border = c("darkblue", "darkred")
)

#--------------------------------------------------
# 6. Pie Chart: Depression Distribution
#--------------------------------------------------

counts <- table(data_clean$Depression)
labels <- c("Non-Depressed", "Depressed")
percentages <- round(100 * counts / sum(counts), 1)

labels_with_pct <- paste0(labels, "\n", counts, " (", percentages, "%)")

pie(
  counts,
  main = "Distribution of Depression Status",
  col = rainbow(length(counts)),
  labels = labels_with_pct
)

#--------------------------------------------------
# 7. Hypothesis Test
# Wilcoxon Rank-Sum Test (non-parametric comparison)
#--------------------------------------------------

cat("Wilcoxon Rank-Sum Test Results:\n")
wilcox_test_result <- wilcox.test(CGPA ~ Depression, data = data_clean)
print(wilcox_test_result)

##############################################
# END OF SCRIPT
##############################################
