# =====================================================================
# INSTALL REQUIRED PACKAGE (ONLY ggplot2)
# =====================================================================
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# =====================================================================
# 1. LOAD DATA (NO READR)
# =====================================================================

cat("Select your CSV file...\n")
file_name <- file.choose()

df <- read.csv(file_name, header = TRUE)

cat("\nFile loaded:", file_name, "\n")
cat("\n--- DataFrame Head ---\n")
print(head(df))

cat("\n--- DataFrame Structure ---\n")
print(str(df))



# =====================================================================
# 2. DATA CLEANING
# =====================================================================

original_count <- nrow(df)
df_cleaned <- df[df$Time_Spent_Hours >= 0, ]
cleaned_count <- nrow(df_cleaned)

cat("\n--- Data Cleaning Summary ---\n")
cat("Original:", original_count, "\n")
cat("Negative removed:", original_count - cleaned_count, "\n")
cat("Remaining:", cleaned_count, "\n")



# =====================================================================
# 3. Course_Type Summary
# =====================================================================

cat("\n--- Course_Type Frequency ---\n")
print(table(df_cleaned$Course_Type))

cat("\n--- Course_Type Summary ---\n")
print(summary(as.factor(df_cleaned$Course_Type)))



# =====================================================================
# 4. DESCRIPTIVE SUMMARY
# =====================================================================

cat("\n--- Descriptive Statistics (Time Spent) ---\n")
print(summary(df_cleaned$Time_Spent_Hours))



# =====================================================================
# 5. GROUP SUMMARY (NO dplyr)
# =====================================================================

cat("\n--- Summary by Completion Status ---\n")

completion_groups <- split(df_cleaned, df_cleaned$Completed)

for (group in names(completion_groups)) {
  cat("\nCompletion:", group, "\n")
  print(summary(completion_groups[[group]]$Time_Spent_Hours))
}



# =====================================================================
# 6. ONE-SAMPLE T-TEST
# =====================================================================

mu_0 <- 15
x <- df_cleaned$Time_Spent_Hours

t_result <- t.test(x, mu = mu_0, alternative = "greater")

cat("\n--- One-Sample T-Test (mean > 15) ---\n")
print(t_result)

if (t_result$p.value < 0.05) {
  cat("\nConclusion: Reject H0 — average time spent > 15 hours.\n")
} else {
  cat("\nConclusion: Fail to Reject H0 — no strong evidence it's > 15 hours.\n")
}



# =====================================================================
# 7. VISUALIZATIONS
# =====================================================================

# 7.1 Histogram + Density
p1 <- ggplot(df_cleaned, aes(Time_Spent_Hours)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  geom_vline(xintercept = mu_0, linetype = "dashed") +
  geom_vline(xintercept = mean(df_cleaned$Time_Spent_Hours),
             color = "blue", linewidth = 1) +
  ggtitle("Distribution of Time Spent (Hours)")

print(p1)
ggsave("histogram_density.png", p1)



# 7.2 Boxplot by Completion
p2 <- ggplot(df_cleaned, aes(x = Completed, y = Time_Spent_Hours, fill = Completed)) +
  geom_boxplot() +
  ggtitle("Time Spent by Course Completion Status")

print(p2)
ggsave("boxplot_completion.png", p2)



# 7.3 T-test Visualization
sample_mean <- mean(df_cleaned$Time_Spent_Hours)
sample_se <- sd(df_cleaned$Time_Spent_Hours) / sqrt(length(df_cleaned$Time_Spent_Hours))

df_plot <- data.frame(
  label = "Sample Mean",
  mean = sample_mean,
  se = sample_se
)

p3 <- ggplot(df_plot, aes(x = label, y = mean)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3) +
  geom_hline(yintercept = mu_0, linetype = "dashed", color = "red") +
  ggtitle("Sample Mean vs Hypothesized Mean (15 hours)")

print(p3)
ggsave("ttest_visualization.png", p3)
