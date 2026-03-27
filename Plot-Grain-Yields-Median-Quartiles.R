# Lower Silesian Voivodeship
lowerSilesian <- c(9.25, 8.51, 8.15, 7.76, 8.77, 9.91, 8.46, 6.25, 7.39, 9.08, 
                  9.65, 9.91, 8.94, 8.02, 8.46, 8.45, 7.34, 10.22, 8.08, 8.77, 
                  9.48, 9.29, 7.25, 8.97, 7.78, 6.21, 7.21, 7.75, 7.46, 7.14, 
                  8.4, 6.43, 6.3, 8.71, 7.75, 8.58, 8.36, 7.26, 7.19, 7.28, 
                  8.35, 8.06, 9.89, 9.05, 8.56, 8.51, 8.05, 6.17)

# Lubusz Voivodeship
lubusz <- c(6.93, 8.16, 9.14, 9.01, 8.31, 10.39, 7.08, 8.52, 8.5, 9.58, 
              8.48, 8.65, 8.51, 8.99, 8.83, 8.28, 7.97, 7.58, 6.46, 5.79, 
              9.53, 7.27, 8.22, 7.26, 8.08, 10.3, 7.96, 7.74, 6.8, 5.58, 
              6.19, 8.89, 8.91, 7.25, 8.24, 9.15, 9.34, 7.62, 9.05, 9.02, 
              7.78, 7.92, 6.8, 8.73, 6.31, 5.81, 8.78)

length(lowerSilesian)
length(lubusz)

# DATA STRUCTURE

crops_data <- data.frame(
  Crops = c(lowerSilesian, lubusz),
  Voivodeship = c(rep("lowerSilesian", length(lowerSilesian)), 
                  rep("lubusz", length(lubusz)))
)

# plot package inculsion

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# MEDIAN AND QUARTILES BOXPLOT

quartile_chart <- ggplot(crops_data, aes(x = Voivodeship, y = Crops, fill = Voivodeship)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Grain yields - Median and Quartiles",
       y = "Crops [t/ha]",
       x = "Voivodeship") +
  theme(legend.position = "none")

print(quartile_chart)

# MEAN AND STANDARD DEVIATION BOXPLOT

mean_sd_box <- function(x) {
  m <- mean(x)
  s <- sd(x)
  
  data.frame(ymin = m - s, 
             lower = m - s, 
             middle = m, 
             upper = m + s, 
             ymax = m + s)
}

mean_sd_chart <- ggplot(crops_data, aes(x = Voivodeship, y = Crops, fill = Voivodeship)) +
  stat_summary(fun.data = mean_sd_box, geom = "boxplot", alpha = 0.7, width = 0.5) +
  theme_minimal() +
  labs(title = "Grain yields - Mean and Standard Deviation",
       y = "Crops [t/ha]",
       x = "Voivodeship") +
  theme(legend.position = "none")

print(mean_sd_chart)

# HISTOGRAMS

histograms_chart <- ggplot(crops_data, aes(x = Crops, fill = Voivodeship)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7) +
  facet_wrap(~Voivodeship, ncol = 1) + 
  theme_minimal() +
  labs(title = "Empirical Distributions of Grain Yields",
       x = "Crops [t/ha]",
       y = "Frequency (Count)") +
  theme(legend.position = "none")

print(histograms_chart)

# moments package inclusion

if(!require(moments)) install.packages("moments")
library(moments)

# MEASURES FOR RAW DATA

calculate_metrics <- function(data_vector) {
  c(
    Mean = mean(data_vector),
    Median = median(data_vector),
    Variance = var(data_vector),
    Std_Deviation = sd(data_vector),
    Coef_Variation_pct = (sd(data_vector) / mean(data_vector)) * 100,
    Skewness = skewness(data_vector),
    Kurtosis = kurtosis(data_vector)
  )
}

metrics_lowerSilesian <- calculate_metrics(lowerSilesian)
metrics_lubusz <- calculate_metrics(lubusz)

raw_data_results <- data.frame(
  Metric = names(metrics_lowerSilesian),
  LowerSilesian = round(metrics_lowerSilesian, 3),
  Lubusz = round(metrics_lubusz, 3),
  row.names = NULL
)

print(raw_data_results)

# MEASURES FOR BINNED DATA

calculate_binned_metrics <- function(data_vector, bin_width = 0.5) {
  
  min_val <- floor(min(data_vector))
  max_val <- ceiling(max(data_vector))
  breaks_seq <- seq(min_val, max_val, by = bin_width)
  
  h <- hist(data_vector, breaks = breaks_seq, plot = FALSE)
  mids <- h$mids
  counts <- h$counts
  n <- sum(counts)
  
  b_mean <- sum(mids * counts) / n
  
  b_var <- sum(counts * (mids - b_mean)^2) / (n - 1) 
  b_sd <- sqrt(b_var)
  
  m3 <- sum(counts * (mids - b_mean)^3) / n
  m4 <- sum(counts * (mids - b_mean)^4) / n
  
  b_skewness <- m3 / (b_sd^3)
  b_kurtosis <- m4 / (b_sd^4)
  
  c(
    Mean = b_mean,
    Variance = b_var,
    Std_Deviation = b_sd,
    Skewness = b_skewness,
    Kurtosis = b_kurtosis
  )
}

binned_lowerSilesian <- calculate_binned_metrics(lowerSilesian)
binned_lubusz <- calculate_binned_metrics(lubusz)

binned_data_results <- data.frame(
  Metric = names(binned_lowerSilesian),
  LowerSilesian = round(binned_lowerSilesian, 3),
  Lubusz = round(binned_lubusz, 3),
  row.names = NULL
)

cat("\n BINNED DATA METRICS \n")
print(binned_data_results)

# NORMALITY TEST (KOLMOGOROV-LILLIEFORS)

if(!require(nortest)) install.packages("nortest")
library(nortest)

# Confidence level: 0.95 -> Alpha: 0.05
# H0: Data follows a normal distribution
# H1: Data does not follow a normal distribution
# Decision rule: If p-value < 0.05, reject H0

lillie_lowerSilesian <- lillie.test(lowerSilesian)
lillie_lubusz <- lillie.test(lubusz)

cat("\nLILLIEFORS TEST: LOWER SILESIAN \n")
print(lillie_lowerSilesian)

cat("\n LILLIEFORS TEST: LUBUSZ \n")
print(lillie_lubusz)

#Results:
#data:  lowerSilesian - D = 0.084753, p-value = 0.526 -> p-value > 0.05
#data:  lubusz - D = 0.083881, p-value = 0.5594 -> p-value > 0.05
#Failed to reject H0 - normal distribution can be applied to data

# PARAMETRIC HYPOTHESIS TESTING (MEAN)

# Significance level (alpha): 0.05
# H0: mu = 8.4
# H1: mu != 8.4
# Test type: Two-sided

t_test_lowerSilesian <- t.test(lowerSilesian, 
                               mu = 8.4, 
                               alternative = "two.sided", 
                               conf.level = 0.95)

cat("\n ONE-SAMPLE T-TEST: LOWER SILESIAN \n")
print(t_test_lowerSilesian)

# Results:
# t = -1.4497, df = 47, p-value = 0.1538 -> e (0.1538) > 0.05
#Failed to reject H0 - the average yield in Lower Silesian doesn't differ significantly from 8.4 t/ha

#PARAMETRIC HYPOTHESIS TESTING (STANDARD DEVIATION / VARIANCE)

# Significance level (alpha): 0.05
#Testing SD = 1.4 -> Variance = 1.96
# H0: sigma^2 = 1.96
# H1: sigma^2 != 1.96

if(!require(EnvStats)) install.packages("EnvStats")
library(EnvStats)

null_sd <- 1.4
null_variance <- null_sd^2

variance_test_lubusz <- varTest(lubusz, 
                                alternative = "two.sided", 
                                conf.level = 0.95, 
                                sigma.squared = null_variance)

cat("\n CHI-SQUARE VARIANCE TEST: LUBUSZ \n")
print(variance_test_lubusz)

# Results:
# Chi-Squared = 30.769, df = 46, p-value = 0.0826 -> 0.0826 > 0.05
#Failed to reject H0 - standard deviation of yields in Lubusz doesn't differ significantly from 1.4

# TWO-SAMPLE COMPARISON (WELCH T-TEST)

# Significance level (alpha): 0.05
# H0: mu_lubusz >= mu_lowerSilesian
# H1: mu_lubusz < mu_lowerSilesian
# Test type: One-sided (less)

comparison_test <- t.test(x = lubusz, 
                          y = lowerSilesian, 
                          alternative = "less", 
                          conf.level = 0.95)

cat("\n TWO-SAMPLE WELCH T-TEST \n")
print(comparison_test)

# Results:
# t = -0.4688324, df = 91.62844, p-value = 0.3201515 -> 0.3201515 > 0.05
#Failed to reject H0 - Lubusz yields are not significantly lower than Lower Silesian yields

#PERMUTATION TEST (RESAMPLING METHOD)

obs_diff <- mean(lubusz) - mean(lowerSilesian)

n_permutations <- 10000
perm_diffs <- numeric(n_permutations)
all_crops <- c(lubusz, lowerSilesian)
n_lubusz <- length(lubusz)

set.seed(42) 

for(i in 1:n_permutations) {
  shuffled_crops <- sample(all_crops)
  
  sim_lubusz <- shuffled_crops[1:n_lubusz]
  sim_lowerSilesian <- shuffled_crops[(n_lubusz + 1):length(all_crops)]
  
  perm_diffs[i] <- mean(sim_lubusz) - mean(sim_lowerSilesian)
}

p_value_perm <- sum(perm_diffs <= obs_diff) / n_permutations

cat("\n PERMUTATION TEST RESULTS \n")
cat("Observed Difference:", obs_diff, "\n")
cat("Permutation P-value:", p_value_perm, "\n")

# Results:
#Observed Difference: -0.105031
#Permutation P-value: 0.3216
# -0.105031 < 0.3216 -> Failed to reject H0 with permutation test as well