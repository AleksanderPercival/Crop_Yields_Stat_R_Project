dolnoslaskie <- c(9.25, 8.51, 8.15, 7.76, 8.77, 9.91, 8.46, 6.25, 7.39, 9.08, 
                  9.65, 9.91, 8.94, 8.02, 8.46, 8.45, 7.34, 10.22, 8.08, 8.77, 
                  9.48, 9.29, 7.25, 8.97, 7.78, 6.21, 7.21, 7.75, 7.46, 7.14, 
                  8.4, 6.43, 6.3, 8.71, 7.75, 8.58, 8.36, 7.26, 7.19, 7.28, 
                  8.35, 8.06, 9.89, 9.05, 8.56, 8.51, 8.05, 6.17)

lubuskie <- c(6.93, 8.16, 9.14, 9.01, 8.31, 10.39, 7.08, 8.52, 8.5, 9.58, 
              8.48, 8.65, 8.51, 8.99, 8.83, 8.28, 7.97, 7.58, 6.46, 5.79, 
              9.53, 7.27, 8.22, 7.26, 8.08, 10.3, 7.96, 7.74, 6.8, 5.58, 
              6.19, 8.89, 8.91, 7.25, 8.24, 9.15, 9.34, 7.62, 9.05, 9.02, 
              7.78, 7.92, 6.8, 8.73, 6.31, 5.81, 8.78)


# POINT 1: BOXPLOTS

custom_boxplot <- function(x, plot_title) {
  xx <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  
  ll <- list(x, xx)
  
  boxplot(ll, main = plot_title, names = c('Median', 'Mean'), 
          col = c("lightblue", "lightgreen"))
}

par(mfrow = c(1, 2))
custom_boxplot(dolnoslaskie, "Dolnoslaskie - Yield")
custom_boxplot(lubuskie, "Lubuskie - Yield")
par(mfrow = c(1, 1))


# POINT 2: HISTOGRAMS AND STATISTICS

# Histograms
par(mfrow = c(1, 2))
hist(dolnoslaskie, main = "Histogram - Dolnoslaskie", xlab = "Yield (t/ha)", col = "lightblue")
hist(lubuskie, main = "Histogram - Lubuskie", xlab = "Yield (t/ha)", col = "lightgreen")
par(mfrow = c(1, 1))

# A) Raw Data
calc_stats_raw <- function(x) {
  n <- length(x)
  mean_val <- mean(x)
  var_val <- var(x)
  sd_val <- sd(x)
  
  sd_pop <- sqrt(sum((x - mean_val)^2) / n)
  m3 <- sum((x - mean_val)^3) / n
  m4 <- sum((x - mean_val)^4) / n
  
  skewness <- m3 / (sd_pop^3)
  kurtosis <- m4 / (sd_pop^4) - 3
  
  return(c(Mean = mean_val, Variance = var_val, SD = sd_val, 
           Skewness = skewness, Kurtosis = kurtosis))
}

# B) Grouped Data
calc_stats_grouped <- function(x) {
  h <- hist(x, plot = FALSE)
  mids <- h$mids
  counts <- h$counts
  n <- sum(counts)
  
  mean_g <- sum(mids * counts) / n
  
  var_g <- sum(counts * (mids - mean_g)^2) / (n - 1)
  sd_g <- sqrt(var_g)
  
  sd_pop_g <- sqrt(sum(counts * (mids - mean_g)^2) / n)
  m3_g <- sum(counts * (mids - mean_g)^3) / n
  m4_g <- sum(counts * (mids - mean_g)^4) / n
  
  skewness_g <- m3_g / (sd_pop_g^3)
  kurtosis_g <- m4_g / (sd_pop_g^4) - 3
  
  return(c(Mean_Grouped = mean_g, Variance_Grouped = var_g, SD_Grouped = sd_g, 
           Skewness_Grouped = skewness_g, Kurtosis_Grouped = kurtosis_g))
}

# Plot

print("--- STATS: DOLNOSLASKIE ---")
print("A) Raw Data:")
print(calc_stats_raw(dolnoslaskie))
print("B) Grouped Data:")
print(calc_stats_grouped(dolnoslaskie))

print("--- STATS: LUBUSKIE ---")
print("A) Raw Data:")
print(calc_stats_raw(lubuskie))
print("B) Grouped Data:")
print(calc_stats_grouped(lubuskie))


# POINT 3: LILLIEFORS NORMALITY TEST

lilliefors_mc_test <- function(x, n_sim = 10000) {
  n <- length(x)
  ks_result <- suppressWarnings(ks.test(x, "pnorm", mean(x), sd(x)))
  d_stat <- unname(ks_result$statistic)
  
  sim_d_stats <- replicate(n_sim, {
    sim_data <- rnorm(n)
    suppressWarnings(ks.test(sim_data, "pnorm", mean(sim_data), sd(sim_data))$statistic)
  })
  
  p_val <- mean(sim_d_stats >= d_stat)
  
  return(c(D_Statistic = d_stat, p_value = p_val))
}

print("--- 3. LILLIEFORS TEST FOR NORMALITY ---")
print("H0: Distribution is normal (alpha = 0.05)")
print("Dolnoslaskie:")
print(lilliefors_mc_test(dolnoslaskie))
print("Lubuskie:")
print(lilliefors_mc_test(lubuskie))
# If p_value > 0.05, we do not reject H0 (it is normal)


# POINT 4: ONE-SAMPLE T-TEST FOR MEAN

# H0: mean_dolnoslaskie = 8.4
# H1: mean_dolnoslaskie != 8.4 (alpha = 0.05)

print("--- 4. TEST FOR MEAN (DOLNOSLASKIE = 8.4) ---")
test_mean_doln <- t.test(dolnoslaskie, mu = 8.4, conf.level = 0.95)
print(test_mean_doln)


# POINT 5: CHI-SQUARE TEST FOR STANDARD DEVIATION

# H0: sd_lubuskie = 1.4  -> var = 1.96
# H1: sd_lubuskie != 1.4

variance_test <- function(x, sigma_null) {
  n <- length(x)
  var_x <- var(x)
  var_null <- sigma_null^2
  
  chi_stat <- (n - 1) * var_x / var_null
  p_val <- 2 * min(pchisq(chi_stat, df = n - 1), 1 - pchisq(chi_stat, df = n - 1))
  
  return(c(Chi_Square_Stat = chi_stat, p_value = p_val))
}

print("--- 5. TEST FOR SD (LUBUSKIE = 1.4) ---")
print(variance_test(lubuskie, sigma_null = 1.4))


# POINT 6: TWO-SAMPLE T-TEST (WELCH)

# Are yields in Lubuskie LOWER than Dolnoslaskie?
# H0: mean_lubuskie >= mean_dolnoslaskie
# H1: mean_lubuskie < mean_dolnoslaskie

print("--- 6. TWO-SAMPLE TEST (LUBUSKIE < DOLNOSLASKIE) ---")
test_two_means <- t.test(lubuskie, dolnoslaskie, alternative = "less", var.equal = FALSE)
print(test_two_means)


# POINT 7: PERMUTATION TEST FOR POINT 6

# Instead of assuming distribution, we randomly shuffle labels many times.
permutation_test <- function(x, y, n_sim = 10000) {
  obs_diff <- mean(x) - mean(y) 
  pooled_data <- c(x, y)
  n_x <- length(x)
  
  sim_diffs <- replicate(n_sim, {
    shuffled <- sample(pooled_data)
    mean(shuffled[1:n_x]) - mean(shuffled[(n_x + 1):length(pooled_data)])
  })
  
  p_val <- mean(sim_diffs <= obs_diff)
  
  return(c(Observed_Difference = obs_diff, p_value = p_val))
}

print("--- 7. PERMUTATION TEST ---")
print(permutation_test(lubuskie, dolnoslaskie))