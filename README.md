# Triticale Yield Analysis: A Statistical Study in Base R

## Project Overview
This project provides a comprehensive statistical analysis of triticale (*pszenżyto*) yields (t/ha) across two Polish provinces: **Dolnośląskie** and **Lubuskie**. The study focuses on verifying normality, comparing central tendencies, and assessing variance through both parametric and non-parametric methods.

A key constraint of this project was the exclusive use of **Base R**. No external libraries were used, showcasing the power of R's native statistical engine and functional programming capabilities.

## Key Features
* **Custom Data Visualization:** Implemented a unique boxplot function to compare Median/Quartiles against Mean/Standard Deviation side-by-side.
* **Frequency Distribution Analysis:** Statistical measures (Mean, Variance, Skewness, Kurtosis) calculated using both raw datasets and grouped frequency distributions.
* **Monte Carlo Lilliefors Test:** Since standard R functions for the Kolmogorov-Smirnov test can be biased when parameters are estimated from the sample, I implemented a **Lilliefors test from scratch** using 10,000 Monte Carlo simulations to derive accurate p-values.
* **Inference Testing:**
    * One-sample and Two-sample (Welch) t-tests for mean yield verification.
    * Chi-square test for variance/standard deviation consistency.
* **Permutation Testing:** Implemented a non-parametric permutation test (resampling) to verify the significance of yield differences between regions, providing a robust alternative to parametric tests.

## Methodology & Implementation
The code is designed to be efficient and "R-native":
* **Vectorization:** Heavy use of vectorized operations to avoid `for` loops, improving performance and readability.
* **Resampling:** Utilized the `replicate` function for high-iteration simulations (Normality and Permutation tests).
* **Grouped Data Logic:** Leveraged the `hist()` object's underlying data to calculate descriptive statistics for frequency series without manual binning.

## Results Summary
* **Normality:** Both regions showed distributions consistent with normality (p > 0.05 in Lilliefors MC test).
* **Hypothesis Testing:** No statistically significant difference was found between the yields of the two provinces, despite a slight observed difference in sample means.
* **Consistency:** The Permutation Test results (p ≈ 0.32) closely mirrored the Welch t-test (p ≈ 0.32), validating the robustness of the findings.

## How to Run
1.  Open `analysis.R` in RStudio or any R environment.
2.  Source the script.
3.  View statistical outputs in the console and visualizations in the Plots pane.
