# Triticale Yield Analysis - Statistical Project in R

## Overview
This project performs a comprehensive statistical analysis of triticale crop yields across two Polish regions (Lower Silesian and Lubusz). It was developed to demonstrate proficiency in statistical computing using R, data visualization, and hypothesis testing.

## Tech Stack & Methodologies
- **Language:** R
- **Visualization:** `ggplot2` (Customized boxplots, faceted histograms)
- **Normality Testing:** `nortest` (Lilliefors test)
- **Advanced Stats:** `moments`, `EnvStats`
- **Simulations:** Custom-built **Permutation Test** (Resampling)
- **Version Control:** Git

## Key Features
- **Data Engineering:** Transformation of raw data into tidy data frames for automated pipelines.
- **Statistical Measures:** Comparative analysis of raw vs. binned data, highlighting the "grouping error" phenomenon.
- **Hypothesis Testing:**
  - One-sample t-test (Mean verification).
  - Chi-squared test (Standard deviation verification).
  - Welch’s t-test (Inter-regional comparison).
- **Brute-force Verification:** Implementation of a 10,000-iteration Permutation Test to validate parametric results.

## Visualizations
*(Note: Visuals are documented in the full report. Key findings include left-skewed distributions and platykurtic tendencies in both regions.)*

## Conclusion
The analysis proved that observed differences between regions are statistically insignificant (p > 0.05), showcasing a data-driven approach to agricultural research.
