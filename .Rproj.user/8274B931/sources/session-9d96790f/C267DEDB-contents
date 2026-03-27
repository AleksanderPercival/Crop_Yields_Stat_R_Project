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

# package inculsion

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

