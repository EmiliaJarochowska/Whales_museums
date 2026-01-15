# Load necessary library
library(dplyr)
setwd("C:/Users/Rahma022/Desktop")
# Load the dataset
# Replace 'your_data.csv' with the path to your CSV file
data <- read.csv('Physeter_tooth_sizes.csv')

# Filter out rows where TL is NA
filtered_data <- data %>%
  filter(!is.na(TL..cm.))

# Display the filtered data
print("Filtered Data:")
print(filtered_data)

#Plot relationship between Lenght and TL, and Width and TL respectively
library(ggplot2)
ggplot(filtered_data, aes(x = Length..cm., y = TL..cm.)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Length vs TL", x = "Length (cm)", y = "TL (cm)")

ggplot(filtered_data, aes(x = Width..cm., y = TL..cm.)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Width vs TL", x = "Width (cm)", y = "TL (cm)")

# Calculate Spearman correlation for Length and TL
correlation_length <- cor(filtered_data$Length..cm., filtered_data$TL..cm., method = "spearman")

# Calculate Spearman correlation for Width and TL
correlation_width <- cor(filtered_data$Width..cm., filtered_data$TL..cm., method = "spearman")

# Display the results
cat(sprintf("Spearman Correlation between Length and TL: %.3f\n", correlation_length))
#Spearman Correlation between Length and TL: -0.500, negative correlation between TL and Length
cat(sprintf("Spearman Correlation between Width and TL: %.3f\n", correlation_width))
#Spearman Correlation between Width and TL: -0.500, negative correlation between TL and Width

#Linear Regression Analysis
model_length <- lm(TL..cm. ~ Length..cm., data = filtered_data)
summary(model_length)
#Residuals:
#1      2      3 
#-42.55 -76.59 119.14 

#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)
#(Intercept)  1313.97    1348.93   0.974    0.508
#Length..cm.   -31.46     147.40  -0.213    0.866

#Residual standard error: 147.9 on 1 degrees of freedom
#Multiple R-squared:  0.04356,	Adjusted R-squared:  -0.9129 
#F-statistic: 0.04555 on 1 and 1 DF,  p-value: 0.8661

model_width <- lm(TL..cm. ~ Width..cm., data = filtered_data)
summary(model_width)
#Residuals:
#  1      2      3 
#-75.96  31.65  44.31 

#Coefficients:
  #Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1342.8      254.3   5.280    0.119
#Width..cm.    -139.4      109.6  -1.272    0.424

#Residual standard error: 93.47 on 1 degrees of freedom
#Multiple R-squared:  0.618,	Adjusted R-squared:  0.2359 
#F-statistic: 1.618 on 1 and 1 DF,  p-value: 0.4242



