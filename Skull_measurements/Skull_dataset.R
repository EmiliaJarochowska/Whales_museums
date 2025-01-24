#Checking if skulls changed in size over a historical time period 
setwd("C:/Users/Rahma022/Documents/Skull morphometry _London_edited")
my_data <- read.csv("Skull measurements_NHM.csv")
library(dplyr)

filtered_data <- my_data %>%
  +     filter(!is.na(Year))
library(tidyr)

#a graph to see the trends over time for all characteristics
long_data <- filtered_data %>%
  +     pivot_longer(cols = c("Length_Left_Maxillae", "Length_Right_Maxillae", 
                              +                           "Rostrum_Length", "Condylobasal_Length", 
                              +                           "Width_Occipital_Condyles"),
                     +                  names_to = "Measurement", values_to = "Value")
View(long_data)
library(ggplot2)

ggplot(long_data, aes(x = Year, y = Value, color = Measurement)) +
  +     geom_point() +
  +     geom_smooth(method = "lm") +
  +     labs(title = "Skull Measurements Over Time",
             +          x = "Year", y = "Measurement Value") +
  +     theme_minimal()

#Each measured characteristic as a graph
ggplot(long_data, aes(x = Year, y = Value)) +
  +     geom_point() +
  +     geom_smooth(method = "lm", se = FALSE) +
  +     facet_wrap(~ Measurement, scales = "free_y") +
  +     labs(title = "Trends in Skull Measurements Over Time",
             +          x = "Year", y = "Measurement Value") +
      theme_minimal()

#Linear regression analysis: are the changes over time significant?
library(dplyr)
library(tidyr)

library(broom)

results <- long_data %>%
  group_by(Measurement) %>%
  do(tidy(lm(Value ~ Year, data = .))) %>%
  filter(term == "Year")

print(results)

