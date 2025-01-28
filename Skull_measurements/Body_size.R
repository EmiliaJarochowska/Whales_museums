#Estimated body size in harbour porpoises from skulls
#Model based on Anders paper: [ \log(Y) = \log(a) + b \log(L) ] 
#where 
#(Y) is the total body length (cm).
#(L) is the condylobasal length (mm).
#(a) and (b) are constants derived from the regression analysis.

#For females:
#(a = 27.39)
#(b = 0.45)

#Script for this in R
setwd("C:/Users/Rahma022/Documents/Skull morphometry _London_edited")
my_data <- read.csv("Skull measurements_NHM.csv")
library(dplyr)
filtered_data <- my_data %>%
  filter(Sex == "F")

filtered_data <- filtered_data %>%
  mutate(
    log_a = log(27.39),
    Predicted_Body_Length_cm = 10^(log_a + 0.45 * log(Condylobasal_Length))
  )

head(filtered_data)

