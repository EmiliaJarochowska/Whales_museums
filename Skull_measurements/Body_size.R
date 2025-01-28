#Estimated body size in harbour porpoises from skulls
#Model based on Anders paper: [ \log(Y) = \log(a) + b \log(L) ] 
#where 
#(Y) is the total body length (cm).
#(L) is the condylobasal length (mm).
#(a) and (b) are constants derived from the regression analysis.

#For females:
a_f = 27.39
b_f = 0.45

#Males
a_m = 22.95
b_m = 0.48

#Script for this in R

my_data <- read.csv("Skull_measurements/Skull measurements_NHM.csv")

# de-gpt-ise:
my_data$log_Predicted_Body_Length_cm <- NA
for (i in which(!is.na(my_data$Sex), arr.ind = T)) {
  ifelse(my_data$Sex[i] == "F",
         my_data$log_Predicted_Body_Length_cm[i] <- log(a_f) + b_f * log(my_data$Condylobasal_Length[i]),
         my_data$log_Predicted_Body_Length_cm[i] <- log(a_f) + b_f * log(my_data$Condylobasal_Length[i]))
}

my_data$Predicted_Body_Length_cm <- exp(1)^my_data$log_Predicted_Body_Length_cm