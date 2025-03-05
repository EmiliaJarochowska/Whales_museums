setwd("C:/Users/Rahma022/Documents/1_Skulls/Skull morphometry_Rotterdam/Trip 3")
my_data <- read.csv("Skull measurements_NHM_Rotterdam.csv")
library(dplyr)
library(tidyr)
library(ggplot2)

#Looked at the data and realized it had extra rows which are empty, decided to remove rows 151 to 155
my_data <- my_data[-(151:155), ]

#############################UK dataset#################################
# Filter for Country (I defined United Kingdom and Scotland as UK)
uk_data <- my_data %>% filter(Country %in% c("United Kingdom", "Scotland"))
uk_data <- uk_data %>% filter(!is.na(Year))
filtered_data_uk <- uk_data[order(uk_data$Year),]
View(filtered_data_uk)

long_data_uk <- filtered_data_uk %>%
  pivot_longer(
    cols = c("Length_Left_Maxillae", "Length_Right_Maxillae", 
             "Rostrum_Length", "Condylobasal_Length", 
             "Width_Occipital_Condyles"),
    names_to = "Measurement", 
    values_to = "Value"
  )
View(long_data_uk)

#Plotted all characters across years to get a better idea of differences
ggplot(long_data_uk, aes(x = Year, y = Value, color = Measurement)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Skull Measurements Over Time",
       x = "Year", y = "Measurement") +
  theme_minimal()


#################NL dataset####################################################
# Filter for Country (Netherlands/Holland as NL)
#Have to check the Holland locations again as they sound french, but were categorized as Holland in the London dataset
nl_data <- my_data %>% filter(Country %in% c("Netherlands", "Holland"))
nl_data <- nl_data %>% filter(!is.na(Year))
filtered_data_nl <- nl_data[order(nl_data$Year),]
View(filtered_data_nl)

long_data_nl <- filtered_data_nl %>%
  pivot_longer(
    cols = c("Length_Left_Maxillae", "Length_Right_Maxillae", 
             "Rostrum_Length", "Condylobasal_Length", 
             "Width_Occipital_Condyles"),
    names_to = "Measurement", 
    values_to = "Value"
  )
View(long_data_nl)

#Same Plot as for UK, 
ggplot(long_data_nl, aes(x = Year, y = Value, color = Measurement)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Skull Measurements Over Time",
       x = "Year", y = "Measurement") +
  theme_minimal()

#######################Comparison of UK and NL data#############################
# Filter for Condylobasal_Length in both datasets
condylobasal_data_uk <- long_data_uk %>%
  filter(Measurement == "Condylobasal_Length")

condylobasal_data_nl <- long_data_nl %>%
  filter(Measurement == "Condylobasal_Length")

# Combine the two Datasets for Comparison
combined_condylobasal_data <- bind_rows(
  condylobasal_data_uk %>% mutate(Country = "UK"),
  condylobasal_data_nl %>% mutate(Country = "Netherlands")
)

# Calculate mean Condylobasal Length by Year and Country
mean_condylobasal <- combined_condylobasal_data %>%
  group_by(Year, Country) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop')

# Create the Comparison plot with one line for each Country
ggplot(mean_condylobasal, aes(x = Year, y = Mean_Value, color = Country, group = Country)) +
  geom_line(size = 1) +  # Line for mean values
  labs(title = "Mean Condylobasal Length Over Time",
       x = "Year", y = "Mean Condylobasal Length") +
  theme_minimal() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

#############Smaller Timeframe: 1846 to 1995#####################################
#Chose this Timeframe since we have some data for both regions (UK and NL)
# Calculate mean Condylobasal Length by Year and Country
mean_condylobasal <- combined_condylobasal_data %>%
  group_by(Year, Country) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop')

# Calculate mean Condylobasal Length by Year and Country
combined_condylobasal_filtered <- combined_condylobasal_data %>%
  filter(Year)

# Create the Comparison Scatter Plot with points for each Country
combined_condylobasal_filtered <- combined_condylobasal_filtered %>%
  filter(!is.na(Year) & !is.na(Value))

ggplot(combined_condylobasal_filtered, aes(x = Year, y = Value, color = Country, shape = Country)) +
  geom_point(size = 3) +  
  labs(title = "Condylobasal Length from 1846 to 1995",
       x = "Year", y = "Condylobasal Length") +
  theme_minimal() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

#####Decided to look at Condylobasal Data across all Years for UK and NL
# Create the Comparison Scatter Plot with points for each country
ggplot(combined_condylobasal_data, aes(x = Year, y = Value, color = Country, shape = Country)) +
  geom_point(size = 3) + 
  labs(title = "Condylobasal Length For All Years",
       x = "Year", y = "Condylobasal Length") +
  theme_minimal() +
  theme(
    legend.position = "top",  
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )

###Boxplots for Condylobasal Length in Dutch and UK dataset
#I decided to plot every datapoint I have, instead of just filtering the ones I am sure are adults (will do once I have the Edinburgh dataset)
# Create the Boxplot for Condylobasal Length
ggplot(combined_condylobasal_data, aes(x = Country, y = Value, fill = Country)) +
  geom_boxplot() + 
  labs(title = "Condylobasal Length for UK and Netherlands",
       x = "Country", y = "Condylobasal Length") +
  theme_minimal() +
  theme(legend.position = "none")  

