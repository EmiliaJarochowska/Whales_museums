library(dplyr)
library(tidyr)
library(ggplot2)
#Previously in my datasets I had combined all Scottish, British and Welsh data as "Country = United Kingdom". With the additional skull data from Edinburgh I decided to be more specific, and change it to separate countries if possible
#this is why I named the table I am using for these analyses UK_separate
data_edinburgh <- read.csv("Skull measurements_NHM_Rotterdam_Edinburgh_UKseparate.csv")

###ECS Poster
##Plot 1, with error bars

filtered_data <- data_edinburgh %>%
  filter(!is.na(Sex), !is.na(Year))

#this is because I have most data for this timeframe
filtered_data <- filtered_data %>%
  filter(Year >= 1988 & Year <= 2003)

summary_data <- filtered_data %>%
  group_by(Year, Sex) %>%
  summarise(
    mean_length = mean(Condylobasal_Length, na.rm = TRUE),
    sd_length = sd(Condylobasal_Length, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(summary_data, aes(x = Year, y = mean_length, color = Sex)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_length - sd_length, ymax = mean_length + sd_length), width = 0.2) +
  labs(title = "Temporal change in Condylobasal Length",
       x = "Year",
       y = "Mean Condylobasal Length",
       color = "Sex") +
  scale_x_continuous(breaks = seq(min(summary_data$Year), max(summary_data$Year), by = 1)) + 
  theme_minimal()

####Plot 2

# Filter data for known sex and countries
filtered_data <- data_edinburgh %>%
  filter(Sex %in% c("F", "M") & Country %in% c("Scotland", "Wales", "England", "Netherlands"))

country_order <- c("Wales", "Scotland", "England", "Netherlands")

filtered_data$Country <- factor(filtered_data$Country, levels = country_order)

sample_sizes <- filtered_data %>%
  group_by(Country, Sex) %>%
  summarize(n = n(), .groups = 'drop')

color_palette <- c("F" = "#F8766D", "M" = "#00BFC4")

# Create the boxplot
ggplot(filtered_data, aes(x = Country, y = Condylobasal_Length, fill = Sex)) +
  geom_boxplot() +
  geom_text(data = sample_sizes, aes(x = Country, y = 32, label = paste("N =", n)),
            position = position_dodge(width = 0.75), size = 3) +
  labs(title = "Condylobasal Length for female and male harbour porpoises by country",
       x = "Country",
       y = "Condylobasal length (cm)") +
  theme_minimal() +
  scale_fill_manual(values = color_palette) +
  scale_y_continuous(limits = c(14, 32), breaks = seq(15, 30, by = 5)) +
  theme(panel.grid.major = element_line(color = "lightgray", linewidth = 0.2),
        panel.background = element_rect(fill = "white"))

###Plot 3
adult_data <- data_edinburgh %>%
  filter(Life.stage == "adult")

adult_data_filtered <- adult_data %>%
  filter(!is.na(Year))

adult_data_filtered_sex <- adult_data_filtered %>%
  filter(!is.na(Sex))

mean_data <- adult_data_filtered_sex %>%
  group_by(Year, Sex) %>%
  summarize(Mean_Condylobasal_Length = mean(Condylobasal_Length, na.rm = TRUE), .groups = 'drop')

clean_data <- adult_data_filtered_sex %>%
  filter(!is.na(Size..cm.), !is.na(Condylobasal_Length))

female_data <- clean_data[clean_data$Sex == "F", ]
male_data <- clean_data[clean_data$Sex == "M", ]

# Convert Size..cm. to numeric for both female and male data
female_data$Size..cm. <- as.numeric(female_data$Size..cm.)
male_data$Size..cm. <- as.numeric(male_data$Size..cm.)

# 2 plots next to each other:
par(mfrow = c(1, 2))  

# number of observations
n_female <- nrow(female_data)
n_male <- nrow(male_data)

# females
plot(female_data$Condylobasal_Length, female_data$Size..cm.,
     xlab = "Condylobasal length (cm)",
     ylab = "Body length (cm)",
     main = "Female harbour porpoises",
     pch = 19, col = "red",
     font.lab = 2)  # makes axis titles bold
abline(lm_reverse_female, col = "red", lwd = 2)

# R² females
r_squared_female <- summary(lm_reverse_female)$r.squared

# Add relevant values
text(x = min(female_data$Condylobasal_Length), 
     y = max(female_data$Size..cm.), 
     labels = paste0("R² = ", round(r_squared_female, 3), "\n", "n = ", n_female),
     col = "red", adj = c(0, 1))

# males
plot(male_data$Condylobasal_Length, male_data$Size..cm.,
     xlab = "Condylobasal length (cm)",
     ylab = "Body length (cm)",
     main = "Male harbour porpoises",
     pch = 19, col = "blue",
     font.lab = 2)  
abline(lm_reverse_male, col = "blue", lwd = 2)

# R² males
r_squared_male <- summary(lm_reverse_male)$r.squared

# Add relevant values
text(x = min(male_data$Condylobasal_Length), 
     y = max(male_data$Size..cm.), 
     labels = paste0("R² = ", round(r_squared_male, 3), "\n", "n = ", n_male),
     col = "blue", adj = c(0, 1))

###Checking the p-value
summary_female <- summary(lm_reverse_female)
p_value_female <- summary_female$coefficients[2, 4]  


summary_male <- summary(lm_reverse_male)
p_value_male <- summary_male$coefficients[2, 4]  
print(p_value_male)
