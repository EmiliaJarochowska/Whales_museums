#Script on skull measurements

setwd("C:/Users/Rahma022/OneDrive - Universiteit Utrecht/Desktop/{My_Projects}/Skulls/Data sheet and Script")
my_data <- read.csv("Skull measurements_all_final.csv")

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(cowplot)

# --- Visualize only the sexes based on country --- #
filtered_data <- my_data %>%
  filter(!is.na(Country),  
         !is.na(Year), 
         !is.na(Condylobasal_Length), 
         !Country %in% c("Canada", "Mauritania", "United States")) %>%
  mutate(Country = case_when(
    Country %in% c("Scorland", "Scotland") ~ "Scotland",
    Country %in% c("Holland", "Netherlands") ~ "the Netherlands",
    TRUE ~ Country
  )) %>%
  mutate(Period = case_when(
    Year >= 1840 & Year <= 1945 ~ "1840 - 1945",
    Year >= 1946 & Year <= 1980 ~ "1946 - 1980",
    Year >= 1981 & Year <= 1990 ~ "1981 - 1990",
    Year >= 1991 & Year <= 1994 ~ "1991 - 1994",
    Year >= 1995 & Year <= 1996 ~ "1995 - 1996",
    Year >= 1997 & Year <= 1999 ~ "1997 - 1999",
    Year >= 2000 & Year <= 2017 ~ "2000 - 2017",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>% 
  mutate(Sex = ifelse(is.na(Sex), "unknown", Sex))  

year_counts <- filtered_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = 'drop')

print(year_counts, row.names = FALSE)

period_counts <- filtered_data %>%
  group_by(Period) %>%
  summarise(Count = n(), .groups = 'drop')

period_levels <- c(
  "1840 - 1945",
  "1946 - 1980",
  "1981 - 1990",
  "1991 - 1994",
  "1995 - 1996",
  "1997 - 1999",
  "2000 - 2017"
)

filtered_data$Period <- factor(filtered_data$Period, levels = period_levels)

ggplot(filtered_data, aes(x = Period, y = Condylobasal_Length, color = Country, shape = Sex)) +
  geom_jitter(alpha = 0.7, width = 0.2, size = 3) +
  geom_text(data = period_counts, aes(x = Period, 
                                      y = max(filtered_data$Condylobasal_Length, na.rm = TRUE) + 1,  
                                      label = paste("N =", Count)),  
            vjust = -0.5, size = 4, color = "black", inherit.aes = FALSE) +
  labs(x = "Years", 
       y = "Condylobasal Length (cm)") +  
  scale_shape_manual(values = c("M" = 17, "F" = 16, "unknown" = 15)) +  
  scale_y_continuous(breaks = seq(0, 32, by = 0.5)[seq(1, length(seq(0, 32, by = 0.5)), by = 2)]) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#-- Condylobasal length according to country --- #
#looking more closely into the year with the most data, by comparing average condylobasal length across countries, regardless of year

filtered_data_1991_1994 <- filtered_data %>%
  filter(Year >= 1991 & Year <= 1994)

summary_stats <- filtered_data_1991_1994 %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Mean_Length = mean(Condylobasal_Length, na.rm = TRUE),
    Median_Length = median(Condylobasal_Length, na.rm = TRUE),
    SD_Length = sd(Condylobasal_Length, na.rm = TRUE)
  )

ggplot(filtered_data_1991_1994, aes(x = Country, y = Condylobasal_Length)) +
  geom_boxplot() +
  labs(title = "Condylobasal Length by Country (1991-1994)",
       x = "Country",
       y = "Condylobasal Length (cm)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######### Getting 5 subplots to see which characteristic aligns best with body length

# --- Condylobasal Length vs Body Length --- #
cb_data <- filtered_data %>%
  filter(!is.na(Condylobasal_Length), !is.na(Size..cm.)) %>%
  mutate(
    Size..cm. = gsub("[^0-9.]", "", Size..cm.)
  )

plot_cb <- ggplot(cb_data, aes(x = Condylobasal_Length, y = Size..cm.)) +
      geom_point(alpha = 0.7, color = "steelblue") +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
       labs(
            x = "Condylobasal Length (cm)",
            y = "Total Body Length (cm)",
          title = "Condylobasal vs Body Length"
        ) +
     theme_minimal()


# --- Width of Occipital Condyles vs Body Length --- #
wo_data <- filtered_data %>%
       filter(!is.na(Width_Occipital_Condyles), !is.na(Size..cm.)) %>%
       mutate(
             Size..cm. = gsub("[^0-9.]", "", Size..cm.),
             Size..cm. = as.numeric(Size..cm.)
         )

plot_wo <- ggplot(wo_data, aes(x = Width_Occipital_Condyles, y = Size..cm.)) +
       geom_point(alpha = 0.7, color = "forestgreen") +
       geom_smooth(method = "lm", se = TRUE, color = "darkred") +
       labs(
             x = "Width of Occipital Condyles (cm)",
             y = "Total Body Length (cm)",
             title = "Occipital Condyle Width vs Body Length"
         ) +
       theme_minimal()

combined <- plot_grid(plot_cb, plot_wo, labels = c("A", "B"), ncol = 2)
library(gridExtra)

# --- Condylobasal Length vs Body Length --- #
cb_data <- filtered_data %>%
      filter(!is.na(Condylobasal_Length), !is.na(Size..cm.)) %>%
      mutate(
            Size..cm. = gsub("[^0-9.]", "", Size..cm.),
            Size..cm. = as.numeric(Size..cm.)
         )

model_cb <- lm(Size..cm. ~ Condylobasal_Length, data = cb_data)
r2_cb <- summary(model_cb)$r.squared
plot_cb <- ggplot(cb_data, aes(x = Condylobasal_Length, y = Size..cm.)) +
       geom_point(alpha = 0.7, color = "steelblue") +
       geom_smooth(method = "lm", se = TRUE, color = "darkred") +
       annotate("text",
                               x = Inf, y = Inf,
                               label = paste0("R² = ", round(r2_cb, 2)),
                               hjust = 1.1, vjust = 1.5,
                               size = 5, color = "black") +
       labs(
             x = "Condylobasal Length (cm)",
             y = "Total Body Length (cm)",
             title = "Condylobasal vs Body Length"
         ) +
       theme_minimal()

# --- Width of Occipital Condyles vs Body Length --- #
wo_data <- filtered_data %>%
       filter(!is.na(Width_Occipital_Condyles), !is.na(Size..cm.)) %>%
       mutate(
             Size..cm. = gsub("[^0-9.]", "", Size..cm.),
             Size..cm. = as.numeric(Size..cm.)
         )

model_wo <- lm(Size..cm. ~ Width_Occipital_Condyles, data = wo_data)
r2_wo <- summary(model_wo)$r.squared

plot_wo <- ggplot(wo_data, aes(x = Width_Occipital_Condyles, y = Size..cm.)) +
       geom_point(alpha = 0.7, color = "forestgreen") +
       geom_smooth(method = "lm", se = TRUE, color = "darkred") +
       annotate("text",
                               x = Inf, y = Inf,
                               label = paste0("R² = ", round(r2_wo, 2)),
                               hjust = 1.1, vjust = 1.5,
                               size = 5, color = "black") +
       labs(
             x = "Width of Occipital Condyles (cm)",
             y = "Total Body Length (cm)",
             title = "Occipital Condyle Width vs Body Length"
         ) 
     theme_minimal()

grid.arrange(plot_cb, plot_wo, ncol = 2)

# Got error, had to make sure category "Size..cm." is numeric
filtered_data <- filtered_data %>%
       mutate(Size..cm. = gsub("[^0-9.]", "", Size..cm.),
                          Size..cm. = as.numeric(Size..cm.))

# creating scatter plot with regression line and R²
create_plot <- function(x_var, y_var = "Size..cm.", data, color_point = "steelblue") {
       df <- data %>% filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
       model <- lm(as.formula(paste(y_var, "~", x_var)), data = df)
       r2_val <- summary(model)$r.squared
       
         ggplot(df, aes_string(x = x_var, y = y_var)) +
             geom_point(alpha = 0.7, color = color_point) +
             geom_smooth(method = "lm", se = TRUE, color = "darkred") +
             annotate("text",
                                         x = Inf, y = Inf,
                                         label = paste0("R² = ", round(r2_val, 2)),
                                         hjust = 1.1, vjust = 1.5,
                                         size = 4, color = "black") +
             labs(x = x_var, y = "Total Body Length (cm)") +
             theme_minimal()
   }


plot_cb <- create_plot("Condylobasal_Length", data = filtered_data, color_point = "steelblue")

plot_wo <- create_plot("Width_Occipital_Condyles", data = filtered_data, color_point = "forestgreen")
plot_lm <- create_plot("Length_Left_Maxillae", data = filtered_data, color_point = "orange")
plot_rm <- create_plot("Length_Right_Maxillae", data = filtered_data, color_point = "purple")
plot_rostrum <- create_plot("Rostrum_Length", data = filtered_data, color_point = "brown")

grid.arrange(plot_cb, plot_wo, plot_lm, plot_rm, plot_rostrum, ncol = 3)

#--- Condylobasal length over time, taking sex and life stages into account ---#
my_data <- read.csv("Skull measurements_all_final.csv")

filtered_data <- my_data %>%
  filter(!is.na(Country),  
         !is.na(Year), 
         !is.na(Condylobasal_Length), 
         !is.na(Life.stage),  
         !Country %in% c("Canada", "Mauritania", "United States")) %>%
  mutate(Country = case_when(
    Country %in% c("Scorland", "Scotland") ~ "Scotland",
    Country %in% c("Holland", "Netherlands") ~ "the Netherlands",
    TRUE ~ Country
  )) %>%
  mutate(Period = case_when(
    Year >= 1840 & Year <= 1945 ~ "1840 - 1945",
    Year >= 1946 & Year <= 1980 ~ "1946 - 1980",
    Year >= 1981 & Year <= 1990 ~ "1981 - 1990",
    Year >= 1991 & Year <= 1994 ~ "1991 - 1994",
    Year >= 1995 & Year <= 1996 ~ "1995 - 1996",
    Year >= 1997 & Year <= 1999 ~ "1997 - 1999",
    Year >= 2000 & Year <= 2017 ~ "2000 - 2017",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%  
  mutate(Sex = ifelse(is.na(Sex), "unknown", Sex),  
         Size = case_when(
           Life.stage == "adult" ~ 4,        
           Life.stage == "subadult" ~ 2,     
           Life.stage == "juvenile" ~ 1.5,   
           TRUE ~ 2                           
         ))

year_counts <- filtered_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = 'drop')

print(year_counts, row.names = FALSE)

period_counts <- filtered_data %>%
  group_by(Period) %>%
  summarise(Count = n(), .groups = 'drop')

period_levels <- c(
  "1840 - 1945",
  "1946 - 1980",
  "1981 - 1990",
  "1991 - 1994",
  "1995 - 1996",
  "1997 - 1999",
  "2000 - 2017"
)

filtered_data$Period <- factor(filtered_data$Period, levels = period_levels)

ggplot(filtered_data, aes(x = Period, y = Condylobasal_Length, color = Country, shape = Sex, size = Size)) +
  geom_jitter(alpha = 0.7, width = 0.2) +  
  geom_text(data = period_counts, aes(x = Period, 
                                      y = max(filtered_data$Condylobasal_Length, na.rm = TRUE) + 1, 
                                      label = paste("N =", Count)),  
            vjust = -0.5, size = 4, color = "black", inherit.aes = FALSE) +
  labs(x = "Time Period", 
       y = "Condylobasal Length (cm)") +  
  scale_shape_manual(values = c("M" = 16, "F" = 17, "unknown" = 1)) +  
  scale_y_continuous(breaks = seq(0, 32, by = 0.5)[seq(1, length(seq(0, 32, by = 0.5)), by = 2)]) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size(range = c(1, 4)) 

# --- Removing countries, and instead focusing on genetic subpopulations --- #

my_data <- read.csv("Skull measurements_all_final.csv")

clean_data <- my_data %>%
  select(-matches("^X(\\.\\d*)?$")) %>%
  filter(
    !is.na(Genetic.subpopulation),
    !is.na(Year),
    !is.na(Condylobasal_Length)
  ) %>%
  mutate(
    Sex = as.character(Sex),
    Life.stage = as.character(Life.stage),
    Genetic.subpopulation = as.character(Genetic.subpopulation),
    
    # Replace missing values
    Sex = ifelse(is.na(Sex) | Sex == "", "N", Sex),
    Life.stage = ifelse(is.na(Life.stage) | Life.stage == "", "unknown", Life.stage),
    Genetic.subpopulation = ifelse(is.na(Genetic.subpopulation) | Genetic.subpopulation == "", "NA", Genetic.subpopulation)
  )

clean_data <- clean_data %>%
  mutate(
    Period = case_when(
      Year >= 1860 & Year <= 1969 ~ "1860–1969",
      Year >= 1970 & Year <= 1989 ~ "1970–1989",
      Year >= 1990 & Year <= 1992 ~ "1990–1992",
      Year >= 1993 & Year <= 1996 ~ "1993–1996",
      Year >= 1997 & Year <= 1999 ~ "1997–1999",
      Year >= 2000              ~ "2000–2017",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Period))

period_levels <- c(
  "1860–1969",
  "1970–1989",
  "1990–1992",
  "1993–1996",
  "1997–1999",
  "2000–2017"
)

clean_data$Period <- factor(clean_data$Period, levels = period_levels)
clean_data$Sex <- factor(clean_data$Sex, levels = c("F", "M", "N"))

clean_data_grouped <- clean_data %>%
  mutate(
    Life.stage.grouped = case_when(
      Life.stage %in% c("subadult", "juvenile") ~ "subadult/juvenile",
      TRUE ~ Life.stage
    )
  )

clean_data_grouped$Life.stage.grouped <- factor(
  clean_data_grouped$Life.stage.grouped,
  levels = c("adult", "subadult/juvenile", "neonate", "unknown")
)

size_constant <- 4  

color_mapping <- c(
  "Eastern North Sea" = "red",
  "Western North Sea" = "blue",
  "NA" = "black"
)

shape_mapping <- c(
  "F" = 17,   # triangle
  "M" = 15,   # square
  "N" = 4     # cross
)

# Transparency mapping by life stage
alpha_mapping <- c(
  "adult" = 1,
  "subadult/juvenile" = 0.7,
  "neonate" = 0.4,
  "unknown" = 0.2
)

period_counts <- clean_data_grouped %>%
  group_by(Period) %>%
  summarise(N = n(), .groups = "drop")

ggplot(clean_data_grouped, aes(
  x = Period,
  y = Condylobasal_Length,
  color = Genetic.subpopulation,
  shape = Sex,
  alpha = Life.stage.grouped
)) +
  geom_jitter(width = 0.2, height = 0, size = size_constant) +
  
  geom_text(
    data = period_counts,
    aes(
      x = Period,
      y = max(clean_data_grouped$Condylobasal_Length, na.rm = TRUE) + 1,
      label = paste0("N = ", N)
    ),
    inherit.aes = FALSE,
    vjust = 0,
    size = 4
  ) +
  
  scale_color_manual(values = color_mapping) +
  scale_shape_manual(values = shape_mapping) +
  scale_alpha_manual(values = alpha_mapping) +
  
  labs(
    x = "Years",
    y = "Condylobasal Length (cm)",
    color = "Genetic Subpopulation",
    shape = "Sex",
    alpha = "Life Stage",
    title = "Condylobasal Length by Period, Sex, and Genetic Subpopulation"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# ------------ Checking the model from Anders------------------------#
#Estimated body size in harbour porpoises from skulls
#Model based on Anders paper: [ \log(Y) = \log(a) + b \log(L) ] 
#where 
#(Y) is the total body length (cm).
#(L) is the condylobasal length (mm).
#(a) and (b) are constants derived from the regression analysis.

#For females:
#(a = 27.39)
#(b = 0.45)

#for males:
#(a = 22.95)
#(b = 0.48)

library(dplyr)

anders_constants <- list(
  F = list(a = 27.39, b = 0.45),
  M = list(a = 22.95, b = 0.48)
)

# prediction of body length
predict_body_length <- function(sex, L_mm) {
  if (sex %in% names(anders_constants)) {
    a <- anders_constants[[sex]]$a
    b <- anders_constants[[sex]]$b
    return(a * (L_mm ^ b))
  } else {
    return(NA_real_)  
  }
}

predicted_data <- my_data %>%
  filter(Sex %in% c("F", "M")) %>%
  mutate(
    Condylobasal_Length_mm = Condylobasal_Length * 10,  # changing cm to mm
    Predicted_Body_Length_cm = mapply(
      predict_body_length,
      sex = Sex,
      L_mm = Condylobasal_Length_mm
    )
  )

head(predicted_data)

library(ggplot2)
library(dplyr)
library(gridExtra)

predicted_data <- predicted_data %>%
  mutate(
    Size..cm. = as.numeric(gsub("[^0-9.]", "", Size..cm.))  
  )

# residuals (Observed - Predicted)
residual_data <- predicted_data %>%
  filter(!is.na(Size..cm.), !is.na(Predicted_Body_Length_cm)) %>%
  mutate(
    Residual = Size..cm. - Predicted_Body_Length_cm
  )

# females
plot_females <- ggplot(filter(residual_data, Sex == "F"),
                       aes(x = Predicted_Body_Length_cm, y = Residual)) +
  geom_point(color = "red", alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted Body Length (cm)",
    y = "Observed - Predicted (cm)",
    title = "Residuals for Females"
  ) +
  theme_minimal()

# males
plot_males <- ggplot(filter(residual_data, Sex == "M"),
                     aes(x = Predicted_Body_Length_cm, y = Residual)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Predicted Body Length (cm)",
    y = "Observed - Predicted (cm)",
    title = "Residuals for Males"
  ) +
  theme_minimal()

grid.arrange(plot_females, plot_males, ncol = 2)

###Results: 
# --- females ---#
#model predicts females to be ~200 cm longer than their actual observed body lengths.
#Anders constants (a = 27.39, b = 0.45) overestimate female sizes 

# --- males --- #
#body length is also overestimated

