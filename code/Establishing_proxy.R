skulls <- read.csv(file="data/skulls/Skull_measurements_pooled.csv",
                   header = TRUE)

measurement_cols <- c(3:7, which(names(skulls) == "Size..cm."))
for (col in measurement_cols) {
  skulls[[col]] <- as.numeric(as.character(skulls[[col]]))
}

#### Only records for which body size is known ####

skulls <- skulls[!is.na(skulls$Size..cm.), ]
nrow(skulls[which(skulls$Size..cm. > 130), ])

# How many taxa
table(skulls$Sex, useNA = "ifany")

# Combine plotting with regression model fitting

par(mfrow = c(2, 3))

predictor_cols <- names(skulls)[3:7]

unique_countries <- unique(skulls$Country[!is.na(skulls$Country)])
country_colors <- rainbow(length(unique_countries))
names(country_colors) <- unique_countries

models <- list()

for (i in 1:length(predictor_cols)) {
  col_name <- predictor_cols[i]

  complete_data <- skulls[!is.na(skulls[[col_name]]) & !is.na(skulls$Size..cm.), ]

  formula_str <- paste("Size..cm. ~", col_name)

  # Overall model
  model_overall <- lm(as.formula(formula_str), data = complete_data)
  models[[paste0(col_name, "_overall")]] <- model_overall

  # Model for females
  data_female <- complete_data[complete_data$Sex == "F" & !is.na(complete_data$Sex), ]
  if (nrow(data_female) >= 2) {
    model_female <- lm(as.formula(formula_str), data = data_female)
    models[[paste0(col_name, "_female")]] <- model_female
  }

  # Model for males
  data_male <- complete_data[complete_data$Sex == "M" & !is.na(complete_data$Sex), ]
  if (nrow(data_male) >= 2) {
    model_male <- lm(as.formula(formula_str), data = data_male)
    models[[paste0(col_name, "_male")]] <- model_male
  }

  point_colors <- sapply(complete_data$Country, function(x) {
    if (is.na(x)) {
      return("gray")  
    } else {
      return(country_colors[x])
    }
  })

  point_shapes <- sapply(complete_data$Sex, function(x) {
    if (is.na(x)) {
      return(1)  
    } else if (x == "F") {
      return(19)  
    } else if (x == "M") {
      return(17)  
    } else {
      return(1) 
    }
  })

  plot(complete_data[[col_name]], complete_data$Size..cm.,
       xlab = col_name,
       ylab = "Size (cm)",
       main = paste(col_name, "vs Size"),
       pch = point_shapes, col = point_colors)

  if (exists("model_female")) {
    abline(model_female, col = "blue", lwd = 2, lty = 1)
  }
  if (exists("model_male")) {
    abline(model_male, col = "red", lwd = 2, lty = 1)
  }

  legend_text <- c("Females:")

  if (exists("model_female")) {
    coef_f <- coef(model_female)
    eq_f <- paste0("  y = ", round(coef_f[2], 3), "x + ", round(coef_f[1], 3))
    r2_f <- paste0("  R² = ", round(summary(model_female)$r.squared, 3))
    legend_text <- c(legend_text, eq_f, r2_f)
  } else {
    legend_text <- c(legend_text, "  N/A")
  }

  legend_text <- c(legend_text, "Males:")

  if (exists("model_male")) {
    coef_m <- coef(model_male)
    eq_m <- paste0("  y = ", round(coef_m[2], 3), "x + ", round(coef_m[1], 3))
    r2_m <- paste0("  R² = ", round(summary(model_male)$r.squared, 3))
    legend_text <- c(legend_text, eq_m, r2_m)
  } else {
    legend_text <- c(legend_text, "  N/A")
  }

  n_items <- length(legend_text)
  legend_colors <- rep(c("blue", "blue", "blue", "red", "red", "red"), length.out = n_items)
  legend_lty <- rep(c(1, 0, 0, 1, 0, 0), length.out = n_items)
  legend_lwd <- rep(c(2, 0, 0, 2, 0, 0), length.out = n_items)

  legend("topleft", legend = legend_text,
         col = legend_colors, lty = legend_lty, lwd = legend_lwd,
         bty = "n", cex = 0.7)

  if (exists("model_female")) rm(model_female)
  if (exists("model_male")) rm(model_male)
}

par(mfrow = c(1, 1))

for (col_name in names(models)) {
  print(summary(models[[col_name]]))
}
