############################################################
#SKULL ANALYSIS PIPELINE
############################################################

############################
# 1. Libraries
############################
library(tidyverse)
library(janitor)
library(patchwork)
library(dplyr)
library(ggplot2)
library(grid)
library(minpack.lm)
library(grid)
library(gridExtra)
library(officer)
library(flextable)
library(tseries)
library(stringr)
library(scales)
library(broom)
library(gt)
library(purrr)

theme_set(theme_bw())

############################
# 2. Load data
############################

setwd
raw_data <- read_csv("Skull measurements_all_final.csv") %>%
  clean_names()

############################
# 3. Exclude those specimens, that didn't make sense to include
############################

excluded_ids <- c(
  "1993.73.14",
  "SW.1939-17",
  "Z.1992.151.42",
  "SW.1954-15"
)

valid_sex <- c("M","F")
valid_life_stage <- c("juvenile","subadult","adult")

############################
# 4. Clean the data
############################

data_clean <- raw_data %>%
  mutate(
    size = readr::parse_number(as.character(size)),
    condylobasal_length = readr::parse_number(as.character(condylobasal_length)),
    length_left_maxillae = readr::parse_number(as.character(length_left_maxillae)),
    length_right_maxillae = readr::parse_number(as.character(length_right_maxillae)),
    rostrum_length = readr::parse_number(as.character(rostrum_length)),
    width_occipital_condyles = readr::parse_number(as.character(width_occipital_condyles)),
    year = readr::parse_number(as.character(year)),
    
    sex = str_trim(as.character(sex)),
    life_stage = tolower(str_trim(as.character(life_stage)))
  ) %>%
  filter(!registration_nr %in% excluded_ids)

############################################################
# GRAPH SET 1:  METADATA VISUALIZATION
############################################################

metadata_data <- data_clean

# Create decade grouping
metadata_data <- metadata_data %>%
  mutate(decade = floor(year/10)*10)

calc_metadata <- function(df, var){
  
  df %>%
    group_by(decade) %>%
    summarise(
      pct = mean(!is.na(.data[[var]]))*100,
      n = n(),
      .groups="drop"
    )
}

meta_country <- calc_metadata(metadata_data,"country") %>%
  mutate(category="Country")

meta_lifestage <- calc_metadata(metadata_data,"life_stage") %>%
  mutate(category="Life stage")

meta_sex <- calc_metadata(metadata_data,"sex") %>%
  mutate(category="Sex")

meta_size <- calc_metadata(metadata_data,"size") %>%
  mutate(category="Size")

meta_all <- bind_rows(meta_country,meta_lifestage,meta_sex,meta_size)

# Define consistent x-axis breaks, I decided for every 20 years
year_breaks <- seq(1840, 2020, by = 20)

plot_metadata <- ggplot(meta_all,
                        aes(decade, pct, fill = category)) +
  
  geom_col(show.legend = FALSE) +
  
  # N labels for decades with bars
  geom_text(
    data = meta_all %>% filter(pct > 0),
    aes(label = paste0("N=", n)),
    vjust = 1.5,
    size = 5
  ) +
  
  # N labels for decades without bars
  geom_text(
    data = meta_all %>% filter(pct == 0),
    aes(y = -Inf, label = paste0("N=", n)),
    vjust = -0.5,
    size = 5
  ) +
  
  facet_wrap(~category, ncol = 1, scales = "free_y") +
  
  scale_x_continuous(breaks = year_breaks) +
  
  labs(
    x = "Decade",
    y = "Percentage of records with data"
  ) +
  
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold")
  )

plot_metadata

ggsave(
  filename = "metadata_plot.png",
  plot = plot_metadata,
  width = 10,        
  height = 14,       
  dpi = 600          
)

meta_table <- meta_all %>%
  select(decade, category, pct, n) %>%
  arrange(decade, category) %>%
  pivot_wider(
    names_from = category,
    values_from = c(pct, n),
    names_glue = "{.value}_{category}"
  ) %>%
  arrange(decade)

# View table
meta_table

# Optional: export for writing results
write.csv(meta_table,
          "metadata_summary_table.csv",
          row.names = FALSE)

############################################################
# GRAPH 2: GOMPERTZ GROWTH CURVES 
############################################################
df_growth <- data_clean %>%
  filter(
    sex %in% valid_sex,
    !is.na(size),
    !is.na(age),
    age > 0,
    size > 0
  ) %>%
  mutate(
    sex = recode(sex, "F"="Female", "M"="Male"),
    sex = factor(sex, levels=c("Female","Male")),
    age = as.numeric(age),
    size = as.numeric(size),
    life_stage = factor(
      life_stage,
      levels=c("juvenile","subadult","adult")
    )
  )

gompertz <- function(age, A, B, C){
  A * exp(-B * exp(-C * age))
}

start_A <- max(df_growth$size, na.rm=TRUE)
start_B <- 1
start_C <- 0.05

fit_female <- nls(
  size ~ gompertz(age, A, B, C),
  data = df_growth %>% filter(sex=="Female"),
  start = list(A=start_A, B=start_B, C=start_C),
  algorithm="port",
  lower=c(A=0, B=0, C=0)
)

fit_male <- nls(
  size ~ gompertz(age, A, B, C),
  data = df_growth %>% filter(sex=="Male"),
  start = list(A=start_A, B=start_B, C=start_C),
  algorithm="port",
  lower=c(A=0, B=0, C=0)
)

age_seq <- seq(
  min(df_growth$age, na.rm=TRUE),
  max(df_growth$age, na.rm=TRUE),
  length.out=200
)

predictions <- bind_rows(
  
  data.frame(
    age = age_seq,
    size = predict(fit_female, newdata=data.frame(age=age_seq)),
    sex = "Female"
  ),
  
  data.frame(
    age = age_seq,
    size = predict(fit_male, newdata=data.frame(age=age_seq)),
    sex = "Male"
  )
  
) %>%
  mutate(sex=factor(sex, levels=c("Female","Male")))

coef_f <- coef(fit_female)
coef_m <- coef(fit_male)

xmax <- max(df_growth$age, na.rm=TRUE)
xmin <- min(df_growth$age, na.rm=TRUE)

ymax <- max(df_growth$size, na.rm=TRUE)
ymin <- min(df_growth$size, na.rm=TRUE)

x_pad <- (xmax - xmin) * 0.18
y_pad <- (ymax - ymin) * 0.08

formula_labels <- data.frame(
  sex = c("Female","Male"),
  x = xmax + x_pad,
  y = ymin + y_pad,
  label = c(
    paste0(
      "Female Gompertz\n",
      "L(t) = ", round(coef_f["A"],1),
      " · exp(-", round(coef_f["B"],2),
      " · exp(-", round(coef_f["C"],2),"t))"
    ),
    paste0(
      "Male Gompertz\n",
      "L(t) = ", round(coef_m["A"],1),
      " · exp(-", round(coef_m["B"],2),
      " · exp(-", round(coef_m["C"],2),"t))"
    )
  )
)

n_labels <- df_growth %>%
  group_by(sex) %>%
  summarise(n=n(), .groups="drop") %>%
  mutate(
    x = xmax + x_pad,
    y = ymax - y_pad
  )

plot_gompertz <- ggplot(df_growth, aes(age, size)) +
  
  geom_point(
    aes(shape=life_stage, size=life_stage, colour=sex),
    alpha=0.85
  ) +
  
  geom_line(
    data=predictions,
    aes(age, size, colour=sex),
    linewidth=1.3
  ) +
  
  facet_wrap(~sex) +
  
  scale_shape_manual(
    values=c(
      "juvenile"=16,
      "subadult"=17,
      "adult"=15
    )
  ) +
  
  scale_size_manual(
    values=c(
      "juvenile"=2.3,
      "subadult"=2.8,
      "adult"=3.2
    )
  ) +
  
  scale_colour_manual(
    values=c(
      "Female"="#E64B35FF",
      "Male"="#4DBBD5FF"
    )
  ) +
  
  scale_x_continuous(limits=c(xmin, xmax+x_pad)) +
  scale_y_continuous(limits=c(ymin, ymax+y_pad*2)) +
  
  labs(
    x="Age (years)",
    y="Body length (cm)",
    shape="Life stage",
    size="Life stage",
    colour="Sex"
  ) +
  
  theme_minimal(base_size=16)

plot_gompertz

ggsave(
  "gompertz_growth_curves.png",
  plot_gompertz,
  width=12,
  height=6,
  dpi=600
)

############################################################
# GRAPH 3: REGRESSIONS ANALYSIS
############################################################
# Remove ONLY the specified outlier specimens
analysis_data <- data_clean %>%
  filter(!registration_nr %in% excluded_ids) %>%
  filter(
    sex %in% valid_sex,
    life_stage %in% valid_life_stage,
    !is.na(size)
  )


# ---- Plot function ----
create_plot <- function(x_var, data, color_point, show_y_label = TRUE) {
  
  if (!x_var %in% names(data)) stop(paste("Column", x_var, "does not exist."))
  
  
  df <- data[!is.na(data[[x_var]]), ]
  
  if (nrow(df) == 0) stop(paste("No data available for", x_var))
  
  model <- lm(size ~ df[[x_var]], data = df)
  r2 <- summary(model)$r.squared
  
  p_val <- summary(model)$coefficients[2,4]
  p_label <- ifelse(p_val < 0.001, "p < 0.001", paste0("p = ", round(p_val, 3)))
  
  axis_labels <- c(
    "condylobasal_length"      = "Condylobasal Length (cm)",
    "width_occipital_condyles" = "Width of Occipital Condyles (cm)",
    "length_left_maxillae"     = "Length of Left Maxilla (cm)",
    "length_right_maxillae"    = "Length of Right Maxilla (cm)",
    "rostrum_length"           = "Rostrum Length (cm)"
  )
  
  ggplot(df, aes(x = .data[[x_var]], y = size)) +
    geom_point(alpha = 0.7, color = color_point) +
    
    # BLACK regression line
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    
    annotate(
      "text",
      x = Inf,
      y = -Inf,
      label = paste0("R² = ", round(r2, 2), "\n", p_label),
      hjust = 1.1,
      vjust = -0.5,
      size = 5
    ) +
    labs(
      x = axis_labels[[x_var]],
      y = if (show_y_label) "Total Body Length (cm)" else NULL
    ) +
    theme_minimal(base_size = 16)
}


# ---- Create plots ----
plot_cb      <- create_plot("condylobasal_length",        analysis_data, "#4A90E2", TRUE) + coord_cartesian(ylim = c(0,200))
plot_wo      <- create_plot("width_occipital_condyles",   analysis_data, "#FF6F61", FALSE) + coord_cartesian(ylim = c(0,200))
plot_lm      <- create_plot("length_left_maxillae",       analysis_data, "orange", TRUE) + coord_cartesian(ylim = c(0,200))
plot_rm      <- create_plot("length_right_maxillae",      analysis_data, "purple", FALSE) + coord_cartesian(ylim = c(0,200))
plot_rostrum <- create_plot("rostrum_length",             analysis_data, "brown", TRUE) + coord_cartesian(ylim = c(0,200))


final_plot <- 
  (plot_cb | plot_wo) /
  (plot_lm | plot_rm) /
  (plot_rostrum)

measurements <- c(
  "condylobasal_length",
  "width_occipital_condyles",
  "length_left_maxillae",
  "length_right_maxillae",
  "rostrum_length"
)

# Mapping to capitalized names
measurement_names <- c(
  "Condylobasal Length",
  "Width of Occipital Condyles",
  "Length of Left Maxilla",
  "Length of Right Maxilla",
  "Rostrum Length"
)

ggsave(
  "skull_measurements_regressions.png",
  final_plot,
  width = 14,
  height = 12,
  dpi = 600,
  bg = "white"
)

lm_summary_list <- lapply(seq_along(measurements), function(i) {
  var <- measurements[i]
  display_name <- measurement_names[i]
  
  df <- analysis_data[!is.na(analysis_data[[var]]), ]
  n_obs <- nrow(df)
  model <- lm(size ~ df[[var]], data = df)
  tidy_model <- broom::tidy(model)
  r2 <- summary(model)$r.squared
  
  # Extract slope (second row), standard error, and p-value
  slope <- tidy_model$estimate[2]
  slope_se <- tidy_model$std.error[2]
  p_val <- tidy_model$p.value[2]
  
  data.frame(
    Measurement = display_name,
    N          = n_obs,
    Slope      = round(slope, 3),
    SD         = round(slope_se, 3),
    R_squared  = round(r2, 3),
    P_value    = signif(p_val, 3)
  )
})

lm_summary_table <- bind_rows(lm_summary_list)

# ---- Print table ----
print(lm_summary_table)

# ---- Convert summary table ----
table_grob <- tableGrob(lm_summary_table, rows = NULL)  # rows=NULL removes row names

# Save as PNG
png("linear_model_summary.png", width = 1400, height = 700, res = 150)
grid.newpage()
grid.draw(table_grob)
dev.off()

############################################################
# GRAPH 4: KNOWN VS. PREDICTED BODY LENGTHS
############################################################

# ---- Filter data for this analysis only ----
analysis_filtered <- analysis_data %>%
  mutate(
    life_stage = factor(life_stage, levels = c("juvenile", "subadult", "adult")),
    sex = factor(sex, levels = c("M", "F"))
  ) %>%
  filter(!is.na(life_stage))

# ---- Multi-linear regression per life stage ----
multi_model_results <- analysis_filtered %>%
  group_by(life_stage) %>%
  group_modify(~ {
    m <- lm(
      size ~ sex +
        condylobasal_length +
        length_left_maxillae +
        length_right_maxillae +
        rostrum_length +
        width_occipital_condyles,
      data = .x,
      na.action = na.exclude
    )
    
    mf <- model.frame(m)
    mf$sex <- factor(mf$sex, levels = c("M", "F"))
    
    tibble(
      size = mf$size,
      predicted = predict(m, newdata = mf),
      sex = mf$sex,
      life_stage = unique(.x$life_stage)[1],
      R2 = summary(m)$r.squared
    )
  })

# ---- Get overall model p-value per life stage ----
p_values <- analysis_filtered %>%
  group_by(life_stage) %>%
  group_modify(~ {
    m <- lm(
      size ~ sex +
        condylobasal_length +
        length_left_maxillae +
        length_right_maxillae +
        rostrum_length +
        width_occipital_condyles,
      data = .x
    )
    
    f <- summary(m)$fstatistic
    p_val <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    
    tibble(p_value = p_val)
  })

# ---- Combine R² + p-value for the labels ----
r2_labels <- multi_model_results %>%
  group_by(life_stage) %>%
  summarise(R2 = unique(R2), .groups = "drop") %>%
  left_join(p_values, by = "life_stage") %>%
  mutate(label = paste0("R² = ", round(R2, 2), "\np = ", signif(p_value, 3)))


sex_colors <- c("F" = "#E64B35FF", "M" = "#4DBBD5FF")

# ---- Plot predicted vs observed ----
p_stage_multi <- ggplot(
  multi_model_results,
  aes(x = size, y = predicted, colour = sex)
) +
  geom_point(alpha = 0.75, size = 4) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey40",
    size = 0.8
  ) +
  facet_wrap(~ life_stage) +
  geom_text(
    data = r2_labels,
    aes(x = Inf, y = -Inf, label = label),
    hjust = 1.1,
    vjust = -0.5,
    inherit.aes = FALSE,
    size = 5
  ) +
  scale_colour_manual(
    name = "Sex",
    values = sex_colors,
    labels = c("Male", "Female")
  ) +
  labs(
    x = "Observed total body length (cm)",
    y = "Predicted total body length (cm)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.spacing = unit(1.5, "lines"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    strip.background = element_rect(fill = "grey80", colour = "black"),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )

# ---- Print plot ----
print(p_stage_multi)

ggsave(
  filename = "predicted_vs_observed_plot_red_blue.png",
  plot = p_stage_multi,
  width = 10,
  height = 4,
  dpi = 600,
  bg = "white"
)

############################################################
# GRAPH 5: BODY LENGTH BY LIFE STAGE / SUBPOPULATION
############################################################

df_box <- analysis_data %>%
  filter(
    sex %in% valid_sex,
    life_stage %in% valid_life_stage,
    genetic_subpopulation %in% c("Western North Sea","Eastern North Sea")
  ) %>%
  mutate(
    life_stage = factor(
      life_stage,
      levels = c("juvenile","subadult","adult")
    ),
    sex = factor(
      sex,
      levels = c("M","F"),   # Males on top
      labels = c("Male","Female")
    ),
    genetic_subpopulation = factor(
      genetic_subpopulation,
      levels = c("Western North Sea","Eastern North Sea")
    )
  )


summary_stats <- df_box %>%
  group_by(sex, genetic_subpopulation, life_stage) %>%
  summarise(
    n = n(),
    y = max(size, na.rm = TRUE),
    .groups = "drop"
  )


plot_box_lifestage <- ggplot(
  df_box,
  aes(x = life_stage, y = size, fill = life_stage)
) +
  
  geom_boxplot(
    width = 0.7,
    colour = "black",
    outlier.shape = 16,
    outlier.size = 2
  ) +
  
  
geom_text(
  data = summary_stats,
  aes(
    x = life_stage,
    y = y,
    label = paste0("N=", n)
  ),
  vjust = -0.6,
  size = 5,
  inherit.aes = FALSE
) +
  
  facet_grid(sex ~ genetic_subpopulation) +
  
  scale_fill_manual(values = c(
    "juvenile" = "#FDD0A2",
    "subadult" = "#A1D99B",
    "adult"    = "#9ECAE1"
  )) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  
  labs(
    x = "Life stage",
    y = "Total body length (cm)"
  ) +
  
  theme_bw(base_size = 18) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = "grey90"),
    panel.spacing = unit(1, "cm"),
    legend.position = "none"
  )

plot_box_lifestage

ggsave(
  filename = "BodyLength_LifeStage_Subpopulation.png",
  plot = plot_box_lifestage,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white",
  device = "png",
  type = "cairo"
)

# ---- Create summary statistics table ----
summary_table <- df_box %>%
  group_by(sex, genetic_subpopulation, life_stage) %>%
  summarise(
    n = n(),
    mean = mean(size, na.rm = TRUE),
    median = median(size, na.rm = TRUE),
    sd = sd(size, na.rm = TRUE),
    se = sd / sqrt(n),
    
    # ---- NEW: range ----
    min = min(size, na.rm = TRUE),
    max = max(size, na.rm = TRUE),
    
    # ---- NEW: 95% CI ----
    ci_lower = mean - qt(0.975, df = n - 1) * se,
    ci_upper = mean + qt(0.975, df = n - 1) * se,
    
    .groups = "drop"
  ) %>%
  mutate(
    mean = round(mean, 1),
    median = round(median, 1),
    sd = round(sd, 1),
    se = round(se, 2),
    
    # round new columns
    min = round(min, 1),
    max = round(max, 1),
    ci_lower = round(ci_lower, 1),
    ci_upper = round(ci_upper, 1)
  )

table_plot <- tableGrob(summary_table)

ggsave(
  "BodyLength_Summary_Table.png",
  table_plot,
  width = 14,
  height = 8,
  dpi = 600
)

############################################################
# FITTING OF THE GALATIUS MODEL
############################################################

a_f <- 27.39
b_f <- 0.45

a_m <- 22.95
b_m <- 0.48


# ---- Prepare data ----
df_allometry <-
  data_clean %>%
  filter(
    sex %in% c("F","M"),
    genetic_subpopulation %in%
      c("Western North Sea","Eastern North Sea"),
    !is.na(size),
    !is.na(condylobasal_length),
    condylobasal_length > 0
  ) %>%
  mutate(
    sex = recode(sex,"F"="Female","M"="Male"),
    genetic_subpopulation =
      factor(genetic_subpopulation,
             levels=c("Western North Sea",
                      "Eastern North Sea"))
  )

# ---- Published model by Galatius ----

df_allometry <-
  df_allometry %>%
  mutate(
    expected_length =
      case_when(
        sex=="Female" ~
          exp((log(condylobasal_length)-log(a_f))/b_f),
        sex=="Male" ~
          exp((log(condylobasal_length)-log(a_m))/b_m)
      ),
    
    deviation =
      log(expected_length) -
      mean(log(expected_length), na.rm=TRUE)
  )

# ---- Test data mismatch with model ----

anova_paper <-
  aov(deviation ~ sex * genetic_subpopulation,
      data=df_allometry)

summary(anova_paper)

# ---- Visualize the deviation from the model ----

plot_paper <-
  ggplot(
    df_allometry,
    aes(genetic_subpopulation,
        deviation,
        fill=sex)
  ) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_boxplot(width=0.65, alpha=0.75) +
  facet_wrap(~sex) +
  labs(
    x="Genetic subpopulation",
    y="Original model"
  ) +
  theme_bw(base_size=18) +
  theme(
    legend.position="none",
    strip.text=element_text(face="bold")
  )


# ---- Population-specific allometric models ----

model_west <-
  lm(
    log(size) ~ log(condylobasal_length) + sex,
    data =
      df_allometry %>%
      filter(genetic_subpopulation=="Western North Sea")
  )

model_east <-
  lm(
    log(size) ~ log(condylobasal_length) + sex,
    data =
      df_allometry %>%
      filter(genetic_subpopulation=="Eastern North Sea")
  )

# ---- Extract adjusted parameters + sex correction ----
a_west <- exp(coef(model_west)["(Intercept)"])
b_west <- coef(model_west)["log(condylobasal_length)"]
sex_factor_west <- exp(coef(model_west)["sexMale"])

a_east <- exp(coef(model_east)["(Intercept)"])
b_east <- coef(model_east)["log(condylobasal_length)"]
sex_factor_east <- exp(coef(model_east)["sexMale"])

# ---- Predict using calibrated models ----
df_allometry <-
  df_allometry %>%
  mutate(
    predicted_population =
      case_when(
        genetic_subpopulation=="Western North Sea" &
          sex=="Female" ~
          a_west *
          condylobasal_length^b_west,
        
        genetic_subpopulation=="Western North Sea" &
          sex=="Male" ~
          a_west *
          condylobasal_length^b_west *
          sex_factor_west,
        
        genetic_subpopulation=="Eastern North Sea" &
          sex=="Female" ~
          a_east *
          condylobasal_length^b_east,
        
        genetic_subpopulation=="Eastern North Sea" &
          sex=="Male" ~
          a_east *
          condylobasal_length^b_east *
          sex_factor_east
      ),
    
    deviation_population =
      log(size) -
      log(predicted_population)
  )


# ---- Validate calibrated model ----
anova_population <-
  aov(
    deviation_population ~
      sex * genetic_subpopulation,
    data=df_allometry
  )

summary(anova_population)


# ---- Visualise calibrated model fit ----
plot_calibrated <-
  ggplot(
    df_allometry,
    aes(genetic_subpopulation,
        deviation_population,
        fill=sex)
  ) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_boxplot(width=0.65, alpha=0.75) +
  facet_wrap(~sex) +
  labs(
    x="Genetic subpopulation",
    y="Adjusted model"
  ) +
  theme_bw(base_size=18) +
  theme(
    legend.position="none",
    strip.text=element_text(face="bold")
  )


# ---- Combine original Galatius and my adjusted model ----
plot_combined <-
  plot_paper /
  plot_calibrated +
  plot_layout(heights=c(1,1)) +
  plot_annotation(tag_levels="A")

plot_combined

ggsave(
  "allometry_model_comparison.png",
  plot_combined,
  width=10,
  height=12,
  dpi=600
)

# ---- Calibration of population model ----
validation_data <-
  df_allometry %>%
  filter(
    !is.na(size),
    !is.na(predicted_population)
  ) %>%
  mutate(
    abs_difference =
      abs(predicted_population - size)
  )

recal_model <-
  lm(
    size ~ predicted_population,
    data = validation_data
  )

summary(recal_model)

cal_intercept <- coef(recal_model)[1]
cal_slope     <- coef(recal_model)[2]

validation_data_calibrated <-
  validation_data %>%
  mutate(
    calibrated_size =
      cal_intercept +
      cal_slope * predicted_population,
    
    calibration_error =
      calibrated_size - size,
    
    abs_calibration_error =
      abs(calibration_error),
    
    percent_calibration_error =
      100 * calibration_error / size
  )


# ---- Plot calibrated prediction error ----
plot_calibrated_error <-
  ggplot(
    validation_data_calibrated,
    aes(genetic_subpopulation,
        calibration_error,
        fill = genetic_subpopulation)
  ) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_boxplot(width=0.65, alpha=0.75) +
  geom_jitter(width=0.15, alpha=0.35, size=2) +
  labs(
    x = "Genetic subpopulation",
    y = "Calibration error (cm)",
    title = "Prediction Error After Model Calibration"
  ) +
  theme_bw(base_size=18) +
  theme(
    legend.position="none",
    plot.title=element_text(face="bold")
  )

plot_calibrated_error

# ---- Measured data vs calibrated prediction plot ----
plot_fit <-
ggplot(
validation_data_calibrated,
aes(size, calibrated_size)
) +
geom_point(alpha=0.6) +
geom_abline(slope=1, intercept=0, linetype="dashed") +
labs(
x="Measured body length (cm)",
y="Calibrated predicted length (cm)",
title="Measured vs Calibrated Body Length"
) +
theme_bw(base_size=18)

plot_fit

# ---- Export calibrated dataset ----
write.csv(
  validation_data_calibrated,
  "allometry_model_calibrated_validation.csv",
  row.names = FALSE
)


# ---- Extract the resulting formulas ----

extract_formula <- function(model, pop){
  
  b  <- coef(model)["log(condylobasal_length)"]
  aF <- exp(coef(model)["(Intercept)"])
  
  sex_coef <- coef(model)[grep("sex", names(coef(model)))]
  aM <- aF * exp(sex_coef)
  
  cat("\n", pop, "\n")
  
  cat("Female:\n")
  cat("Body length =", round(aF,4),
      "* CBL^", round(b,4), "\n")
  
  cat("Male:\n")
  cat("Body length =", round(aM,4),
      "* CBL^", round(b,4), "\n")
}

extract_formula(model_west,
                "Western North Sea")

extract_formula(model_east,
                "Eastern North Sea")

#Western North Sea 
#Female:
#Body length = 0.6797 * CBL^ 1.6325 
#Male:
#Body length = 0.6582 * CBL^ 1.6325 

#Eastern North Sea 
#Female:
#Body length = 2.4846 * CBL^ 1.2528 
#Male:
#Body length = 2.44 * CBL^ 1.2528 

############################################################
# GRAPH 6: TEMPORAL TRENDS BY LIFE STAGE
############################################################
make_plot <- function(stage, subpop){
  
  df <- analysis_data %>%
    filter(
      life_stage == stage,
      genetic_subpopulation == subpop,
      !is.na(year),
      !is.na(size)
    )
  
  # ---- slope + p-value ----
  get_stats <- function(sex_val){
    
    # omit unreliable Eastern NS juvenile/subadult
    if(subpop=="Eastern North Sea" &
       stage %in% c("subadult","juvenile")) return(NULL)
    
    d <- df %>% filter(sex==sex_val)
    
    if(nrow(d)>=3 && length(unique(d$year))>=2){
      m <- lm(size ~ year, data=d)
      slope <- round(coef(m)["year"],3)
      p_val <- signif(summary(m)$coefficients["year","Pr(>|t|)"],3)
      
      paste0(ifelse(sex_val=="M","Males","Females"),
             ": slope = ",slope," (p = ",p_val,")")
    } else NULL
  }
  
  subtitle_vec <- c(get_stats("M"), get_stats("F"))
  subtitle_txt <- if(length(subtitle_vec)>0)
    paste(subtitle_vec, collapse=" | ")
  else NULL
  
  counts <- df %>% count(sex)
  
  p <- ggplot(df, aes(year, size, color = sex)) +
    geom_point(size = 3.5, alpha = 0.85)
  
  # regression only if enough data
  for(sx in counts$sex){
    
    n_val <- counts$n[counts$sex==sx]
    
    if(!is.na(n_val) && n_val >= 3){
      
      p <- p +
        geom_smooth(
          data = df %>% filter(sex==sx),
          method = "lm",
          se = TRUE,
          color = "black"
        )
    }
  }
  
  p +
    scale_x_continuous(breaks = year_breaks) +
    scale_y_continuous(limits = c(100,200),
                       breaks = seq(100,200,20)) +
    labs(
      title = tools::toTitleCase(stage),
      subtitle = subtitle_txt,
      x = "Year of Collection",
      y = "Total body length (cm)",
      color = "Sex"
    ) +
    theme_bw(base_size = 18) +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      strip.background = element_rect(fill="grey85"),
      plot.title = element_text(face="bold"),
      plot.subtitle = element_text(size=14),
      legend.title = element_text(size=16),
      legend.text = element_text(size=14),
      panel.spacing = unit(1,"cm")
    )
}

# ---- create the different panels ----
west_adult    <- make_plot("adult","Western North Sea")
west_subadult <- make_plot("subadult","Western North Sea")
west_juvenile <- make_plot("juvenile","Western North Sea")

east_adult    <- make_plot("adult","Eastern North Sea")
east_subadult <- make_plot("subadult","Eastern North Sea")
east_juvenile <- make_plot("juvenile","Eastern North Sea")

header_box <- function(label){
  
  ggplot() +
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill="grey85") +
    annotate("text", x=0,y=0,label=label, fontface="bold", size=6) +
    theme_void()
}

# ---- assemble columns ----
west_column <- (header_box("Western North Sea") /
                  west_adult /
                  west_subadult /
                  west_juvenile) +
  plot_layout(heights=c(0.08,1,1,1))

east_column <- (header_box("Eastern North Sea") /
                  east_adult /
                  east_subadult /
                  east_juvenile) +
  plot_layout(heights=c(0.08,1,1,1))

plot_trends <- west_column | east_column

plot_trends

ggsave(
  filename = "temporal_trends_lifestage_subpopulation.png",
  plot = plot_trends,
  device = "png",
  width = 16,      # inches — controls final physical size
  height = 12,
  units = "in",
  dpi = 600,       # publication quality resolution
  bg = "white"
)


############################################################
# GRAPH 7: BODY SIZE TRENDS OVER TIME
############################################################
analysis_data <- data_clean %>%
  mutate(
    year = as.integer(str_extract(as.character(year), "\\d{4}")),
    sex = factor(sex, levels = c("F","M")),
    genetic_subpopulation = factor(
      genetic_subpopulation,
      levels = c("Western North Sea","Eastern North Sea")
    ),
    
    predicted_size = case_when(
      genetic_subpopulation=="Western North Sea" & sex=="F" & !is.na(condylobasal_length) ~ 0.6797 * condylobasal_length^1.6325,
      genetic_subpopulation=="Western North Sea" & sex=="M" & !is.na(condylobasal_length) ~ 0.6582 * condylobasal_length^1.6325,
      genetic_subpopulation=="Eastern North Sea" & sex=="F" & !is.na(condylobasal_length) ~ 2.4846 * condylobasal_length^1.2528,
      genetic_subpopulation=="Eastern North Sea" & sex=="M" & !is.na(condylobasal_length) ~ 2.44 * condylobasal_length^1.2528,
      TRUE ~ NA_real_
    ),
    
    size_final = coalesce(size, predicted_size),
    
    data_type = case_when(
      !is.na(size) ~ "Measured",
      is.na(size) & !is.na(predicted_size) ~ "Predicted",
      TRUE ~ NA_character_
    ),
    data_type = factor(data_type, levels = c("Measured","Predicted"))
  ) %>%
  filter(!is.na(size_final), !is.na(year)) %>%
  mutate(
    life_stage_estimated = factor(
      case_when(
        genetic_subpopulation == "Western North Sea" & sex == "F" & size_final <= 130 ~ "juvenile",
        genetic_subpopulation == "Western North Sea" & sex == "F" & size_final <= 150 ~ "subadult",
        genetic_subpopulation == "Western North Sea" & sex == "F" & size_final >= 151 ~ "adult",
        
        genetic_subpopulation=="Western North Sea" & sex=="M" & size_final <= 120 ~ "juvenile",
        genetic_subpopulation=="Western North Sea" & sex=="M" & size_final <= 140 ~ "subadult",
        genetic_subpopulation=="Western North Sea" & sex=="M" & size_final >= 141 ~ "adult",
        
        genetic_subpopulation=="Eastern North Sea" & sex=="F" & size_final <= 110 ~ "juvenile",
        genetic_subpopulation=="Eastern North Sea" & sex=="F" & size_final <= 140 ~ "subadult",
        genetic_subpopulation=="Eastern North Sea" & sex=="F" & size_final >= 141 ~ "adult",
        
        genetic_subpopulation=="Eastern North Sea" & sex=="M" & size_final <= 115 ~ "juvenile",
        genetic_subpopulation=="Eastern North Sea" & sex=="M" & size_final <= 135 ~ "subadult",
        genetic_subpopulation=="Eastern North Sea" & sex=="M" & size_final >= 136 ~ "adult",
        
        TRUE ~ NA_character_
      ),
      levels = c("adult","subadult","juvenile")
    )
  )

# unified Y-axis limits
global_y_min <- min(analysis_data$size_final, na.rm = TRUE) - 5
global_y_max <- max(analysis_data$size_final, na.rm = TRUE) + 5

make_plot <- function(stage, subpop, n_ticks = 5) {
  
  df <- analysis_data %>%
    filter(life_stage_estimated == stage,
           genetic_subpopulation == subpop)
  
  if(nrow(df) == 0) {
    return(
      ggplot() +
        theme_void() +
        labs(title = paste(stage, "-", subpop, "(no data)"))
    )
  }
  
  # individual X-axis per subplot
  x_min <- min(df$year, na.rm = TRUE)
  x_max <- max(df$year, na.rm = TRUE)
  x_breaks <- round(seq(x_min, x_max, length.out = n_ticks))
  
  # calculate n, slope, p-value per sex
  stats_text <- df %>%
    group_by(sex) %>%
    summarise(
      n = n(),
      fit = list(lm(size_final ~ year)),
      .groups = "drop"
    ) %>%
    mutate(
      slope = map_dbl(fit, ~coef(.x)[2]),
      pval = map_dbl(fit, ~summary(.x)$coefficients[2,4]),
      label = paste0(sex, ": n=", n, ", slope=", round(slope,2), " cm/yr, p=", signif(pval,3))
    ) %>%
    pull(label) %>%
    paste(collapse = "\n")
  
  ggplot(df, aes(x = year, y = size_final)) +
    geom_point(aes(color = sex, shape = data_type),
               size = 2.6, alpha = 0.9,
               position = position_jitter(width = 0.4)) +
    geom_smooth(aes(color = sex),
                method = "lm", se = FALSE,
                linewidth = 1.1,
                show.legend = FALSE) +
    scale_color_manual(values = c(F = "#F8766D", M = "#00BFC4")) +
    scale_shape_manual(values = c(Measured = 16, Predicted = 1)) +
    scale_x_continuous(limits = c(x_min, x_max), breaks = x_breaks) +
    scale_y_continuous(limits = c(global_y_min, global_y_max)) +
    labs(title = tools::toTitleCase(stage),
         x = "Year of Collection",
         y = "Total body length (cm)") +
    annotate("text", 
             x = x_min + 0.05*(x_max-x_min), 
             y = global_y_max - 0.05*(global_y_max-global_y_min),
             label = stats_text,
             hjust = 0,
             vjust = 1,
             size = 4
    ) +
    theme_bw(base_size = 18) +
    theme(
      plot.title = element_text(face="bold", size=16),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      axis.text.x = element_text(angle=45, hjust=1),
      legend.position = "none"
    )
}

west_adult    <- make_plot("adult","Western North Sea")
west_subadult <- make_plot("subadult","Western North Sea")
west_juvenile <- make_plot("juvenile","Western North Sea")

east_adult    <- make_plot("adult","Eastern North Sea")
east_subadult <- make_plot("subadult","Eastern North Sea")
east_juvenile <- make_plot("juvenile","Eastern North Sea")

header_box <- function(label){
  ggplot() +
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill="grey85") +
    annotate("text", x=0,y=0,label=label, fontface="bold", size=5) +
    theme_void()
}

west_column <- (header_box("Western North Sea") /
                  west_adult /
                  west_subadult /
                  west_juvenile) +
  plot_layout(heights = c(0.06, 1, 1, 1))

east_column <- (header_box("Eastern North Sea") /
                  east_adult /
                  east_subadult /
                  east_juvenile) +
  plot_layout(heights = c(0.06, 1, 1, 1))

plots_grid <- patchwork::wrap_plots(
  west_column,
  east_column,
  ncol = 2
)

legend_plot <- ggplot(
  analysis_data %>% 
    filter(life_stage_estimated=="adult",
           genetic_subpopulation=="Western North Sea"),
  aes(x = year, y = size_final)
) +
  geom_point(aes(color = sex, shape = data_type), size = 3) +
  scale_color_manual(values = c(F = "#F8766D", M = "#00BFC4"), name = "Sex") +
  scale_shape_manual(values = c(Measured = 16, Predicted = 1), name = "Data type") +
  theme_bw(base_size = 18) +
  theme(legend.position = "right")

legend <- cowplot::get_legend(legend_plot)

final_plot <-
  (plots_grid | patchwork::wrap_elements(legend)) +
  plot_layout(widths = c(5, 1))

print(final_plot)

ggsave(
  filename = "body_size_trends_final.png",
  plot = final_plot,
  width = 16,
  height = 18,
  units = "in",
  dpi = 600,
  bg = "white"
)

############################################################
# Spearman rho & Kendalls tau
############################################################
trend_data <- analysis_data %>%
  filter(
    !is.na(size_final),
    !is.na(year),
    !is.na(life_stage_estimated),   
    genetic_subpopulation %in% c("Western North Sea","Eastern North Sea")
  ) %>%
  mutate(year = as.integer(year))

############################################################
# FUNCTION TO CALCULATE TREND STATISTICS
############################################################

trend_test <- function(df){
  
  yearly <- df %>%
    group_by(year) %>%
    summarise(mean_size = mean(size_final, na.rm = TRUE), .groups = "drop")
  
  # if too few years → return NA row (safe)
  if(nrow(yearly) < 3){
    return(tibble(
      n_years = nrow(yearly),
      slope_cm_per_year = NA_real_,
      spearman_rho = NA_real_,
      spearman_p = NA_real_,
      kendall_tau = NA_real_,
      kendall_p = NA_real_
    ))
  }
  
  # Linear regression slope
  lm_model <- lm(mean_size ~ year, data = yearly)
  slope <- coef(lm_model)[2]
  
  # Spearman
  spearman_test <- cor.test(
    yearly$year,
    yearly$mean_size,
    method = "spearman"
  )
  
  # Kendall
  kendall_test <- cor.test(
    yearly$year,
    yearly$mean_size,
    method = "kendall"
  )
  
  tibble(
    n_years = nrow(yearly),
    slope_cm_per_year = slope,
    spearman_rho = as.numeric(spearman_test$estimate),
    spearman_p = spearman_test$p.value,
    kendall_tau = as.numeric(kendall_test$estimate),
    kendall_p = kendall_test$p.value
  )
}

############################################################
# LEVEL 1 — SUBPOPULATION ONLY
############################################################

results_level1 <- trend_data %>%
  group_by(genetic_subpopulation) %>%
  group_modify(~trend_test(.x)) %>%
  mutate(level = "Subpopulation")

############################################################
# LEVEL 2 — SUBPOPULATION × SEX
############################################################

results_level2 <- trend_data %>%
  filter(!is.na(sex)) %>%
  group_by(genetic_subpopulation, sex) %>%
  group_modify(~trend_test(.x)) %>%
  mutate(level = "Subpopulation × Sex")

############################################################
# LEVEL 3 — SUBPOPULATION × SEX × LIFE STAGE
############################################################

results_level3 <- trend_data %>%
  filter(!is.na(sex), !is.na(life_stage_estimated)) %>%  # ✅ FIX
  group_by(genetic_subpopulation, sex, life_stage_estimated) %>%
  group_modify(~trend_test(.x)) %>%
  mutate(level = "Subpopulation × Sex × Life stage")

############################################################
# COMBINE RESULTS
############################################################

trend_results <- bind_rows(
  results_level1,
  results_level2,
  results_level3
) %>%
  mutate(
    across(
      c(slope_cm_per_year, spearman_rho, kendall_tau),
      ~round(.x, 3)
    ),
    across(
      c(spearman_p, kendall_p),
      ~signif(.x, 3)
    )
  ) %>%
  arrange(level, genetic_subpopulation, sex, life_stage_estimated)

############################################################
# CREATE TABLE (gt)
############################################################

trend_table <- trend_results %>%
  gt::gt() %>%
  gt::cols_label(
    genetic_subpopulation = "Subpopulation",
    sex = "Sex",
    life_stage_estimated = "Life stage",
    n_years = "Years",
    slope_cm_per_year = "Slope (cm/year)",
    spearman_rho = "Spearman ρ",
    spearman_p = "p (ρ)",
    kendall_tau = "Kendall τ",
    kendall_p = "p (τ)",
    level = "Analysis level"
  ) %>%
  gt::tab_header(
    title = "Temporal Trends in Harbour Porpoise Body Size"
  )

trend_table

############################################################
# SAVE OUTPUT
############################################################

write.csv(
  trend_results,
  "body_size_trend_statistics.csv",
  row.names = FALSE
)

# Save table as image
table_plot <- gridExtra::tableGrob(trend_results)

ggplot2::ggsave(
  "body_size_trend_table.png",
  table_plot,
  width = 14,
  height = 8,
  dpi = 600
)


############################################################
# ACF Plots
############################################################

# ---- helper function to generate ACF plot for a subset ----
make_acf_plot <- function(stage, subpop, sex_val = NULL){
  
  df <- analysis_data %>%
    filter(life_stage_estimated == stage,
           life_stage_estimated != "neonate",
           genetic_subpopulation == subpop)
  
  # filter by sex if provided
  if(!is.null(sex_val)) df <- df %>% filter(sex == sex_val)
  
  # skip if too few points
  if(nrow(df) < 3) return(ggplot() + theme_void() + labs(title = "Not enough data"))
  
  # order by year
  df <- df %>% arrange(year)
  
  acf_obj <- acf(df$size_final, plot = FALSE, na.action = na.pass)
  acf_df <- data.frame(lag = acf_obj$lag, acf = acf_obj$acf)
  
  # Standard horizontal significance lines for ACF
  n <- nrow(df)
  ci <- 1.96 / sqrt(n)  # approximate 95% confidence interval
  
  ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_hline(yintercept = ci, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = -ci, linetype = "dashed", color = "gray50") +
    geom_segment(aes(xend = lag, yend = 0), linewidth = 1, color = "#0072B2") +
    scale_x_continuous(breaks = seq(0, max(acf_df$lag), by = 1)) +
    scale_y_continuous(limits = c(-1,1)) +
    labs(x = "Lag (years)", y = "ACF", title = tools::toTitleCase(stage)) +
    theme_bw(base_size = 14) +
    theme(
      axis.title = element_text(size=12),
      axis.text = element_text(size=10),
      plot.title = element_text(size=12, face="bold", hjust = 0.5)
    )
}

# ---- Create all panels ----
# Western North Sea
west_juvenile <- make_acf_plot("juvenile","Western North Sea")
west_subadult <- make_acf_plot("subadult","Western North Sea")
west_adult    <- make_acf_plot("adult","Western North Sea")

# Eastern North Sea
east_juvenile <- make_acf_plot("juvenile","Eastern North Sea")
east_subadult <- make_acf_plot("subadult","Eastern North Sea")
east_adult    <- make_acf_plot("adult","Eastern North Sea")

header_box <- function(label){
  ggplot() +
    annotate("rect", xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf, fill="grey85") +
    annotate("text", x=0,y=0,label=label, fontface="bold", size=6) +
    theme_void()
}

# ---- assemble columns ----
west_column <- (header_box("Western North Sea") /
                  west_juvenile /
                  west_subadult /
                  west_adult) +
  plot_layout(heights = c(0.08,1,1,1))

east_column <- (header_box("Eastern North Sea") /
                  east_juvenile /
                  east_subadult /
                  east_adult) +
  plot_layout(heights = c(0.08,1,1,1))

acf_trends_plot <- west_column | east_column

acf_trends_plot

ggsave(
  filename = "body_size_acf_by_lifestage_subpopulation.png",
  plot = acf_trends_plot,
  width = 16,
  height = 12,
  units = "in",
  dpi = 600
)
