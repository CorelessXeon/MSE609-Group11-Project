#############################################
# 05_plots_Q40_to_Q43.R
# Purpose:
#   replicate the 4 3d plots from the paper
# Input:
#   artifacts/cleaned_dataset_used_for_modelling.csv
# Output:
#   output/plot/fig1_Q40.png
#   output/plot/fig2_Q41.png
#   output/plot/fig3_Q42.png
#   output/plot/fig4_Q43.png
#############################################

library(tidyverse)
library(here)
library(broom)
library(plotly)
library(dplyr)
library(tidyr)
library(MASS)
# 1. Load cleaned dataset ---------------------------------------------
data_path <- here("model_df.csv")
df <- read_csv(data_path)
# we need to predict y-hat values for Q40 to Q43 and save them in the dataframe
# but first the authors used Multivariate polynomial regression so we need to fit that model first
lvl_income <- c("< $24,999","$25,000-$49,999","$50,000-$79,999","$80,000-$99,000","$100,000-$149,999","$150,000-$249,999","$250,000+")
lvl_education <- c("Highschool","Apprenticeship/Trades","College/CEGEP","University degree","Masters","Medical/paramedical","PhD")
lvl_age <- c("16–24 years","25–34 years","35–44 years","45–54 years","55+ years")
lvl_gender <- c("male", "female", "other")

# Helper function
convert_to_num <- function(x, levels) {
  x <- factor(x, levels = levels)
  as.numeric(x)
}

# Apply
df$income_num    <- convert_to_num(df$income, lvl_income)
df$educ_num      <- convert_to_num(df$education, lvl_education)
df$age_num       <- convert_to_num(df$age5, lvl_age)
df$gender_num    <- convert_to_num(df$gender, lvl_gender)

# DOE Pro (SigmaZone) uses Response Surface Methodology (RSM)
predictors <- c("age_num", "gender_num", "educ_num", "income_num")
# y=β0​+i∑​βi​xi​(main effects)+i<j∑​βij​xi​xj​(interactions)+i∑​βii​xi2​(quadratics)
f_poly <- as.formula(
  paste(
    "y ~ (", paste(predictors, collapse = " + "), ")^2 +",
    paste("I(", predictors, "^2)", collapse = " + ")
  )
)

# fit lm gives the formula
# \(y=\beta _{0}\)\(+\beta _{1}\cdot \text{age}_{\text{num}}+\beta _{2}\cdot \text{gender}_{\text{num}}+\beta _{3}\cdot \text{educ}_{\text{num}}+\beta _{4}\cdot \text{income}_{\text{num}}\)\(+\beta _{5}\cdot \text{age}_{\text{num}}^{2}+\beta _{6}\cdot \text{gender}_{\text{num}}^{2}+\beta _{7}\cdot \text{educ}_{\text{num}}^{2}+\beta _{8}\cdot \text{income}_{\text{num}}^{2}\)\(+\beta _{9}\cdot (\text{age}_{\text{num}}\cdot \text{gender}_{\text{num}})+\beta _{10}\cdot (\text{age}_{\text{num}}\cdot \text{educ}_{\text{num}})+\beta _{11}\cdot (\text{age}_{\text{num}}\cdot \text{income}_{\text{num}})\)\(+\beta _{12}\cdot (\text{gender}_{\text{num}}\cdot \text{educ}_{\text{num}})+\beta _{13}\cdot (\text{gender}_{\text{num}}\cdot \text{income}_{\text{num}})\)\(+\beta _{14}\cdot (\text{educ}_{\text{num}}\cdot \text{income}_{\text{num}})\)\(+\epsilon \)


# fitting model for all outcomes
outcomes <- c("q40_o", "q41_o", "q42_o", "q43_o")

models <- lapply(outcomes, function(resp) {
  f <- update(f_poly, paste(resp, "~ ."))
  lm(f, data = df)
})

names(models) <- outcomes

#plotting helper function
plot_question_surface_poly <- function(model, df, question_name,
                                       save_dir = "artifacts/plots",
                                       file_type = "html") {

  # --------------------------------------------
  # 1. Build grid for plotting
  # --------------------------------------------
  # age_vals    <- sort(unique(df$age_num))
  # gender_vals <- sort(unique(df$gender_num))
  # to meke the plot smooth, we use irrealistic values in between integers
  age_vals    <- seq(1, 5, length.out = 100)
  gender_vals <- seq(1, 3, length.out = 100)
  grid <- expand.grid(
    age_num    = age_vals,
    gender_num = gender_vals,
    educ_num   = 4,
    income_num = 4 
  )

  # --------------------------------------------
  # 2. Predict fitted values from polynomial regression
  # --------------------------------------------
  grid$predicted <- predict(model, newdata = grid)

  # --------------------------------------------
  # 3. Reshape into matrix for surface plot
  # --------------------------------------------
  zmat <- matrix(grid$predicted,
                 nrow = length(age_vals),
                 ncol = length(gender_vals),
                 byrow = FALSE)

  # --------------------------------------------
  # 4. 3D Plotly Surface
  # --------------------------------------------
  p <- plot_ly(
    x = gender_vals,
    y = age_vals,
    z = zmat,
    type = "surface"
  ) %>%
    layout(
      title = paste(question_name, "- Polynomial Regression Surface"),
      scene = list(
        # flip x-axis by setting range from max to min
        xaxis = list(
          title = "Gender (coded)",
          range = c(3, 1),                # <--- explicit reversed range (max -> min)
          tickmode = "array",
          tickvals = 1:3,
          ticktext = levels(df$gender)    # will appear left->right as male, female, other
        ),
        yaxis = list(
          title = "Age (coded)",
          tickmode = "array",
          tickvals = 1:5,
          ticktext = levels(df$age5)
        ),
        zaxis = list(title = "Predicted Score")
      )
    )

  # --------------------------------------------
  # 5. Save output
  # --------------------------------------------
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  file_path <- file.path(save_dir, paste0(question_name, ".", file_type))

  if (file_type == "html") {
    htmlwidgets::saveWidget(p, file_path, selfcontained = FALSE)
  } else if (file_type == "png") {
    plotly::orca(p, file_path)
  }

  return(p)
}


plot_q40 <- plot_question_surface_poly(models$q40_o, df, "Q40")
plot_q41 <- plot_question_surface_poly(models$q41_o, df, "Q41")
plot_q42 <- plot_question_surface_poly(models$q42_o, df, "Q42")
plot_q43 <- plot_question_surface_poly(models$q43_o, df, "Q43")