#############################################
# 03_ordinal_logistic_regression.R
# Purpose:
#   - Replicate Appendix Table 1A — Ordinal logistic regression
# Input:
#   data/cleaned_dataset.csv
# Output:
#   artifacts/tables/appendix_table_1A_replication.html
#   artifacts/tables/appendix_table_1A_replication.csv
#   artifacts/models/q40_olr_model.rds
#   artifacts/models/q41_olr_model.rds
#   artifacts/models/q42_olr_model.rds
#   artifacts/models/q43_olr_model.rds
#############################################

# ---- 1) Load cleaned data ----
model_df <- readRDS("data/cleaned_dataset.rds")

# Safety: check the columns we need exist and look like factors
stopifnot(all(c("q40_o","q41_o","q42_o","q43_o","age5","gender","education","income") %in% names(model_df)))
stopifnot(is.ordered(model_df$q40_o), is.factor(model_df$age5))
stopifnot(is.factor(model_df$gender), is.factor(model_df$education), is.factor(model_df$income))

# ---- 2) building reproducible function for model fitting ----
fit_polr_model <- function(df, yvar_chr) {
  # Preprocess
  df_use <- preprocess_df(df, yvar_chr)
  # Fit
  model <- fit_polr(yvar_chr = yvar_chr, df_use = df_use)
  # Tidy
  tt <- tidy_polr_results(model)

  # Build maps (we store maps_global for assemble stage)
  age_map <- setNames(lvl_age[-1], grep("^age5", tt$term, value = TRUE))
  gender_map <- setNames(lvl_gender[-1], grep("^gender", tt$term, value = TRUE))
  edu_map <- setNames(lvl_education[-1], grep("^education", tt$term, value = TRUE))
  inc_map <- setNames(lvl_income[-1], grep("^income", tt$term, value = TRUE))


  Factor <- dplyr::case_when(
    tt$term %in% names(age_map)    ~ "Age",
    tt$term %in% names(gender_map) ~ "Gender",
    tt$term %in% names(edu_map)    ~ "Education",
    tt$term %in% names(inc_map)    ~ "Income",
    TRUE ~ NA_character_
  )

  Category <- coalesce(
    map_cat(tt$term, age_map, "Age"),
    map_cat(tt$term, gender_map, "Gender"),
    map_cat(tt$term, edu_map, "Education"),
    map_cat(tt$term, inc_map, "Income")
  )

  split <- data.frame(Factor = Factor,
                      Category = Category,
                      stringsAsFactors = FALSE)

   # ---- Outcome label ----
  out_label <- dplyr::case_when(
    yvar_chr == "q40_o" ~ "Knowledge of AI (Q40)",
    yvar_chr == "q41_o" ~ "Comfort with AI (Q41)",
    yvar_chr == "q42_o" ~ "Comfort with AI using consented data (Q42)",
    yvar_chr == "q43_o" ~ "Comfort with AI using de-identified data (Q43)",
    TRUE ~ yvar_chr
  )

  # ---- Reference rows -----
  ref_rows <- tibble::tibble(
    Factor   = c("Age","Gender","Income","Education"),
    Category = c("16–24 years","male","< $24,999","Highschool"),
    OR = 1,
    LCL = NA_real_,
    UCL = NA_real_,
    p.value = NA_real_
  )

  # ---- Bind results + add reference rows ----
  res <- dplyr::bind_cols(
    split,
    tt %>% dplyr::select(OR, LCL, UCL, p.value)
  ) %>%
    dplyr::bind_rows(ref_rows) %>%
    dplyr::mutate(
      Factor_f = factor(Factor,
                        levels = c("Age", "Gender", "Income", "Education")),
      Cat_f = dplyr::case_when(
        Factor == "Age"       ~ factor(Category, levels = lvl_age),
        Factor == "Gender"    ~ factor(Category, levels = lvl_gender),
        Factor == "Income"    ~ factor(Category, levels = lvl_income),
        Factor == "Education" ~ factor(Category, levels = lvl_education),
        TRUE                  ~ factor(Category)
      )
    ) %>%
    dplyr::arrange(Factor_f, Cat_f) %>%
    dplyr::select(Factor, Category, OR, LCL, UCL, p.value)

  # ---- Build final output table ----
  out_table <- res %>%
    dplyr::mutate(
      `Odds Ratio (95% CI)` = ifelse(
        is.na(LCL),
        "1.00 (ref)",
        paste0(fmt2(OR), " (", fmt2(LCL), "–", fmt2(UCL), ")")
      ),
      `p-value` = ifelse(is.na(p.value), "", fmt2(p.value)),
      sig       = stars(p.value),
      Outcome   = out_label
    ) %>%
    dplyr::select(
      Outcome,
      Factor,
      Categories = Category,
      `Odds Ratio (95% CI)`,
      `p-value`,
      sig
    )

  # Return
  list(
    model = model,
    table = out_table
  )
}

# ---- 3) Fit all four (unchanged calls) ----
q40 <- fit_polr_model(model_df, "q40_o")
q41 <- fit_polr_model(model_df, "q41_o")
q42 <- fit_polr_model(model_df, "q42_o")
q43 <- fit_polr_model(model_df, "q43_o")
saveRDS(q40$model, "artifacts/models/q40_olr_model.rds")
saveRDS(q41$model, "artifacts/models/q41_olr_model.rds")
saveRDS(q42$model, "artifacts/models/q42_olr_model.rds")
saveRDS(q43$model, "artifacts/models/q43_olr_model.rds")
tbl_q40 <- q40$table
tbl_q41 <- q41$table
tbl_q42 <- q42$table
tbl_q43 <- q43$table
appendix_1a <- dplyr::bind_rows(tbl_q40, tbl_q41, tbl_q42, tbl_q43)

# ---- 4) Combine the four precomputed result tables into one report ----
tbl_q40$`Dependent Variable` <- "Q.40 – How knowledgeable are you about what artificial intelligence is?"
tbl_q41$`Dependent Variable` <- "Q.41 – How comfortable are you with AI being used as a tool in healthcare?"
tbl_q42$`Dependent Variable` <- "Q.42 – How comfortable are you with AI using consented data?"
tbl_q43$`Dependent Variable` <- "Q.43 – How comfortable are you with AI using de-identified data?"
appendix_1a_final <- dplyr::bind_rows(tbl_q40, tbl_q41, tbl_q42, tbl_q43) %>%
  dplyr::select(
    `Dependent Variable`,
    Factor,
    Categories,
    `Odds Ratio (95% CI)`,
    `p-value`
  )

# ---- 5) Create GT table for Appendix Table 1A ----
appendix_1a_gt <- appendix_1a_final %>%
  gt::gt(groupname_col = "Dependent Variable") %>%
  gt::tab_header(
    title = md("**Appendix Table 1A (Replication): Ordinal logistic regression — Odds ratios (95% CI) and p-values**")
  ) %>%
  gt::cols_label(
    Factor                = md("**Factor**"),
    Categories            = md("**Categories**"),
    `Odds Ratio (95% CI)` = md("**Odds Ratio (95% CI)**"),
    `p-value`             = md("**p-value**")
  ) %>%
  gt::fmt_number(columns = "p-value", decimals = 3) %>%
  gt::tab_source_note(
    source_note = md("Notes: Reference categories — Age 16–24; Gender male; Education Highschool; Income < $24,999. Age in this replication uses the five bins present in this CDHS export (16–24, 25–34, 35–44, 45–54, 55+).")
  )
# ---- 6) Save the final HTML and CSV report ----
gt::gtsave(appendix_1a_gt, "artifacts/tables/appendix_table_1A_replication.html")
readr::write_csv(appendix_1a_final, "artifacts/tables/appendix_table_1A_replication.csv")
