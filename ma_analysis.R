#Field of education and democratic Erosion 
#Pilot data analysis
#Packages
packages <- c(
  "tidyverse",
  "haven",
  "janitor",
  "psych",
  "broom",
  "cregg",
  "FindIt",
  "fastDummies",
  "tidyr",
  "stringr",
  "marginaleffects",
  "stargazer",
  "tidyr",
  "knitr", 
  "kableExtra",
  "gtsummary",
  "tableone"
)
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
invisible(lapply(packages, library, character.only = TRUE))
###
###
###
#H1: conditional AMCEs
conjoint_long <- ##LOAD DATA
  as.data.frame(read.csv("ma_conjoint_full.csv"))

basic1_cmm <- ##CONDTIONAL AMCE WITH ORIGINAL PROFILE PARTY ID
  cj(
  data = conjoint_long,
  formula = choice ~ Age + Gender + PreviousOccupation + 
    PoliticalParty + PoliticalStatement1 + PoliticalStatement2,
  id =~ResponseId,
  estimate = "amce",
  by =~ major_hss
)

basic2_cmm <- ##CONDTIONAL AMCE WITH IN-PARTY PROFILE CODING
  cj(
  data = conjoint_long,
  formula = choice ~ Age + Gender + PreviousOccupation + 
    Inparty + PoliticalStatement1 + PoliticalStatement2,
  id =~ResponseId,
  estimate = "amce",
  by = ~ major_hss
)

##DIFFERENCES IN AMCEs FOR HYPOTHESIS TESTING
diffs1 <- amce_diffs(
  choice ~ Age + Gender + PreviousOccupation +
    PoliticalParty + PoliticalStatement1 + PoliticalStatement2,
  data = conjoint_long,
  id = ~ResponseId,
  by = ~major_hss
)

diffs2 <- amce_diffs(
  choice ~ Age + Gender + PreviousOccupation +
                       Inparty + PoliticalStatement1 + PoliticalStatement2,
  data = conjoint_long,
  id = ~ResponseId,
  by = ~major_hss
  )
##TABLES
as.data.frame(diffs1) |>
  dplyr::mutate(
    Attribute = dplyr::case_when(
      feature == "Age" ~ "Age",
      feature == "Gender" ~ "Gender",
      feature == "PreviousOccupation" ~ "Previous occupation",
      feature == "PoliticalParty" ~ "Political Party",
      feature == "PoliticalStatement1" ~ "Political Statement 1",
      feature == "PoliticalStatement2" ~ "Political Statement 2",
      TRUE ~ feature
    ),
    
    Level = dplyr::case_when(
      level == "PS1_Mild" ~ "Mild Transgression",
      level == "PS1_Strong" ~ "Strong Transgression",
      level == "PS2_Mild" ~ "Mild Transgression",
      level == "PS2_Strong" ~ "Strong Transgression",
      TRUE ~ level
    ),
    
    Difference = estimate,
    `Std. Error` = std.error,
    p_value = p
  ) |>
  dplyr::mutate(
    stars = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    `Difference (SE)` = paste0(
      round(Difference, 3), stars,
      " (", round(`Std. Error`, 3), ")"
    )
  ) |>
  dplyr::select(Attribute, Level, `Difference (SE)`, p_value) |>
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    digits = 3,
    caption = "Differences in Conditional AMCEs (HSS v. non-HSS graduates)"
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "striped"))

as.data.frame(diffs2) |>
  dplyr::mutate(
    Attribute = dplyr::case_when(
      feature == "Age" ~ "Age",
      feature == "Gender" ~ "Gender",
      feature == "PreviousOccupation" ~ "Previous occupation",
      feature == "Inparty" ~ "In-party",
      feature == "PoliticalStatement1" ~ "Political Statement 1",
      feature == "PoliticalStatement2" ~ "Political Statement 2",
      TRUE ~ feature
    ),
    
    Level = dplyr::case_when(
      level == "PS1_Mild" ~ "Mild Transgression",
      level == "PS1_Strong" ~ "Strong Transgression",
      level == "PS2_Mild" ~ "Mild Transgression",
      level == "PS2_Strong" ~ "Strong Transgression",
      TRUE ~ level
    ),
    
    Difference = estimate,
    `Std. Error` = std.error,
    p_value = p
  ) |>
  dplyr::mutate(
    stars = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.1 ~ "*",
      TRUE ~ ""
    ),
    `Difference (SE)` = paste0(
      round(Difference, 3), stars,
      " (", round(`Std. Error`, 3), ")"
    )
  ) |>
  dplyr::select(Attribute, Level, `Difference (SE)`, p_value) |>
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    digits = 3,
    caption = "Differences in Conditional AMCEs (HSS v. non-HSS graduates)"
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "striped"))

##CONDITIONAL AMCE PLOTS
df1 <- as.data.frame(basic1_cmm)
df1 <- df1 %>%
  mutate(
    level = case_when(
      feature == "PoliticalStatement1" & level == "PS1_Mild" ~ "Mild Transgression",
      feature == "PoliticalStatement1" & level == "PS1_Strong" ~ "Strong Transgression",
      
      feature == "PoliticalStatement2" & level == "PS2_Mild" ~ "Mild Transgression",
      feature == "PoliticalStatement2" & level == "PS2_Strong" ~ "Strong Transgression",
      
      TRUE ~ level
    )
  ) %>%
  group_by(feature) %>%
  mutate(level = reorder(level, estimate))

ggplot(df1, aes(x = estimate,
                y = reorder(level, estimate),
                color = factor(major_hss))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.5),
    height = 0.2,
    orientation = "y"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    x = "Conditional AMCEs",
    y = NULL,
    color = "Education Field",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

df2 <- as.data.frame(basic2_cmm)
df2 <- df2 %>%
  mutate(
    level = case_when(
      feature == "PoliticalStatement1" & level == "PS1_Mild" ~ "Mild Transgression",
      feature == "PoliticalStatement1" & level == "PS1_Strong" ~ "Strong Transgression",
      
      feature == "PoliticalStatement2" & level == "PS2_Mild" ~ "Mild Transgression",
      feature == "PoliticalStatement2" & level == "PS2_Strong" ~ "Strong Transgression",
      
      TRUE ~ level
    )
  ) %>%
  group_by(feature) %>%
  mutate(level = reorder(level, estimate))

ggplot(df2, aes(x = estimate,
                y = reorder(level, estimate),
                color = factor(major_hss))) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.5),
    height = 0.2,
    orientation = "y"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-max_abs, max_abs)) +
  facet_wrap(~ feature, scales = "free_y") +
  labs(
    x = "Conditional AMCEs",
    y = NULL,
    color = "Education Field"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )
#####
#####
#H2: AMIE: IN-PARTY * TRANSGRESSION STRENGTH
df_clean <- ##CLEAN OUT NAs FOR FINDIT PACKAGE
  conjoint_long %>%
  dplyr::select(
    ResponseId, task, choice,
    Age, Gender, PreviousOccupation,
    Inparty, PoliticalStatement1, PoliticalStatement2,
    major_hss
  ) %>%
  tidyr::drop_na()

##SPLIT DATA INTO EDUCATION SUB-GROUPS
df_hss <- df_clean %>%
  dplyr::filter(major_hss == "HSS") %>%
  dplyr::mutate(
    pair_id = paste(ResponseId, task, sep = "_")
  )
df_non_hss <- df_clean %>%
  dplyr::filter(major_hss == "Non-HSS") %>%
  dplyr::mutate(
    pair_id = paste(ResponseId, task, sep = "_")
  )
factor_vars <- c(
  "Age", "Gender", "PreviousOccupation",
  "Inparty", "PoliticalStatement1", "PoliticalStatement2"
)
##RE-FACTORIZE
df_hss[factor_vars] <- lapply(df_hss[factor_vars], factor)
df_non_hss[factor_vars] <- lapply(df_non_hss[factor_vars], factor)

##FIT MODELS 
fit_hss <- ##HSS GROUP
  CausalANOVA(
  formula = choice ~ Age + Gender + PreviousOccupation +
    Inparty + PoliticalStatement1 + PoliticalStatement2,
  data = df_hss,
  pair.id = df_hss$pair_id,
  diff = TRUE,
  int2.formula = ~ PoliticalStatement1:Inparty + PoliticalStatement2:Inparty,
  ord.fac = rep(FALSE, 6),
  nway = 2
)

fit_non_hss <- ##NON-HSS GROUP
  CausalANOVA(
  formula = choice ~ Age + Gender + PreviousOccupation +
    Inparty + PoliticalStatement1 + PoliticalStatement2,
  data = df_non_hss,
  pair.id = df_non_hss$pair_id,
  diff = TRUE,
  int2.formula = ~ PoliticalStatement1:Inparty + PoliticalStatement2:Inparty,
  ord.fac = rep(FALSE, 6),
  nway = 2
)

##EXTRACT AMIE ESTIMATES 
ce_bonus_hss1 <- ConditionalEffect(
  fit_hss, 
  treat.fac = "Inparty", 
  cond.fac = "PoliticalStatement1"
)

ce_bonus_non_hss1 <- ConditionalEffect(
  fit_non_hss, 
  treat.fac = "Inparty", 
  cond.fac = "PoliticalStatement1"
)

ce_bonus_hss2 <- ConditionalEffect(
  fit_hss, 
  treat.fac = "Inparty", 
  cond.fac = "PoliticalStatement2"
)

ce_bonus_non_hss2 <- ConditionalEffect(
  fit_non_hss, 
  treat.fac = "Inparty", 
  cond.fac = "PoliticalStatement2"
)

##EXTRACT DATA FOR PLOTTING 
ce_to_df <- function(ce_obj, group_name, statement_name) {
  mat_list <- ce_obj[[1]]
  out <- lapply(names(mat_list), function(cond_name) {
    mat <- mat_list[[cond_name]]
    transgression <- ifelse(grepl("Mild", cond_name), "Mild", "Strong")
    data.frame(
      Group = group_name,
      Statement = statement_name,
      Transgression = transgression,
      estimate = mat["Yes", "ConditionalEffect"],
      ci_low   = mat["Yes", "2.5% CI"],
      ci_high  = mat["Yes", "97.5% CI"]
    )
  })
  do.call(rbind, out)
}

df_plot <- bind_rows(
  ce_to_df(ce_bonus_hss1,     "HSS Graduates", "Media Freedom"),
  ce_to_df(ce_bonus_non_hss1, "Non-HSS Graduates", "Media Freedom"),
  ce_to_df(ce_bonus_hss2,     "HSS Graduates", "Election Integrity"),
  ce_to_df(ce_bonus_non_hss2, "Non-HS Graduates", "Election Integrity")
)
##PLOT
ggplot(df_plot, aes(
  x = Statement,
  y = estimate,
  color = Group,
  shape = Transgression
)) +
  geom_point(position = position_dodge(width = 0.35), size = 3) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.15,
    position = position_dodge(width = 0.35)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~Group) +
  labs(
    y = "Change in Probability (In-party Vote)",
    x = NULL,
    color = "Candidate",
    shape = "Transgression Strength"
  ) +
  theme_minimal()

##EXTRACT DATA FOR TABLE 
extract_full_amie <- function(obj, group_label, statement_label) {
  imap_dfr(obj$ConditionalEffects, function(df, node_name) {
    res <- as.data.frame(df)
    res$Inparty <- rownames(res)
    res$Group <- group_label
    res$Statement <- statement_label
    res$Severity <- ifelse(grepl("Mild", node_name), "Mild", "Strong")
    return(res)
  })
}

full_amie_data <- ##COMBINE ALL AMIE DATA
  bind_rows(
  extract_full_amie(ce_bonus_hss1, "HSS Graduates", "Media Freedom"),
  extract_full_amie(ce_bonus_non_hss1, "non-HSS Graduates", "Media Freedom"),
  extract_full_amie(ce_bonus_hss2, "HSS Graduates", "Election Integrity"),
  extract_full_amie(ce_bonus_non_hss2, "non-HSS Graduates", "Election Integrity")
)

table_final <- 
  full_amie_data %>%
  mutate(
    se = (`97.5% CI` - `2.5% CI`) / 3.92,
    z_score = ifelse(se > 0, abs(ConditionalEffect / se), 0),
    p_val_raw = 2 * (1 - pnorm(z_score)),
    Estimate_CI = paste0(round(ConditionalEffect, 3), 
                         " [", round(`2.5% CI`, 3), ", ", round(`97.5% CI`, 3), "]"),
    P_Value = case_when(
      ConditionalEffect == 0 ~ "-",
      p_val_raw < 0.001 ~ "< 0.001",
      TRUE ~ as.character(round(p_val_raw, 3))
    )
  ) %>%
  dplyr::select(Statement, Severity, Inparty, Group, Estimate_CI, P_Value) %>%
  arrange(Statement, desc(Severity), desc(Inparty)) %>%
  mutate(
    Statement = case_when(
      grepl("1|Integrity", Statement) ~ "Election Integrity",
      grepl("9|Media", Statement) ~ "Media Freedom",
      TRUE ~ "Other Statement"
    ),
    Inparty = case_when(
      grepl("Yes", Inparty) ~ "Yes",
      grepl("No", Inparty) ~ "No",
      TRUE ~ Inparty
    )
  )

table_for_kable <- as.data.frame(table_final)
rownames(table_for_kable) <- NULL
#TABLE
table_for_kable %>%
  kable(format = "latex",
    col.names = c("Statement", "Severity", "In-party", "Group", "Estimate [95% CI]", "p-value"),
    align = "llccrc",
    booktabs = TRUE 
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  collapse_rows(columns = 1:2, valign = "top")

##
##
#H3: FORCED CHOICE 
specialconjoint <- ## READ DATA 
  as.data.frame(read.csv("specialconjoint_ma.csv"))

###BALANCE TESTS 
balance_df <- 
  specialconjoint %>%
  mutate(
    rural = factor(rural, labels = c("Urban", "Rural")),
    income = factor(income,
                    labels = c("Lowest", "Low", "Middle", "High", "Highest")),
    major_hss = factor(major_hss,
                       labels = c("Non-HSS", "HSS")),
    partyid_true = factor(partyid_true,
                          labels = c("Civic Coalition", "Law and Justice")),
    gender = factor(gender,
                    labels = c("Male", "Female")),
    bornpre_1989 = factor(bornpre_1989,
                          labels = c("Born ≥1989", "Born <1989")),
    parentseducation = factor(parentseducation,
                              labels = c("No Higher Ed", "Higher Ed"))
  )


tab <- balance_df %>%
  tbl_summary(
    by = strongundem,
    include = c(rural, income, major_hss, partyid_true,
                gender, bornpre_1989, parentseducation, ct_norm),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing = "no"
  ) %>%
  add_p() 

tab %>%
  as_kable_extra(format = "latex", booktabs = TRUE) %>%
  kable_styling()

treated <- ##SUBSET FOR TREATED ONLY
  specialconjoint %>%
  filter(strongundem == 1)

h3 <- ##MODEL IN-PARTY CHOICE
  lm(
  inpartychosen ~ major_hss  + 
    Age + Gender + PreviousOccupation, 
  data = treated
)
summary(h3)

controls_row <- c("Controls", "Yes")
#TABLE
stargazer(h3,  
          type = "latex", 
          dep.var.labels = "In-party candidate chosen",
          covariate.labels = c("HSS Major"),
          omit = c("Age", "Gender", "PreviousOccupation"),
          add.lines = list(controls_row),
          star.cutoffs = c(0.1, 0.05, 0.01))
#PLOT
plot_predictions(h3, condition = "major_hss") +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Education Group (0 = Non-HSC, 1 = HSC)",
    y = "Predicted Probability of Choosing In-Party Candidate"
  ) +
  theme_minimal()

#CRITICAL THINKING MODEL
h3_1 <- #CRITICAL THINKING MODEL
  lm(
  inpartychosen ~ ct_norm  + 
    Age + Gender + PreviousOccupation, 
  data = treated
)
summary(h3_1)

stargazer(h3_1,  
          type = "latex", 
          dep.var.labels = "In-party candidate chosen",
          covariate.labels = c("Critcal Thinking"),
          omit = c("Age", "Gender", "PreviousOccupation"),
          add.lines = list(controls_row),
          star.cutoffs = c(0.1, 0.05, 0.01))

##CRITICAL THINKING ON MAJOR MODEL
h3_2 <- 
  lm(ct_norm ~ major_hss + gender + rural + income + parentseducation + bornpre_1989,
     data = specialconjoint)
summary(h3_2)

stargazer(h3_2,  
          type = "latex", 
          dep.var.labels = "Critical Thinking",
          covariate.labels = c("HSS major"),
          omit = c("gender", "rural", "income", "parentseducation", "bornpre_1989"),
          add.lines = list(controls_row),
          star.cutoffs = c(0.1, 0.05, 0.01))
