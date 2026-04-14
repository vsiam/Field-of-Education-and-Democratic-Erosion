#Field of education and democratic Erosion 
#Pilot data cleaning and recoding
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
  "stargazer"
)
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}
invisible(lapply(packages, library, character.only = TRUE))

####
education <- ## LOAD DATA
  as.data.frame(read.csv("soft_launch_data.csv"))

education_clean <- #CLEANING
  education %>%
  slice(-c(1, 2)) %>% ## REMOVE FIRST TWO ROWS
  filter(Finished == "True") %>% ##REMOVE NON-FINISHERS
  filter(IPAddress != "180.190.125.121" ) %>% ##REMOVE DYNATA TEST TRIALS
  filter(age_18 == "Yes" & residency == "Yes" & ba_degree == "Yes") %>% ##REMOVE NON-QUALIFIERS
  filter(manipulation == "Recent laws passed by Sejm") ##REMOVE THOSE WHO FAILED MANIPULATION CHECK 

education_recoded <- ##RECODE DEMOGRAPHIC VARS
  education_clean %>%
  mutate(
    rural = recode(rural,
                   "Large City" = 0,
                   "Small town" = 0,
                   "Village" = 1
    ) %>% as.numeric(),
    gender = recode(gender,
                    "Female" = 1,
                    "Male" = 0
    ) %>% as.numeric(),
    parentseducation = recode(parentseducation,
                              "Yes" = 1, 
                              "No" = 0,
                              "I am not sure/I do not wish to answer" = NA_real_
    ) %>% as.numeric(),
    income = recode(income,
                    "Less than 15,000 zl" = 0,
                    "15,000 zl - 40,000 zl" = 1,
                    "40,000 zl - 75,000 zl" = 2,
                    "75,000 zl - 150,000 zl" = 3,
                    "More than 150,000 zl" = 4,
                    "I am not sure/I do not wish to answer" = NA_real_
    ) %>% as.numeric(),
    bornpre_1989 = case_when(
      year_born < 1989 ~ 1, 
      year_born >= 1989 ~ 0, 
      .default = NA_real_
    )
  )

##coding education field 
degree_else_hsc <- ##WRITE-IN OPTIONS
  c(
    "Prawo",
    "Magister prawa",
    "magister prawa",
    "MAGISTER KULTUROZNAWSTWA W ZAKRESIE TEORII 
  I HISTORII KULTURY oraz MAGISTER BIBLIOTEKOZNAWSTWA I INFORMACJI NAUKOWEJ",
    "Filologia polska",
    "Dyplomowany ekonomista"
  )

education_recoded <- ## 1 if chose in a question or if equals to write-in
  education_recoded %>%
  mutate(
    major_hss = as.numeric(
      (!is.na(degree_hsc) & degree_hsc != "") |
        (!is.na(seconddegree_hsc) & seconddegree_hsc != "") |
        (!is.na(degree_else) & degree_else %in% degree_else_hsc) |
        (!is.na(seconddegree_else) & seconddegree_else %in% degree_else_hsc)
    )
  )

education_recoded <- ##CODING PARTY-ID 
  education_recoded %>%
  mutate(
    partyid_true = case_when(
      partyid %in% c("Civic Coalition", "Law and Justice") ~ partyid,
      partyid == "Other party" & party_id_else %in% c("Civic Coalition", "Law and Justice") ~ party_id_else,
      TRUE ~ NA_character_
    )
  )

ct_key <- ##CRITICAL THINKING TEST KEY
  c(
  "ct_1_1" = "Weak Argument",
  "ct_1_2" = "Weak Argument",
  "ct_1_3" = "Strong Argument",
  "ct_2_1" = "Assumption is not justified",
  "ct_2_2" = "Assumption is not justified",
  "ct_2_3" = "Assumption is not justified",
  "ct_3_1" = "True",
  "ct_3_2" = "True",
  "ct_3_3" = "False",
  "ct_4_1" = "Conclusion does not follow",
  "ct_4_2" = "Conclusion follows",
  "ct_4_3" = "Conclusion follows",
  "ct_5_1" = "Conclusion follows",
  "ct_5_2" = "Conclusion does not follow",
  "ct_5_3" = "Conclusion follows"
)


education_recoded <- ##CREATING INDEX
  education_recoded %>%
  mutate(
    # Convert all CT items to 1/0 based on the key
    across(all_of(names(ct_key)), ~ as.numeric(.x == ct_key[cur_column()]), .names = "score_{col}"),
    # Sum total correct
    ct_total = rowSums(across(starts_with("score_ct_")), na.rm = TRUE),
    # Normalize to 0-1
    ct_norm = ct_total / length(ct_key)
  )

write.csv(education_recoded, file = "ma_recoded.csv")
#######
#######
#CREATING CONJOINT DATA 
attributes <- #ATTRIBUTES
  c("Age", "Gender", "PreviousOccupation", "PoliticalParty", "PoliticalStatement1", "PoliticalStatement2")

resp_vars <- #COVARIATES
  c("ResponseId", "rural", "income", "major_hss", "gender", "partyid_true", "ct_norm")

attr_cols <- #ATTRIBUTE COLS
  grep("Task\\d+_Profile\\d+_.*", names(education_recoded), value = TRUE)

choice_cols <- #CHOICE COLS
  grep("conj_\\d+_1", names(education_recoded), value = TRUE)

conjoint_long <- #PIVOTING 
  education_recoded %>%
  dplyr::select(all_of(resp_vars),
                matches("Task\\d+_Profile\\d+_.*"), matches("conj_\\d+_1")) %>%
  #PIVOT LONGER
  pivot_longer(   
    cols = matches("Task\\d+_Profile\\d+_.*"),
    names_to = c("task", "profile", "attribute"),
    names_pattern = "Task(\\d+)_Profile(\\d+)_(.*)",
    values_to = "value"
  ) %>%
  # SPREAD
  pivot_wider(
    names_from = "attribute",
    values_from = "value"
  ) %>%
  # ASSIGN CHOICES
  rowwise() %>%
  mutate(
    choice = {
      
      col_name <- paste0("conj_", task, "_1")
      choice_val <- cur_data()[[col_name]] 
      if (profile == "1" && choice_val == "Candidate A") 1 else
        if (profile == "2" && choice_val == "Candidate B") 1 else 0
    }
  ) %>%
  ungroup() %>%
  #CONVERT TO FACTORS 
  mutate(
    ResponseId = as.factor(ResponseId),
    task = as.factor(task),
    profile = as.factor(profile)
  ) %>%
  arrange(ResponseId, task, profile)

conjoint_long <- ##TRANSLATING LEVELS; RECODING POLITICAL STATEMENTS, CONVERTING TO FACTORS 
  conjoint_long %>%
  mutate(
    Age = factor(Age, levels = c(39, 47, 54, 59, 62)),
    
    Gender = factor(Gender,
                    levels = c("Mężczyzna", "Kobieta"),
                    labels = c("Male", "Female")),
    
    PreviousOccupation = factor(PreviousOccupation,
                                levels = c("Członek rządu", "Przedsiębiorca", "Działacz społeczny"),
                                labels = c("Member of the Cabinet", "Businessperson", "Civil Activist")),
    
    PoliticalParty = factor(PoliticalParty,
                            levels = c("Koalicja Obywatelska", "Prawo i Sprawiedliwość"),
                            labels = c("Civic Coalition", "Law and Justice")),
    
    PoliticalStatement1 = case_when(
      PoliticalStatement1 == "W poście na Twitterze kandydat odpowiedział dziennikarzom z państwowej stacji telewizyjnej, którzy krytykowali kampanię, nazywając ich kłamcami i zdrajcami" ~ "PS1_Mild",
      PoliticalStatement1 == "W poście na Twitterze kandydat odpowiedział dziennikarzom z państwowej stacji telewizyjnej, którzy krytykowali kampanię, wzywając policję do ich aresztowania" ~ "PS1_Strong",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("PS1_Mild", "PS1_Strong")),
    
    PoliticalStatement2 = case_when(
      PoliticalStatement2 == "W wywiadzie kandydat mówił o przeciwniku politycznym, o którym krążą pogłoski o udział w przestępstwach gospodarczych, nazywając go zdrajcą" ~ "PS2_Mild",
      PoliticalStatement2 == "W wywiadzie kandydat mówił o przeciwniku politycznym, o którym krążą pogłoski o udział w przestępstwach gospodarczych, wzywając do zakazu startu w wyborach" ~ "PS2_Strong",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("PS2_Mild", "PS2_Strong")),
    
    # In-Party attribute
    Inparty = case_when(
      is.na(partyid_true) ~ NA_character_,
      PoliticalParty == partyid_true ~ "Yes",
      PoliticalParty != partyid_true ~ "No"
    ) %>% factor(levels = c("No", "Yes"))
  ) %>%
  mutate(
    major_hss = factor(major_hss,
                       levels = c(0, 1),
                       labels = c("Non-HSS", "HSS"))
  )

write.csv(conjoint_long, file = "ma_conjoint_full.csv")

##specialconjoint data
specialconjoint <- ##SELECT VARS
  education_recoded %>% 
  dplyr::select(all_of(resp_vars), parentseducation, bornpre_1989, special_conj_1_1,
                matches("Special+_Profile\\d+_.*"))

specialconjoint <- ##CONVERT TO FACTORS, TRANSLATE, RECODE
  specialconjoint %>%
  mutate(
    Age = factor(Special_Profile1_Age, levels = c(39, 47, 54, 59, 62)),
    
    Gender = factor(Special_Profile1_Gender,
                    levels = c("Mężczyzna", "Kobieta"),
                    labels = c("Male", "Female")),
    
    PreviousOccupation = factor(Special_Profile1_PreviousOccupation,
                                levels = c("Członek rządu", "Przedsiębiorca", "Działacz społeczny"),
                                labels = c("Member of the Cabinet", "Businessperson", "Civil Activist")),
    
    Special_Profile1_PoliticalParty = factor(Special_Profile1_PoliticalParty,
                                             levels = c("Koalicja Obywatelska", "Prawo i Sprawiedliwość"),
                                             labels = c("Civic Coalition", "Law and Justice")),
    
    Special_Profile2_PoliticalParty = factor(Special_Profile2_PoliticalParty,
                                             levels = c("Koalicja Obywatelska", "Prawo i Sprawiedliwość"),
                                             labels = c("Civic Coalition", "Law and Justice")),
    
    Special_Profile1_PoliticalStatement1 = case_when(
      Special_Profile1_PoliticalStatement1 == "W poście na Twitterze kandydat odpowiedział dziennikarzom z państwowej stacji telewizyjnej, którzy krytykowali kampanię, nazywając ich kłamcami i zdrajcami" ~ "PS1_Mild",
      Special_Profile1_PoliticalStatement1 == "W poście na Twitterze kandydat odpowiedział dziennikarzom z państwowej stacji telewizyjnej, którzy krytykowali kampanię, wzywając policję do ich aresztowania" ~ "PS1_Strong",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("PS1_Mild", "PS1_Strong")),
    
    Special_Profile1_PoliticalStatement2 = case_when(
      Special_Profile1_PoliticalStatement2 == "W wywiadzie kandydat mówił o przeciwniku politycznym, o którym krążą pogłoski o udział w przestępstwach gospodarczych, nazywając go zdrajcą" ~ "PS2_Mild",
      Special_Profile1_PoliticalStatement2 == "W wywiadzie kandydat mówił o przeciwniku politycznym, o którym krążą pogłoski o udział w przestępstwach gospodarczych, wzywając do zakazu startu w wyborach" ~ "PS2_Strong",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("PS2_Mild", "PS2_Strong")),
    
    Special_Profile2_PoliticalStatement1 = case_when(
      Special_Profile2_PoliticalStatement1 == "W poście na Twitterze kandydat odpowiedział dziennikarzom z państwowej stacji telewizyjnej, którzy krytykowali kampanię, nazywając ich kłamcami i zdrajcami" ~ "PS1_Mild",
      Special_Profile2_PoliticalStatement1 == "W poście na Twitterze kandydat odpowiedział dziennikarzom z państwowej stacji telewizyjnej, którzy krytykowali kampanię, wzywając policję do ich aresztowania" ~ "PS1_Strong",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("PS1_Mild", "PS1_Strong")),
    
    Special_Profile2_PoliticalStatement2 = case_when(
      Special_Profile2_PoliticalStatement2 == "W wywiadzie kandydat mówił o przeciwniku politycznym, o którym krążą pogłoski o udział w przestępstwach gospodarczych, nazywając go zdrajcą" ~ "PS2_Mild",
      Special_Profile2_PoliticalStatement2 == "W wywiadzie kandydat mówił o przeciwniku politycznym, o którym krążą pogłoski o udział w przestępstwach gospodarczych, wzywając do zakazu startu w wyborach" ~ "PS2_Strong",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("PS2_Mild", "PS2_Strong"))
  )

specialconjoint <- ##TREATMENT CODING
  specialconjoint %>%
  mutate(
    ##PROFILES THAT MATCH RESPONDENTS PARTY
    inparty_profile = case_when(
      Special_Profile1_PoliticalParty == partyid_true ~ 1,
      Special_Profile2_PoliticalParty == partyid_true ~ 2,
      TRUE ~ NA_real_
    ),
    
   ##HARD CHOICES
    strongundem = case_when(
      inparty_profile == 1 & 
        (Special_Profile1_PoliticalStatement1 == "PS1_Strong" & Special_Profile2_PoliticalStatement1 == "PS1_Mild" &
           Special_Profile1_PoliticalStatement2 == "PS2_Strong" & Special_Profile2_PoliticalStatement2 == "PS2_Mild") ~ 1,
      inparty_profile == 2 & 
        (Special_Profile2_PoliticalStatement1 == "PS1_Strong" & Special_Profile1_PoliticalStatement1 == "PS1_Mild" &
           Special_Profile2_PoliticalStatement2 == "PS2_Strong" & Special_Profile1_PoliticalStatement2 == "PS2_Mild") ~ 1,
      TRUE ~ 0
    ),
    
    ##COVERT CANDIDATES TO 1 and 2
    choice_numeric = case_when(
      special_conj_1_1 %in% c("Candidate A", "1", "A") ~ 1,
      special_conj_1_1 %in% c("Candidate B", "2", "B") ~ 2,
      TRUE ~ NA_real_
    ),
    
    ##COMPARE NUMERIC CHOISE TO PARTY ID
    inpartychosen = ifelse(choice_numeric == inparty_profile, 1, 0)
  ) %>%
  ##FACTOR EDUCATION MAJOR
  mutate(
    major_hss = factor(major_hss,
                       levels = c(0, 1),
                       labels = c("Non-HSS", "HSS"))
  )
write.csv(specialconjoint, file = "specialconjoint_ma.csv")