library(dplyr)
library(survey)
library(tableone)
library(broom)
library(ggplot2)
library(tidyverse)
library(haven)
library(stringr)
library(logistf)
library(mice)

df <- read.csv("D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\hrs.csv")

####preparing the df####
##define sample size; n = 21029
sum(df$inw2010)
df <- df[df$inw2010 == 1,]

##exclude age < 50; n = 19280 
df <- df[which(df$age2010 >= 50), ]

##delete na in education; n = 19257
df <- subset(df, !(is.na(years_edu)))

## number of missing of parental education
missing_mother <- sum(is.na(df$years_edu_m))
prop_mother <- missing_mother / nrow(df)

missing_father <- sum(is.na(df$years_edu_f))
prop_father <- missing_father / nrow(df)

#### construct the exposure: education mobility ####
df <- subset(df, !(is.na(years_edu_f) & is.na(years_edu_m)))

df$highest_education_parent <- ifelse(
  is.na(df$years_edu_f), df$years_edu_m,
  ifelse(
    is.na(df$years_edu_m), df$years_edu_f,
    ifelse(df$years_edu_f > df$years_edu_m, df$years_edu_f, df$years_edu_m)
  )
)

##exclude NA for outcomes dementia - baseline 2010
df <- df %>% filter(!is.na(cogfunction2010))

df <- df %>%
  mutate(
    cognitive_score_2010 = word_recall2010 + r10ser7 ,
    cognitive_missing = is.na(word_recall2010) + is.na(r10ser7)
  ) %>%
  mutate(
    cognitive_score_2010 = ifelse(cognitive_missing > 1, NA, cognitive_score_2010)
  )

# group the respondent by birth year
df$decade <- ifelse(df$birth_year >= 1913 & df$birth_year <= 1929, 1920,
                    ifelse(df$birth_year >= 1930 & df$birth_year <= 1939, 1930,
                           ifelse(df$birth_year >= 1940 & df$birth_year <= 1949, 1940,
                                  ifelse(df$birth_year >= 1950 & df$birth_year <= 1959, 1950,
                                         1960))))

###1. percentile rank of education level
df <- df %>%
  group_by(decade) %>%
  mutate(percentile_rank = rank(years_edu, ties.method = c("max")) / length(years_edu))

df <- df %>%
  group_by(decade) %>%
  mutate(percentile_rank_parent = rank(highest_education_parent, ties.method = c("max")) / length(highest_education_parent))

lm_model <- lm(df$percentile_rank ~ df$percentile_rank_parent)
df$educational_mobility_residual <- resid(lm_model)

###2. categorized by percentile ranking
df$education_quan <- cut(
  df$percentile_rank,
  breaks = quantile(df$percentile_rank, probs = c(0, 0.5, 1)),
  labels = c(1, 2),
  include.lowest = TRUE
)

df$education_p_quan <- cut(
  df$percentile_rank_parent,
  breaks = quantile(df$percentile_rank_parent, probs = c(0, 0.5, 1)),
  labels = c(1, 2),
  include.lowest = TRUE
)

df$educational_mobility_pr <- NA
df$educational_mobility_pr[df$education_p_quan == 1 & df$education_quan == 1] <- 1
df$educational_mobility_pr[df$education_p_quan == 2 & df$education_quan == 2] <- 2
df$educational_mobility_pr[as.numeric(df$education_p_quan) > as.numeric(df$education_quan)] <- 3
df$educational_mobility_pr[as.numeric(df$education_p_quan) < as.numeric(df$education_quan)] <- 4

df$educational_mobility_pr <- factor(df$educational_mobility_pr,
                                     levels = c(1, 3, 4, 2),
                                     labels = c("Stably low", "Downward mobility", "Upward mobility", "Stably high"))
df$educational_mobility_pr <- relevel(df$educational_mobility_pr, ref = "Stably low")

####construct covariates for 2010####
#age
df$age <- df$age2010

#gender: 1 is men; 2 is women
df$gender <- ifelse(df$gender == 1, "men", "women")
df$gender <- factor(df$gender)

#marital status
df$marital_status <- ifelse(df$marital_status2010 == 1 | df$marital_status2010 == 3, "married/partnered", "other")

#race and hispanic
df$Race <- ifelse(df$race == 1 & df$hispanic == 0, 0,
                  ifelse(df$race == 2 & df$hispanic == 0, 1,
                         ifelse(df$hispanic == 1, 2, 3)))

df$Race <- ifelse(df$Race == 0, "Non-Hispanic White",
                  ifelse(df$Race == 1, "Non-Hispanic Black",
                         ifelse(df$Race == 2, "Hispanic", "Other")))

#urbanicity
df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity2010 == 0 ~ "urban",
    urbanicity2010 == 1 ~ "rural",
    TRUE ~ NA_character_
  ))
df$urbanicity <- factor(df$urbanicity)

#labour force status
df$lb_status <- ifelse(!is.na(df$lb_status2010) & df$lb_status2010 == 1, "fulltime_employed", 
                       ifelse(!is.na(df$lb_status2010), "other", NA))

#smoking
df$smoking <- ifelse(df$smoke_now2010 == 1, "current smoker",
                     ifelse(df$smoke_now2010 == 0 & df$smoke_ever2010 == 1, "past smoker", "non smoker"))
df$smoking <- factor(df$smoking, levels = c("non smoker", "past smoker", "current smoker"))

#drinking
df$drink_now <- ifelse(df$drink_frq2010 == 1, 1, 0)

#number of chronic diseases
df <- df %>%
  mutate(n_chronic_category = case_when(
    n_chronic2010 == 0 | n_chronic2010 == 1 ~ "0 or 1 chronic",
    n_chronic2010 >= 2 & n_chronic2010 <= 6 ~ "more than 2 chronic",
    TRUE ~ NA_character_
  ))

#physical activity
freq_map <- c("1" = 7, "2" = 2, "3" = 1, "4" = 0.25, "5" = 0)
df$freq_moderate <- freq_map[as.character(df$mode_act2010)]
df$freq_vigorous <- freq_map[as.character(df$vigo_act2010)]
df$total_freq <- ifelse(is.na(df$freq_moderate) | is.na(df$freq_vigorous), NA, 
                        df$freq_moderate + df$freq_vigorous)
df <- df %>%
  mutate(phy_act = ifelse(is.na(total_freq), NA, ifelse(total_freq > 2, 1, 0)))

#wealth
df$people_living_with2010 <- as.numeric(as.character(df$people_living_with2010))
df$equivalized_wealth2010 <- ifelse(is.na(df$people_living_with2010) | is.na(df$household_wealth2010), NA, 
                                    df$household_wealth2010 / sqrt(df$people_living_with2010))
df$equivalized_wealth <- ifelse(is.na(df$equivalized_wealth2010), NA, 
                                round(df$equivalized_wealth2010 / 1000, 2))

quantile_breaks <- quantile(df$equivalized_wealth, probs = c(0, 0.5, 1), na.rm = TRUE)
df$wealth_quartile <- ifelse(is.na(df$equivalized_wealth), NA,
                             cut(df$equivalized_wealth,
                                 breaks = quantile_breaks,
                                 include.lowest = TRUE,
                                 labels = c("1", "2")))

#nativity
df$us_born <- ifelse(df$us_born == 11, 0, 1)

####childhood indicators####
childhood <- read_dta('D:\\R project\\Educational mobility and depression\\HRS\\hrs_childhood\\AGGCHLDFH2016A_R.dta')
childhood <- childhood %>%
  mutate(hhidpn = str_c(HHID, PN, sep = ""))

childhood <- childhood %>%
  mutate(
    financial = case_when(
      FAMFIN == 5 ~ "YES",
      MOVFIN == 1 ~ "YES",
      FMFINH == 1 ~ "YES",
      FAMFIN %in% c(1, 3) & MOVFIN == 5 & FMFINH == 5 ~ "NO",
      TRUE ~ NA_character_
    ),
    
    stress = case_when(
      TRPOLICE == 1 ~ "YES",
      DRKDRUG == 1 ~ "YES",
      PHYABUSE == 1 ~ "YES",
      SCHLOVER == 1 ~ "YES",
      TRPOLICE == 5 & DRKDRUG == 5 & PHYABUSE == 5 & SCHLOVER == 5 ~ "NO",
      TRUE ~ NA_character_
    ),
    
    warmth = case_when(
      RELWMO == 5 & RELWFA == 5 & 
        (as.numeric(ATTENMO == 1) + as.numeric(EFFMO == 1) + as.numeric(TEACHMO == 1)) >= 2 ~ "YES",
      (RELWMO == 1 | RELWFA == 1) & 
        (as.numeric(ATTENMO == 4) + as.numeric(EFFMO == 4) + as.numeric(TEACHMO == 4)) >= 2 ~ "NO",
      (RELWMO %in% c(1, 2) | RELWFA %in% c(1, 2)) & 
        (ATTENMO == 4 | EFFMO == 4 | TEACHMO == 4) ~ "NO",
      RELWMO == 6 | RELWFA == 6 ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

childhood <- childhood %>%
  mutate(health = case_when(
    RTHLTHCH %in% c(1, 2, 3) ~ "healthy",
    RTHLTHCH %in% c(4, 5) ~ "less_healthy", 
    TRUE ~ NA_character_
  ))

childhood$hhidpn <- as.numeric(childhood$hhidpn)
childhood <- dplyr::select(childhood, c("hhidpn", "health", "stress", "financial", "warmth"))
df <- left_join(df, childhood, by = c("hhidpn"))

# Create dementia indicator for 2010 only (cross-sectional)
df <- df %>%
  mutate(
    dementia_2010 = ifelse(is.na(cogfunction2010), NA, ifelse(cogfunction2010 == 3, 1, 0))
  )

# Remove rows with missing dementia outcome
df_cs <- df[!is.na(df$dementia_2010),]

cat("Sample size for cross-sectional analysis:", nrow(df_cs), "\n")
cat("Dementia prevalence:\n")
print(table(df_cs$dementia_2010, useNA = "ifany"))

####========== PREPARE DATASET FOR ALL ANALYSES ==========####

variables_analyze <- c("age", "gender", "lb_status", "marital_status", "wealth_quartile", 
                       "drink_now", "smoking", "n_chronic_category", "us_born", "urbanicity", 
                       "Race", "stress", "health", "financial", "warmth", "height2010", "phy_act", "soc_activity2010")

# Prepare dataset for imputation
df_cs_impute <- df_cs[, c(variables_analyze, "dementia_2010", "educational_mobility_pr")]
df_cs_impute <- df_cs_impute %>%
  mutate_if(names(.) %in% c("gender", "lb_status", "marital_status", "wealth_quartile", 
                            "drink_now", "smoking", "n_chronic_category", "us_born", 
                            "urbanicity", "Race", "stress", "health", "financial", 
                            "warmth", "phy_act", "soc_activity2010"), as.factor)

# Imputation using mice
set.seed(1005)
mice_cs <- mice(df_cs_impute, method = "cart", m = 1, maxit = 5)
imputed_cs <- complete(mice_cs)

# Replace imputed values into original dataset
common_cols_cs <- intersect(names(df_cs), names(imputed_cs))
df_cs[common_cols_cs] <- imputed_cs[common_cols_cs]


# Set reference levels
df_cs$educational_mobility_pr <- factor(df_cs$educational_mobility_pr, 
                                        levels = c("Stably low","Downward mobility","Upward mobility", "Stably high"))
df_cs$educational_mobility_pr <- relevel(df_cs$educational_mobility_pr, ref = "Stably low")
df_cs$Race <- factor(df_cs$Race, levels = c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Other"))
df_cs$Race <- relevel(df_cs$Race, ref = "Non-Hispanic White")
df_cs$gender <- relevel(factor(df_cs$gender), ref = "men")
df_cs$wealth_quartile <- relevel(factor(df_cs$wealth_quartile), ref = "1")
df_cs$n_chronic_category <- relevel(factor(df_cs$n_chronic_category), ref = "0 or 1 chronic")
df_cs$drink_now <- relevel(factor(df_cs$drink_now), ref = "0")
df_cs$smoking <- relevel(factor(df_cs$smoking), ref = "non smoker")
df_cs$education_quan <- relevel(factor(df_cs$education_quan), ref = "1")
df_cs$us_born <- factor(df_cs$us_born)
df_cs$phy_act <- factor(df_cs$phy_act)
df_cs$soc_activity2010 <- factor(df_cs$soc_activity2010)
df_cs$dementia_2010 <- as.numeric(df_cs$dementia_2010)

#============
#MAIN ANALYSIS - Logistic Regression - gender interaction
#============
model_main <- glm(dementia_2010 ~ educational_mobility_pr * gender + age  + Race + 
                    height2010 + stress + health + 
                    financial + warmth+ us_born, 
                  data = df_cs, family = binomial(link = "logit"))
library(sjPlot)
tab_model(model_main)


========== SA 1: Using 1.5 SD CUTOFF ==========####
classify_dementia_1.5sd <- function(cog_scores, edu_levels) {
  dementia <- rep(NA_real_, length(cog_scores))
  edu_unique <- unique(edu_levels[!is.na(edu_levels)])
  
  for (edu_level in edu_unique) {
    idx <- which(edu_levels == edu_level & !is.na(edu_levels))
    cog_subset <- cog_scores[idx]
    
    mean_cog <- mean(cog_subset, na.rm = TRUE)
    sd_cog <- sd(cog_subset, na.rm = TRUE)
    cutoff <- mean_cog - 1.5 * sd_cog
    
    for (i in idx) {
      if (!is.na(cog_scores[i])) {
        dementia[i] <- ifelse(cog_scores[i] < cutoff, 1, 0)
      }
    }
  }
  
  return(dementia)
}

# Apply 1.5 SD cutoff for 2010 cognition
df_cs$dementia_2010_1.5sd <- classify_dementia_1.5sd(df_cs$cognitive_score_2010, df_cs$H_edu)

# 
# 
# # Remove NA values for analysis
# df_sa1 <- df_cs %>% 
#   filter(!is.na(dementia_2010_1.5sd) & !is.na(educational_mobility_pr)) %>%
#   mutate(dementia_2010_1.5sd = as.numeric(dementia_2010_1.5sd))

model_sa1 <- glm(dementia_2010_1.5sd ~ educational_mobility_pr + age + gender + Race + 
                   height2010 + stress + health + 
                   financial + warmth+ us_born, 
                 data = df_cs, family = binomial(link = "logit"), maxit = 50)

tab_model(model_sa1)



####========== SA 3: SAMPLING WEIGHTS ==========####
df_cs_weights <- df_cs %>%
  filter(!is.na(s_weight) & s_weight > 0)

# Normalize weights
df_cs_weights$s_weight_norm <- df_cs_weights$s_weight / sum(df_cs_weights$s_weight, na.rm = TRUE) * nrow(df_cs_weights)

# Weighted logistic model
model_weights <- glm(dementia_2010 ~ educational_mobility_pr + age + gender + Race + 
                       height2010 + stress + health + 
                       financial + warmth+ us_born, 
                     data = df_cs_weights, family = binomial(link = "logit"),
                     weights = s_weight_norm)

tab_model(model_weights)

####========== SA 4: COMPLETE CASE ANALYSIS ==========####
# Create complete case dataset
df_cs_complete <- as.data.frame(df_cs)
cols_needed <- c("educational_mobility_pr", "dementia_2010", variables_analyze)
df_cs_complete <- df_cs_complete[, cols_needed]

# Keep only complete cases
complete_idx <- complete.cases(df_cs_complete)
df_cs_complete <- df_cs_complete[complete_idx, ]


# Set reference levels for complete case dataset
df_cs_complete$educational_mobility_pr <- relevel(factor(df_cs_complete$educational_mobility_pr), ref = "Stably low")
df_cs_complete$Race <- relevel(factor(df_cs_complete$Race), ref = "Non-Hispanic White")
df_cs_complete$gender <- relevel(factor(df_cs_complete$gender), ref = "men")
df_cs_complete$wealth_quartile <- relevel(factor(df_cs_complete$wealth_quartile), ref = "1")
df_cs_complete$n_chronic_category <- relevel(factor(df_cs_complete$n_chronic_category), ref = "0 or 1 chronic")
df_cs_complete$drink_now <- relevel(factor(df_cs_complete$drink_now), ref = "0")
df_cs_complete$smoking <- relevel(factor(df_cs_complete$smoking), ref = "non smoker")
df_cs_complete$us_born <- factor(df_cs_complete$us_born)
df_cs_complete$phy_act <- factor(df_cs_complete$phy_act)
df_cs_complete$soc_activity2010 <- factor(df_cs_complete$soc_activity2010)
df_cs_complete$dementia_2010 <- as.numeric(df_cs_complete$dementia_2010)

# Logistic regression on complete cases
model_complete <- glm(dementia_2010 ~ educational_mobility_pr + age + gender + Race + 
                        height2010 + stress + health + 
                        financial + warmth + us_born, 
                      data = df_cs_complete, family = binomial(link = "logit"))

tab_model(model_complete)




