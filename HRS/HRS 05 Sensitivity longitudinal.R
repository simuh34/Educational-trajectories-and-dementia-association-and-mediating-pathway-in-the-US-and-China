library(dplyr)
library(survival)
library(mice)
library(tableone)
library(broom)
library(ggplot2)
library(tidyverse)
library(haven)
library(stringr)

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

##exclude NA for outcomes dementia
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

####construct covariates####
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

# Create dementia indicators for each wave
df <- df %>%
  mutate(
    cogfunction2010_dementia = ifelse(is.na(cogfunction2010), NA, ifelse(cogfunction2010 == 3, 1, 0)),
    cogfunction2012_dementia = ifelse(is.na(cogfunction2012), NA, ifelse(cogfunction2012 == 3, 1, 0)),
    cogfunction2014_dementia = ifelse(is.na(cogfunction2014), NA, ifelse(cogfunction2014 == 3, 1, 0)),
    cogfunction2016_dementia = ifelse(is.na(cogfunction2016), NA, ifelse(cogfunction2016 == 3, 1, 0)),
    cogfunction2018_dementia = ifelse(is.na(cogfunction2018), NA, ifelse(cogfunction2018 == 3, 1, 0))
  )

# EXCLUDE baseline dementia cases at 2010 (cogfunction2010 == 3)
cat("Before excluding baseline dementia (2010):", nrow(df), "\n")
df <- df %>% filter(cogfunction2010_dementia == 0 | is.na(cogfunction2010_dementia))
cat("After excluding baseline dementia (2010):", nrow(df), "\n")

# Define event status and event time (从2012开始追踪，不包括baseline的dementia)
df <- df %>%
  mutate(
    event_status = case_when(
      cogfunction2012_dementia == 1 | cogfunction2014_dementia == 1 | 
        cogfunction2016_dementia == 1 | cogfunction2018_dementia == 1 ~ 1,
      is.na(cogfunction2012_dementia) & is.na(cogfunction2014_dementia) & 
        is.na(cogfunction2016_dementia) & is.na(cogfunction2018_dementia) ~ NA_real_,
      TRUE ~ 0
    )
  )

# Drop rows without any dementia records
df_dementia <- df[!is.na(df$event_status),]

# Calculate event dates and event time (使用r10iwmid作为baseline = 2010年)
df_dementia <- df_dementia %>%
  mutate(
    baseline_date = as.Date(r10iwmid, origin = "1960-01-01"),
    
    event_date = case_when(
      event_status == 0 & !is.na(r14iwmid) ~ as.Date(r14iwmid, origin = "1960-01-01"),
      event_status == 0 & !is.na(death_date) ~ as.Date(death_date, origin = "1960-01-01"),
      event_status == 0 & is.na(r14iwmid) ~ as.Date(median(r14iwmid, na.rm = TRUE), origin = "1960-01-01"),
      
      event_status == 1 ~ as.Date(
        coalesce(
          ifelse(cogfunction2010_dementia == 1, r10iwmid, NA),
          ifelse(cogfunction2012_dementia == 1, r11iwmid, NA),
          ifelse(cogfunction2014_dementia == 1, r12iwmid, NA),
          ifelse(cogfunction2016_dementia == 1, r13iwmid, NA),
          ifelse(cogfunction2018_dementia == 1, r14iwmid, NA)
        ), 
        origin = "1960-01-01"
      ),
      
      TRUE ~ as.Date(NA)
    )
  )

# Calculate event time in months from baseline (2010)
df_dementia <- df_dementia %>%
  mutate(
    event_time = as.numeric(difftime(event_date, baseline_date, units = "days")) / 30.44
  ) %>%
  dplyr::select(-baseline_date)

cat("Event status distribution:\n")
print(table(df_dementia$event_status, useNA = "ifany"))
cat("\nEvent time summary:\n")
print(summary(df_dementia$event_time))

####========== PREPARE DATASET FOR ALL ANALYSES ==========####

variables_impute <- c("age", "gender", "lb_status", "marital_status", "wealth_quartile", 
                      "drink_now", "smoking", "n_chronic_category", "us_born", "urbanicity", 
                      "Race", "stress", "health", "financial", "warmth", "height2010", "phy_act", "soc_activity2010")

# Prepare and impute main dataset
df_main_converted <- df_dementia[, variables_impute]
df_main_converted <- df_main_converted %>%
  mutate_if(names(.) %in% c("gender", "lb_status", "marital_status", "wealth_quartile", 
                            "drink_now", "smoking", "n_chronic_category", "us_born", 
                            "urbanicity", "Race", "stress", "health", "financial", 
                            "warmth", "phy_act", "soc_activity2010"), as.factor)

set.seed(1005)
mice_main <- mice(df_main_converted, method = "cart", m = 1, maxit = 5)
imputed_main <- complete(mice_main)
common_cols_main <- intersect(names(df_dementia), names(imputed_main))
df_dementia[common_cols_main] <- imputed_main[common_cols_main]

# Set reference levels for main dataset
df_dementia$educational_mobility_pr <- factor(df_dementia$educational_mobility_pr, 
                                              levels = c("Stably low","Downward mobility","Upward mobility", "Stably high"))
df_dementia$educational_mobility_pr <- relevel(df_dementia$educational_mobility_pr, ref = "Stably low")
df_dementia$Race <- factor(df_dementia$Race, levels = c("Non-Hispanic Black","Non-Hispanic White","Hispanic","Other"))
df_dementia$Race <- relevel(df_dementia$Race, ref = "Non-Hispanic White")
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "men")
df_dementia$wealth_quartile <- relevel(factor(df_dementia$wealth_quartile), ref = "1")
df_dementia$n_chronic_category <- relevel(factor(df_dementia$n_chronic_category), ref = "0 or 1 chronic")
df_dementia$drink_now <- relevel(factor(df_dementia$drink_now), ref = "0")
df_dementia$smoking <- relevel(factor(df_dementia$smoking), ref = "non smoker")
df_dementia$education_quan <- relevel(factor(df_dementia$education_quan), ref = "1")
df_dementia$us_born <- factor(df_dementia$us_born)
df_dementia$phy_act <- factor(df_dementia$phy_act)
df_dementia$soc_activity2010 <- factor(df_dementia$soc_activity2010)

#============
#MAIN ANALYSIS
#============

surv_obj_main <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_main <- coxph(surv_obj_main ~ educational_mobility_pr + age + gender + Race + 
                     height2010 + stress + health + 
                      financial + warmth + us_born, 
                    data = df_dementia)

cat("\nMain Cox Model Summary:\n")
print(summary(model_main))

main_results <- broom::tidy(model_main, conf.int = TRUE) %>%
  filter(grepl("educational_mobility_pr", term)) %>%
  mutate(
    hr = exp(estimate),
    hr_low = exp(conf.low),
    hr_high = exp(conf.high),
    analysis = "Main Analysis"
  ) %>%
  select(term, hr, hr_low, hr_high, p.value, analysis)

print(main_results)

surv_obj_gender <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_main <- coxph(surv_obj_main ~ educational_mobility_pr * gender + age + Race + 
                      height2010 + stress + health + 
                      financial + warmth + us_born, 
                    data = df_dementia)

cat("\nMain Cox Model Summary:\n")
print(summary(model_main))

####========== SA 1: 1.5 SD CUTOFF (Education-Stratified) ==========####
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

df_sa1 <- df_dementia %>%
  mutate(
    event_status_1.5sd = case_when(
      cogfunction2012_dementia == 1 | cogfunction2014_dementia == 1 | 
        cogfunction2016_dementia == 1 | cogfunction2018_dementia == 1 ~ 1,
      is.na(cogfunction2012_dementia) & is.na(cogfunction2014_dementia) & 
        is.na(cogfunction2016_dementia) & is.na(cogfunction2018_dementia) ~ NA_real_,
      TRUE ~ 0
    )
  ) %>%
  filter(!is.na(event_status_1.5sd))
print(table(df_sa1$event_status_1.5sd, useNA = "ifany"))

surv_obj_sa1 <- Surv(time = df_sa1$event_time, event = df_sa1$event_status_1.5sd)
model_sa1 <- coxph(surv_obj_sa1 ~ educational_mobility_pr + age + gender + Race + 
                     height2010 + stress + health + 
                     financial + warmth + us_born, 
                   data = df_sa1)

cat("\nModel Summary (SA1: 1.5 SD):\n")
print(summary(model_sa1))

sa1_results <- broom::tidy(model_sa1, conf.int = TRUE) %>%
  filter(grepl("educational_mobility_pr", term)) %>%
  mutate(
    hr = exp(estimate),
    hr_low = exp(conf.low),
    hr_high = exp(conf.high),
    analysis = "SA1: 1.5 SD"
  ) %>%
  select(term, hr, hr_low, hr_high, p.value, analysis)

print(sa1_results)

####========== SA 5: IPCW (Inverse Probability of Censoring Weighting) ==========####

cat("\n\n========== SA 5: IPCW ==========\n")

# Create censoring indicators for each wave (baseline is iwstat2010)
# iwstat coding: 1=interviewed, 5=deceased, other values (4,7,9)=lost to follow-up
df_ipcw <- df_dementia %>%
  mutate(
    # Wave 2012
    censored_wave2012 = ifelse(iwstat2012 == 5, 1, ifelse(iwstat2012 == 1, 0, NA)),
    lost_wave2012 = ifelse(iwstat2012 == 9 | iwstat2012 == 7 | iwstat2012 == 4, 1, 
                           ifelse(iwstat2012 == 1, 0, NA)),
    
    # Wave 2014
    censored_wave2014 = ifelse(iwstat2014 == 5, 1, ifelse(iwstat2014 == 1, 0, NA)),
    lost_wave2014 = ifelse(iwstat2014 == 9 | iwstat2014 == 7 | iwstat2014 == 4, 1, 
                           ifelse(iwstat2014 == 1, 0, NA)),
    
    # Wave 2016
    censored_wave2016 = ifelse(iwstat2016 == 5, 1, ifelse(iwstat2016 == 1, 0, NA)),
    lost_wave2016 = ifelse(iwstat2016 == 9 | iwstat2016 == 7 | iwstat2016 == 4, 1, 
                           ifelse(iwstat2016 == 1, 0, NA)),
    
    # Wave 2018
    censored_wave2018 = ifelse(iwstat2018 == 5, 1, ifelse(iwstat2018 == 1, 0, NA)),
    lost_wave2018 = ifelse(iwstat2018 == 9 | iwstat2018 == 7 | iwstat2018 == 4, 1, 
                           ifelse(iwstat2018 == 1, 0, NA)),
    
    # Wave 2020
    censored_wave2020 = ifelse(iwstat2020 == 5, 1, ifelse(iwstat2020 == 1, 0, NA)),
    lost_wave2020 = ifelse(iwstat2020 == 9 | iwstat2020 == 7 | iwstat2020 == 4, 1, 
                           ifelse(iwstat2020 == 1, 0, NA))
  )

# Model censoring and lost-to-follow-up for each wave
waves <- c(2012, 2014, 2016, 2018, 2020)

for (wave in waves) {
  censored_var <- paste0("censored_wave", wave)
  lost_var <- paste0("lost_wave", wave)
  prob_censored_var <- paste0("prob_censored_", wave)
  prob_lost_var <- paste0("prob_lost_", wave)
  prob_remain_var <- paste0("prob_remain_", wave)
  
  # Model censoring (death)
  model_censoring <- glm(as.formula(paste(censored_var, "~ age + gender + Race + 
                     height2010 + stress + health + 
                      financial + warmth")),
                         data = df_ipcw, family = binomial, na.action = na.exclude)
  
  # Model lost to follow-up
  model_lost <- glm(as.formula(paste(lost_var, "~ age + gender + Race + 
                     height2010 + stress + health + 
                      financial + warmth")),
                    data = df_ipcw, family = binomial, na.action = na.exclude)
  
  # Predict probabilities
  df_ipcw[[prob_censored_var]] <- predict(model_censoring, type = "response", newdata = df_ipcw)
  df_ipcw[[prob_lost_var]] <- predict(model_lost, type = "response", newdata = df_ipcw)
  
  # Probability of remaining in study
  df_ipcw[[prob_remain_var]] <- (1 - df_ipcw[[prob_censored_var]]) * (1 - df_ipcw[[prob_lost_var]])
}

# Calculate cumulative IPCW weights
df_ipcw <- df_ipcw %>%
  mutate(
    ipcw_2012 = 1 / prob_remain_2012,
    ipcw_2014 = 1 / (prob_remain_2012 * prob_remain_2014),
    ipcw_2016 = 1 / (prob_remain_2012 * prob_remain_2014 * prob_remain_2016),
    ipcw_2018 = 1 / (prob_remain_2012 * prob_remain_2014 * prob_remain_2016 * prob_remain_2018),
    ipcw_2020 = 1 / (prob_remain_2012 * prob_remain_2014 * prob_remain_2016 * prob_remain_2018 * prob_remain_2020)
  )

# Use the final (2020) cumulative weight and trim extreme weights
q01_weight <- quantile(df_ipcw$ipcw_2020, probs = 0.01, na.rm = TRUE)
q99_weight <- quantile(df_ipcw$ipcw_2020, probs = 0.99, na.rm = TRUE)
df_ipcw$ipcw_trimmed <- pmin(pmax(df_ipcw$ipcw_2020, q01_weight), q99_weight)

cat("IPCW Summary:\n")
cat("Mean weight:", mean(df_ipcw$ipcw_trimmed, na.rm = TRUE), "\n")
cat("Range:", range(df_ipcw$ipcw_trimmed, na.rm = TRUE), "\n")
cat("SD:", sd(df_ipcw$ipcw_trimmed, na.rm = TRUE), "\n")

# Cox model with IPCW
surv_obj_ipcw <- Surv(time = df_ipcw$event_time, event = df_ipcw$event_status)
model_ipcw <- coxph(surv_obj_ipcw ~ educational_mobility_pr + age + gender + Race + 
                      height2010 + stress + health + 
                      financial + warmth + us_born, 
                    data = df_ipcw, weights = ipcw_trimmed)

cat("\nModel Summary (SA5: IPCW):\n")
print(summary(model_ipcw))

ipcw_results <- broom::tidy(model_ipcw, conf.int = TRUE) %>%
  filter(grepl("educational_mobility_pr", term)) %>%
  mutate(
    hr = exp(estimate),
    hr_low = exp(conf.low),
    hr_high = exp(conf.high),
    analysis = "SA5: IPCW"
  ) %>%
  select(term, hr, hr_low, hr_high, p.value, analysis)

print(ipcw_results)

####========== SA 6: SAMPLING WEIGHTS ==========####

cat("\n\n========== SA 6: SAMPLING WEIGHTS ==========\n")

df_weights <- df_dementia %>%
  filter(!is.na(s_weight) & s_weight > 0)

# Normalize weights
df_weights$s_weight_norm <- df_weights$s_weight / sum(df_weights$s_weight, na.rm = TRUE) * nrow(df_weights)

cat("Sampling weights summary:\n")
cat("Mean weight:", mean(df_weights$s_weight_norm, na.rm = TRUE), "\n")
cat("Range:", range(df_weights$s_weight_norm, na.rm = TRUE), "\n")
cat("Min weight:", min(df_weights$s_weight_norm, na.rm = TRUE), "\n")

# Check for any remaining invalid weights
invalid_weights <- sum(df_weights$s_weight_norm <= 0, na.rm = TRUE)
cat("Number of invalid (<=0) weights:", invalid_weights, "\n")

# Remove any remaining invalid weights
df_weights <- df_weights %>% filter(s_weight_norm > 0)

# Cox model with sampling weights
surv_obj_weights <- Surv(time = df_weights$event_time, event = df_weights$event_status)
model_weights <- coxph(surv_obj_weights ~ educational_mobility_pr + age + gender + Race + 
                         height2010 + stress + health + 
                         financial + warmth+ us_born, 
                       data = df_weights, weights = s_weight_norm)

cat("\nModel Summary (SA6: Sampling Weights):\n")
print(summary(model_weights))

weights_results <- broom::tidy(model_weights, conf.int = TRUE) %>%
  filter(grepl("educational_mobility_pr", term)) %>%
  mutate(
    hr = exp(estimate),
    hr_low = exp(conf.low),
    hr_high = exp(conf.high),
    analysis = "SA6: Sampling Weights"
  ) %>%
  select(term, hr, hr_low, hr_high, p.value, analysis)

print(weights_results)

####========== SA 7: COMPLETE CASE ANALYSIS ==========####

cat("\n\n========== SA 7: COMPLETE CASE ANALYSIS ==========\n")

# Create complete case dataset BEFORE imputation
# Convert to data frame to remove any grouping
df_no_impute <- as.data.frame(df_dementia)

# Select variables needed
cols_needed <- c("educational_mobility_pr", "event_status", "event_time", variables_impute)
df_no_impute <- df_no_impute[, cols_needed]

# Keep only complete cases
complete_idx <- complete.cases(df_no_impute)
df_no_impute <- df_no_impute[complete_idx, ]

cat("Sample size - Complete cases:", nrow(df_no_impute), "\n")
cat("Sample size - With dementia records:", nrow(df_dementia), "\n")

# Set reference levels
df_no_impute$educational_mobility_pr <- relevel(factor(df_no_impute$educational_mobility_pr), ref = "Stably low")
df_no_impute$Race <- relevel(factor(df_no_impute$Race), ref = "Non-Hispanic White")
df_no_impute$gender <- relevel(factor(df_no_impute$gender), ref = "men")
df_no_impute$wealth_quartile <- relevel(factor(df_no_impute$wealth_quartile), ref = "1")
df_no_impute$n_chronic_category <- relevel(factor(df_no_impute$n_chronic_category), ref = "0 or 1 chronic")
df_no_impute$drink_now <- relevel(factor(df_no_impute$drink_now), ref = "0")
df_no_impute$smoking <- relevel(factor(df_no_impute$smoking), ref = "non smoker")
df_no_impute$us_born <- factor(df_no_impute$us_born)
df_no_impute$phy_act <- factor(df_no_impute$phy_act)
df_no_impute$soc_activity2010 <- factor(df_no_impute$soc_activity2010)

# Cox regression on complete cases
surv_obj_complete <- Surv(time = df_no_impute$event_time, event = df_no_impute$event_status)
model_complete <- coxph(surv_obj_complete ~ educational_mobility_pr + age + gender + Race + 
                          height2010 + stress + health + 
                          financial + warmth, 
                        data = df_no_impute)

cat("\nModel Summary (SA7: Complete Case):\n")
print(summary(model_complete))

complete_results <- broom::tidy(model_complete, conf.int = TRUE) %>%
  filter(grepl("educational_mobility_pr", term)) %>%
  mutate(
    hr = exp(estimate),
    hr_low = exp(conf.low),
    hr_high = exp(conf.high),
    analysis = "SA7: Complete Case"
  ) %>%
  select(term, hr, hr_low, hr_high, p.value, analysis)

print(complete_results)
