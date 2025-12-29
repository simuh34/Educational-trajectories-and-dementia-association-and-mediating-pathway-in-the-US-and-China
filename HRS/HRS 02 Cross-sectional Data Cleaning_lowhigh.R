library(dplyr)

df <- read.csv("D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\hrs.csv")

####preparing the df####
##define sample size; n = baseline from 2010
sum(df$inw2010)
df <- df[df$inw2010 == 1,]

##exclude age >= 50; baseline 2010
df <- df[which(df$age2010 >= 50), ]

##delete na in education; baseline 2010
df <- subset(df, !(is.na(years_edu)))

## number of missing of parental education
missing_mother <- sum(is.na(df$years_edu_m))
prop_mother <- missing_mother / nrow(df)

missing_father <- sum(is.na(df$years_edu_f))
prop_father <- missing_father / nrow(df)

mean(df$years_edu_f > df$years_edu_m, na.rm = TRUE)
sum(df$years_edu_f > df$years_edu_m, na.rm = TRUE)

mean(df$years_edu_m > df$years_edu_f, na.rm = TRUE)
sum(df$years_edu_m > df$years_edu_f, na.rm = TRUE)
missing_mother
prop_mother
missing_father
prop_father

#### construct the exposure: education mobility ####
## highest education level of parent
## delete rows which education of parents are both NA
## raw education
df <- subset(df, !(is.na(years_edu_f) & is.na(years_edu_m)))

df$highest_education_parent<- ifelse(
  is.na(df$years_edu_f), df$years_edu_m,
  ifelse(
    is.na(df$years_edu_m), df$years_edu_f,
    ifelse(df$years_edu_f> df$years_edu_m, df$years_edu_f, df$years_edu_m)
  )
)

##exclude NA for outcomes dementia; baseline 2010
selected_columns <- c("cogfunction2010")
df <- df %>% filter(!is.na(cogfunction2010))

# group the respondent by birth year
df$decade <- ifelse(df$birth_year >= 1913 & df$birth_year <= 1929, 1920,
                    ifelse(df$birth_year>= 1930 & df$birth_year <= 1939, 1930,
                           ifelse(df$birth_year >= 1940 & df$birth_year <= 1949, 1940,
                                  ifelse(df$birth_year >= 1950 & df$birth_year <= 1959, 1950,
                                         1960))))
table(df$decade)

###1. percentile rank of education level
## percentile ranking of respondents' education level
df <- df %>%
  group_by(decade) %>%
  mutate(percentile_rank = rank(years_edu,ties.method = c("max"))/length(years_edu))

## percentile ranking of respondent' parent education level
df <- df %>%
  group_by(decade) %>%
  mutate(percentile_rank_parent =  rank(highest_education_parent,ties.method = c("max"))/length(highest_education_parent))

summary(df$percentile_rank)
summary(df$percentile_rank_parent)

lm_model <- lm(df$percentile_rank ~ df$percentile_rank_parent)
df$educational_mobility_residual <- resid(lm_model)
summary(df$educational_mobility_residual)

###3. categorized by percentile ranking of education level
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

table(df$education_quan)
table(df$education_p_quan)
range(df$years_edu[df$education_quan == 1])
freq_table <- table(df$education_quan)
prop_table <- prop.table(freq_table) * 100  
table(df$education_quan)
print(prop_table)

range(df$highest_education_parent[df$education_p_quan == 1])
freq_table <- table(df$education_p_quan)
prop_table <- prop.table(freq_table) * 100  
table(df$education_p_quan)
print(prop_table)

df$educational_mobility_pr <- NA
df$educational_mobility_pr[df$education_p_quan == 1 & df$education_quan == 1] <- 1  # Stably low
df$educational_mobility_pr[df$education_p_quan == 2 & df$education_quan == 2] <- 2  # Stably high
df$educational_mobility_pr[as.numeric(df$education_p_quan) > as.numeric(df$education_quan)] <- 3  # Downward mobility
df$educational_mobility_pr[as.numeric(df$education_p_quan) < as.numeric(df$education_quan)] <- 4  # Upward mobility

df$educational_mobility_pr <- factor(df$educational_mobility_pr,
                                     levels = c(1, 3, 4, 2),
                                     labels = c("Stably low", "Downward mobility", "Upward mobility", "Stably high"))
df$educational_mobility_pr <- relevel(df$educational_mobility_pr, ref = "Stably low")
table(df$educational_mobility_pr)

####construct the outcome: dementia####
selected_columns <- c("cogfunction2010")

for (col in selected_columns) {
  new_col_name <- paste0(col, "_dementia")
  df[[new_col_name]] <- ifelse(is.na(df[[col]]), NA, ifelse(df[[col]] == 3, 1, 0))
}

selected_columns <- c("cogfunction2010_dementia")
table(df[selected_columns])

####covariates####
#==========================================
#check missingness
#==========================================
library(tidyverse)

missing_percent <- df %>%
  ungroup() %>%          
  select(-decade) %>%    
  summarise_all(~ round(mean(is.na(.), na.rm=TRUE)*100, 2)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(missing_prop = V1) %>%
  filter(missing_prop >= 0) %>%
  arrange(desc(missing_prop))  
print(as.data.frame(missing_percent), row.names = FALSE)

# cross sectional 2010
#recode variabels

#age - baseline 2010
df$age <- df$age2010

#gender: 1 is men; 2 is women
table(df$gender, exclude = NULL)
df$gender <- ifelse(df$gender==1, "men", "women")
df$gender <- factor(df$gender)

#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
df$marital_status<- ifelse(df$marital_status2010==1|df$marital_status2010==3, "married/partnered", "other")
table(df$marital_status, exclude = NULL)

#race and hispanic
#Generate a combined race: 0 non-hisp white, 1 non-hisp black, 2 hispanic, 3 other 
df$Race <- ifelse(df$race == 1 & 
                    df$hispanic == 0, 0,
                  ifelse(df$race == 2 & 
                           df$hispanic == 0, 1,
                         ifelse(df$hispanic == 1, 2, 3)))

df$Race <- ifelse(df$Race == 0, "Non-Hispanic White" , 
                  ifelse(df$Race == 1, "Non-Hispanic Black",
                         ifelse(df$Race == 2, "Hispanic","Other")))
table(df$Race)

#urbanicity - baseline 2010
df <- df %>%
  mutate(urbanicity = case_when(
    urbanicity2010 == 0 ~ "urban",
    urbanicity2010 == 1 ~ "rural",
    TRUE ~ NA_character_
  ))
table(df$urbanicity)
df$urbanicity <- factor(df$urbanicity)

#labour force status - baseline 2010
table(df$lb_status2010)
df$lb_status <- ifelse(!is.na(df$lb_status2010) & df$lb_status2010 == 1, "fulltime_employed", 
                       ifelse(!is.na(df$lb_status2010), "other", NA))

#smoking - baseline 2010
table(df$smoke_ever2010, exclude = NULL)
table(df$smoke_now2010, exclude = NULL)
table(df$smoke_ever2010, df$smoke_now2010)
df$smoking <-ifelse(df$smoke_now2010 == 1, "current smoker",
                    ifelse(df$smoke_now2010 == 0 &
                             df$smoke_ever2010 == 1, "past smoker", "non smoker"))
df$smoking <- factor(df$smoking, levels = c("non smoker", "past smoker", "current smoker"))
table(df$smoking)

#if drinking - baseline 2010
df$drink_now <- ifelse(df$drink_frq2010 == 1, 1, 0)
table(df$drink_now, exclude = NULL)

#number of chronic diseases - baseline 2010
table(df$n_chronic2010)
df <- df %>%
  mutate(n_chronic_category = case_when(
    n_chronic2010 == 0 | n_chronic2010 == 1 ~ "0 or 1 chronic",
    n_chronic2010 >= 2 & n_chronic2010 <= 6 ~ "more than 2 chronic",
    TRUE ~ NA_character_
  ))

table(df$n_chronic_category)

#physical activity - baseline 2010
table(df$vigo_act2010, exclude = NULL)
table(df$mode_act2010, exclude = NULL)
freq_map <- c("1" = 7,  # Every day = 7 days per week
              "2" = 2,  # >1 per week = >1 times per week, here considered as 2
              "3" = 1,  # 1 per week = 1 time per week
              "4" = 0.25, # l-3 per mon = 1-3 times per month, roughly 0.25 times per week
              "5" = 0)   # Never = 0 times per week
df$freq_moderate <- freq_map[as.character(df$mode_act2010)]
df$freq_vigorous <- freq_map[as.character(df$vigo_act2010)]
df$total_freq <- ifelse(is.na(df$freq_moderate) | is.na(df$freq_vigorous), NA, df$freq_moderate + df$freq_vigorous)
df <- df %>%
  mutate(phy_act = ifelse(is.na(total_freq), NA, ifelse(total_freq > 2, 1, 0)))
table(df$phy_act, useNA = "ifany")

#wealth - baseline 2010
df$people_living_with2010 <- as.numeric(as.character(df$people_living_with2010))
df$equivalized_wealth2010 <- ifelse(is.na(df$people_living_with2010) | is.na(df$household_wealth2010), NA, df$household_wealth2010 / sqrt(df$people_living_with2010))

df$equivalized_wealth <- ifelse(is.na(df$equivalized_wealth2010), NA, round(df$equivalized_wealth2010 / 1000, 2))

summary(df$equivalized_wealth)
quantile_breaks <- quantile(df$equivalized_wealth, probs = c(0, 0.5, 1), na.rm = TRUE)
df$wealth_quartile <- ifelse(is.na(df$equivalized_wealth),
                             NA,
                             cut(df$equivalized_wealth,
                                 breaks = quantile_breaks,
                                 include.lowest = TRUE,
                                 labels = c("1", "2")))

table(df$wealth_quartile, useNA = "ifany")

#nativity
df$us_born <- ifelse(df$us_born == 11, 0, 1)

#depression
df$depression <- factor(ifelse(df$cesd2010 >= 3, 1, 0))

####childhood####
library(haven)
childhood <- read_dta('D:\\R project\\Educational mobility and depression\\HRS\\hrs_childhood\\AGGCHLDFH2016A_R.dta')
library(dplyr)
library(stringr)
childhood <- childhood %>%
  mutate(hhidpn = str_c(HHID, PN, sep = ""))

#childhood stress/financial/warmth
childhood <- childhood %>%
  mutate(
    # 1. Financial stress indicator
    financial = case_when(
      FAMFIN == 5 ~ "YES",       # Family considered poor
      MOVFIN == 1 ~ "YES",       # Moved due to financial difficulties
      FMFINH == 1 ~ "YES",       # Family received financial help
      FAMFIN %in% c(1, 3) & MOVFIN == 5 & FMFINH == 5 ~ "NO", # Family is well - off or average, no move due to finance, no financial help
      TRUE ~ NA_character_       # Otherwise, set to NA
    ),
    
    # 2. General stress indicator
    stress = case_when(
      TRPOLICE == 1 ~ "YES",     # Police trouble before 18
      DRKDRUG == 1 ~ "YES",     # Family problems from parental substance abuse
      PHYABUSE == 1 ~ "YES",    # Physical abuse by parents
      SCHLOVER == 1 ~ "YES",    # Repeated a school year before 18
      TRPOLICE == 5 & DRKDRUG == 5 & PHYABUSE == 5 & 
        SCHLOVER == 5 ~ "NO", # No police trouble, no family drug problems, no physical abuse, no school repetition
      TRUE ~ NA_character_
    ),
    
    # 3. Family warmth indicator
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

table(childhood$financial)
table(childhood$stress)
table(childhood$warmth)

#chilhood health
childhood <- childhood %>%
  mutate(health = case_when(
    RTHLTHCH %in% c(1, 2, 3) ~ "healthy",
    RTHLTHCH %in% c(4, 5) ~ "less_healthy", 
    TRUE ~ NA_character_
  ))
childhood$hhidpn <- as.numeric(childhood$hhidpn)
childhood <- dplyr::select(childhood, c("hhidpn","health","stress","financial","warmth"))
df <- left_join(df,childhood, by = c("hhidpn"))

##################################################
#missingness check
variables <- c("age","gender", "lb_status","marital_status","wealth_quartile","drink_now","smoking","n_chronic_category","urbanicity","Race","stress","health","financial","warmth","height2010","depression")

missing_summary <- sapply(df[variables], function(x) {
  percent_missing <- mean(is.na(x)) * 100
  return(round(percent_missing, 2))
})

missing_df <- data.frame(
  Variable = names(missing_summary),
  Missing_Percent = missing_summary
) %>%
  arrange(desc(Missing_Percent))

print(missing_df)

#covariates imputation
df_converted <- df[, c("age","gender", "lb_status","marital_status","wealth_quartile","drink_now","smoking","n_chronic_category","us_born","urbanicity","Race","stress","health","financial","warmth","height2010","phy_act", "soc_activity2010","depression")]
classes <- sapply(df_converted, class)
labelled_vars <- names(classes[classes == "labelled"])
df_converted <- df_converted %>%
  mutate_if(names(.) %in% c("gender", "lb_status","marital_status","wealth_quartile","drink_now","smoking","n_chronic_category","us_born","urbanicity","Race","stress","health","financial","warmth","phy_act", "soc_activity2010","depression"), as.factor)
set.seed(1005)
library(mice)
mice_mod <- mice(df_converted, method = "cart", m =1, maxit = 5)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df), names(imputed_data))
df[common_cols] <- imputed_data[common_cols]

write.csv(df,"D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\imputed_hrs_sectional_lowhigh_2010baseline.csv")
