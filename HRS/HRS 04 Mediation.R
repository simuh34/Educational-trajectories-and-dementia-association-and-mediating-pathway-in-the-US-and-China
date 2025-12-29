library(dplyr)
library(mice)
library(mediation)
library(tableone)

df <- read.csv("D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\imputed_hrs_long_lowhigh.csv")

df <- df %>%
  mutate(
    has_prior_dementia = ifelse(cogfunction2010_dementia == 1, 1, 0)
  )

df_clean <- df %>% filter(has_prior_dementia == 0)

outcome_cols <- c("cogfunction2012_dementia", "cogfunction2014_dementia", 
                  "cogfunction2016_dementia", "cogfunction2018_dementia")

df_clean <- df_clean %>%
  mutate(
    event_status = case_when(
      if_any(all_of(outcome_cols), ~ .x == 1) ~ 1,
      if_all(all_of(outcome_cols), is.na) ~ NA_real_,
      TRUE ~ 0
    )
  )

df_dementia <- df_clean[!is.na(df_clean$event_status),]

# 创建lb_status变量
df_dementia$lb_status <- ifelse(!is.na(df_dementia$lb_status2010) & df_dementia$lb_status2010 == 1, 
                                "fulltime_employed", 
                                ifelse(!is.na(df_dementia$lb_status2010), "other", NA))

df_dementia$educational_mobility_pr <- relevel(factor(df_dementia$educational_mobility_pr, 
                                                      levels = c("Stably low","Downward mobility", "Upward mobility", "Stably high")), ref = "Stably low")
df_dementia$Race <- relevel(factor(df_dementia$Race, levels = c("Non-Hispanic Black","Non-Hispanic White","Hispanic","Other")), ref = "Non-Hispanic White")
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "men")
df_dementia$wealth_quartile <- relevel(factor(df_dementia$wealth_quartile), ref = "1")
df_dementia$n_chronic_cat <- relevel(factor(df_dementia$n_chronic_cat), ref = "0 or 1 chronic")
df_dementia$drink_now <- relevel(factor(df_dementia$drink_now), ref = "0")
df_dementia$smoking <- relevel(factor(df_dementia$smoking), ref = "non smoker")
df_dementia$phy_act <- factor(df_dementia$phy_act)
df_dementia$soc_activity2008 <- factor(df_dementia$soc_activity2008)
df_dementia$lb_status <- relevel(factor(df_dementia$lb_status), ref = "fulltime_employed")

vars_to_impute <- c(
  "age", "gender", "Race", "lb_status", "marital_status", 
  "wealth_quartile", "drink_now", "smoking", "n_chronic_cat", 
  "urbanicity", "height2008", "stress", "health", "financial", 
  "warmth", "phy_act", "soc_activity2008", "cesd2010"
)

df_converted <- df_dementia[, vars_to_impute]
df_converted <- df_converted %>% mutate_if(is.character, as.factor)

set.seed(1005)
mice_mod <- mice(df_converted, method = "cart", m = 1, maxit = 5, printFlag = FALSE)
imputed_data <- complete(mice_mod)
common_cols <- intersect(names(df_dementia), names(imputed_data))
df_dementia[common_cols] <- imputed_data[common_cols]

df_dementia$married <- ifelse(df_dementia$marital_status %in% c("married/partnered"), 1, 0)
df_dementia$drinking_now <- ifelse(df_dementia$drink_now == 1, 1, 0)
df_dementia$smoking_now <- ifelse(df_dementia$smoking == "current smoker", 1, 0)
df_dementia$soc_act <- ifelse(df_dementia$soc_activity2008 == 1, 1, 0)
df_dementia$high_wealth <- ifelse(df_dementia$wealth_quartile %in% c("2"), 1, 0)
df_dementia$multiple_chronic <- ifelse(df_dementia$n_chronic_cat == "more than 2 chronic", 1, 0)
df_dementia$depression <- ifelse(df_dementia$cesd2010 >= 3, 1, 0)
df_dementia$lb_status <- ifelse(df_dementia$lb_status %in% c("fulltime_employed"), 1, 0)
df_dementia$phy_act <- ifelse(df_dementia$phy_act == 1, 1, 0)

df_dementia$downward_vs_low <- ifelse(df_dementia$educational_mobility_pr == "Downward mobility", 1, 
                                      ifelse(df_dementia$educational_mobility_pr == "Stably low", 0, NA))
df_dementia$upward_vs_low <- ifelse(df_dementia$educational_mobility_pr == "Upward mobility", 1, 
                                    ifelse(df_dementia$educational_mobility_pr == "Stably low", 0, NA))
df_dementia$high_vs_low <- ifelse(df_dementia$educational_mobility_pr == "Stably high", 1, 
                                  ifelse(df_dementia$educational_mobility_pr == "Stably low", 0, NA))

df_downward <- df_dementia[!is.na(df_dementia$downward_vs_low), ]
df_upward <- df_dementia[!is.na(df_dementia$upward_vs_low), ]
df_high <- df_dementia[!is.na(df_dementia$high_vs_low), ]

covariates <- c("age", "gender", "Race", "urbanicity", "height2008", 
                "stress", "health", "financial", "warmth", "us_born")

analyze_mediation <- function(data, exposure, mediator, cvs, n_boot = 50) {
  
  data_sub <- data[, c("event_status", exposure, mediator, cvs)]
  data_sub <- na.omit(data_sub)
  
  f_med <- as.formula(paste(mediator, "~", exposure, "+", paste(cvs, collapse = "+")))
  f_out <- as.formula(paste("event_status ~", exposure, "+", mediator, "+", paste(cvs, collapse = "+")))
  
  mod_m <- glm(f_med, data = data_sub, family = binomial("logit"))
  mod_y <- glm(f_out, data = data_sub, family = binomial("logit"))
  
  mod_m$call$formula <- f_med
  mod_y$call$formula <- f_out
  
  set.seed(1005)
  res <- mediate(mod_m, mod_y, treat = exposure, mediator = mediator, boot = TRUE, sims = n_boot)
  sum_res <- summary(res)
  
  return(c(
    ACME = sum_res$d.avg,
    ACME_Low = sum_res$d.avg.ci[1],
    ACME_High = sum_res$d.avg.ci[2],
    ACME_P = sum_res$d.avg.p,
    
    ADE = sum_res$z.avg,
    ADE_P = sum_res$z.avg.p,
    
    Total = sum_res$tau.coef,
    Total_Low = sum_res$tau.ci[1],
    Total_High = sum_res$tau.ci[2],
    Total_P = sum_res$tau.p,
    
    Prop_Med = sum_res$n.avg,
    Prop_Med_Low = sum_res$n.avg.ci[1],
    Prop_Med_High = sum_res$n.avg.ci[2],
    Prop_Med_P = sum_res$n.avg.p
  ))
}

mediators_list <- c("married", "drinking_now", "smoking_now", "soc_act", 
                    "high_wealth", "multiple_chronic", "depression", 
                    "phy_act", "lb_status")

comparisons <- list(
  list(df = df_downward, exp = "downward_vs_low", label = "Downward vs Low"),
  list(df = df_upward, exp = "upward_vs_low", label = "Upward vs Low"),
  list(df = df_high, exp = "high_vs_low", label = "High vs Low")
)

results_df <- data.frame()

for (comp in comparisons) {
  print(paste("Running:", comp$label))
  
  for (med in mediators_list) {
    print(paste("  - Analyzing mediator:", med))
    
    out <- analyze_mediation(comp$df, comp$exp, med, covariates, n_boot = 1000)
    
    row_data <- data.frame(
      Comparison = comp$label,
      Mediator = med,
      N = nrow(comp$df),
      
      ACME = sprintf("%.4f", out["ACME"]),
      ACME_CI = paste0(sprintf("%.4f", out["ACME_Low.2.5%"]), ", ", sprintf("%.4f", out["ACME_High.97.5%"])),
      ACME_P = sprintf("%.3f", out["ACME_P"]),
      
      ADE = sprintf("%.4f", out["ADE"]),
      ADE_P = sprintf("%.3f", out["ADE_P"]),
      
      Total = sprintf("%.4f", out["Total"]),
      Total_CI = paste0(sprintf("%.4f", out["Total_Low.2.5%"]), ", ", sprintf("%.4f", out["Total_High.97.5%"])),
      Total_P = sprintf("%.3f", out["Total_P"]),
      
      Prop_Med = sprintf("%.2f%%", out["Prop_Med"] * 100),
      Prop_Med_CI = paste0(sprintf("%.2f%%", out["Prop_Med_Low.2.5%"] * 100), ", ", 
                           sprintf("%.2f%%", out["Prop_Med_High.97.5%"] * 100)),
      Prop_Med_P = sprintf("%.3f", out["Prop_Med_P"])
    )
    
    results_df <- rbind(results_df, row_data)
  }
}

print(results_df)


