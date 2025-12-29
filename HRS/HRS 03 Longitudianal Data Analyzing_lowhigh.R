library(tableone)
library(kableExtra)
library(CMAverse)
library(survival)
library(dplyr)
library(sjPlot)
library(broom)
library(ggplot2)


df <- read.csv("D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\imputed_hrs_long_lowhigh_2010baseline.csv")


##check prevalence

#events_status
df <- df %>%
  mutate(
    event_status = case_when(
      if_any(starts_with("cogfunction") & ends_with("_dementia"), ~ .x == 1) ~ 1,
      if_all(starts_with("cogfunction") & ends_with("_dementia"), is.na) ~ NA_real_,
      TRUE ~ 0
    )
  )

#drop rows do not have any dementia records (n = 16075)
df_dementia <- df[!is.na(df$event_status),]

# Baseline 2010 = wave 10 = r10iwmid
# 2010 = wave 10 = r10iwmid
# 2012 = wave 11 = r11iwmid
# 2014 = wave 12 = r12iwmid
# 2016 = wave 13 = r13iwmid
# 2018 = wave 14 = r14iwmid

df_dementia <- df_dementia %>%
  mutate(
    min_r14iwmid = min(r14iwmid, na.rm = TRUE),
    median_r14iwmid = median(r14iwmid, na.rm = TRUE),
    
    event_date = case_when(
      event_status == 0 & !is.na(r14iwmid) ~ as.Date(r14iwmid, origin = "1960-01-01"),
      event_status == 0 & !is.na(death_date) & death_date < min_r14iwmid ~ as.Date(death_date, origin = "1960-01-01"),
      event_status == 0 & is.na(r14iwmid) ~ as.Date(median_r14iwmid, origin = "1960-01-01"),
      
      event_status == 1 ~ as.Date(
        coalesce(
          ifelse(cogfunction2010_dementia == 1, r10iwmid, NA),  # 2010 = wave 10
          ifelse(cogfunction2012_dementia == 1, r11iwmid, NA),  # 2012 = wave 11
          ifelse(cogfunction2014_dementia == 1, r12iwmid, NA),  # 2014 = wave 12
          ifelse(cogfunction2016_dementia == 1, r13iwmid, NA),  # 2016 = wave 13
          ifelse(cogfunction2018_dementia == 1, r14iwmid, NA)   # 2018 = wave 14
        ), 
        origin = "1960-01-01"
      ),
      
      TRUE ~ as.Date(NA)
    )
  ) %>%
  dplyr::select(-min_r14iwmid, -median_r14iwmid)

# Calculate event time (months from baseline r10iwmid to event occurrence)
df_dementia <- df_dementia %>%
  mutate(
    event_time = as.numeric(difftime(event_date, 
                                     as.Date(r10iwmid, origin = "1960-01-01"), 
                                     units = "days")) / 30.44
  )

# Check event status distribution
table(df_dementia$event_status, useNA = "ifany")

# Check descriptive statistics of event time
summary(df_dementia$event_time)

# Check number of dementia cases by wave (from 2010 baseline onwards)
dementia_by_wave <- df_dementia %>%
  summarise(
    dementia_2010 = sum(cogfunction2010_dementia == 1, na.rm = TRUE),
    dementia_2012 = sum(cogfunction2012_dementia == 1, na.rm = TRUE),
    dementia_2014 = sum(cogfunction2014_dementia == 1, na.rm = TRUE),
    dementia_2016 = sum(cogfunction2016_dementia == 1, na.rm = TRUE),
    dementia_2018 = sum(cogfunction2018_dementia == 1, na.rm = TRUE)
  )
print(dementia_by_wave)


#reference group
df_dementia$educational_mobility_pr <- factor(df_dementia$educational_mobility_pr, 
                                              levels = c("Stably low","Downward mobility","Upward mobility", "Stably high"))
df_dementia$educational_mobility_pr <- relevel(df_dementia$educational_mobility_pr, ref = "Stably low")
df_dementia$Race <- factor(df_dementia$Race,levels = c("Non-Hispanic Black","Non-Hispanic White","Hispanic","Other"))
df_dementia$Race <- relevel(df_dementia$Race,ref = "Non-Hispanic White" )
df_dementia$gender <- relevel(factor(df_dementia$gender), ref = "men")
df_dementia$wealth_quartile <- relevel(factor(df_dementia$wealth_quartile), ref = "1")
df_dementia$n_chronic_category <- relevel(factor(df_dementia$n_chronic_category), ref = "0 or 1 chronic")
df_dementia$drink_now <- relevel(factor(df_dementia$drink_now), ref = "0")
df_dementia$smoking <- relevel(factor(df_dementia$smoking), ref = "non smoker")
df_dementia$education_quan <- relevel(factor(df_dementia$education_quan), ref = "1")
df_dementia$education_p_quan <- relevel(factor(df_dementia$education_p_quan), ref = "1")
df_dementia$us_born <- factor(df_dementia$us_born)
df_dementia$phy_act <- factor(df_dementia$phy_act)
df_dementia$soc_activity2010 <- factor(df_dementia$soc_activity2010)
df_dementia$depression <- factor(df_dementia$depression)

####model - Cox proportional hazard model (baseline 2010)
library(survival)
library(survminer)

surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_dementia <- coxph(surv_obj ~ educational_mobility_pr + age + gender + Race + 
                         height2010 + stress + health + financial + warmth, data = df_dementia)
summary(model_dementia)
exp(confint(model_dementia))

#forest plot
model_results <- broom::tidy(model_dementia, conf.int = TRUE)

target_var <- model_results %>%
  filter(grepl("educational_mobility_pr", term)) %>%  
  mutate(
    hr = exp(estimate),  
    hr_low = exp(conf.low),  
    hr_high = exp(conf.high)  
  ) %>%
  select(term, hr, hr_low, hr_high, p.value)  
print(target_var)

ref_row <- data.frame(
  term = "educational_mobility_prStably low (ref)", 
  hr = 1,                   
  hr_low = 1,                
  hr_high = 1,               
  p.value = NA               
)
target_var <- bind_rows(ref_row, target_var)

target_var <- target_var %>%
  mutate(term = gsub("educational_mobility_pr", "", term))  

target_var <- target_var %>%
  mutate(term = case_when(
    term == "Stably low (ref)" ~ "Stably low (ref)",  
    term == "Downward mobility" ~ "Downwardly mobile", 
    term == "Upward mobility" ~ "Upwardly mobile",      
    term == "Stably high" ~ "Stably high",
    TRUE ~ term  
  )) %>%
  mutate(term = factor(term, levels = rev( c(
    "Stably low (ref)",          
    "Downwardly mobile",    
    "Upwardly mobile",      
    "Stably high"           
  ))))

forest_plot <- ggplot(target_var, aes(x = hr, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +  
  geom_errorbarh(aes(xmin = hr_low, xmax = hr_high), 
                 height = 0.2, color = "#007F6F", size = 1.5) +
  geom_point(aes(size = 2), color = "#007F6F") +  
  scale_x_continuous(
    limits = c(0.1, 2),
    breaks = c(0.5, 1, 1.5, 2)) +
  coord_trans(x = "log10") +
  labs(
    x = "Hazard Ratio (95% CI)",
    y = NULL,
    title = "HRS (Baseline 2010, Follow-up 2012-2018)"
  ) +
  theme_bw() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.text = element_text(color = "black", size = 12),  
    axis.title = element_text(color = "black", size = 13), 
    panel.background = element_rect(fill = "white"),  
    panel.grid = element_blank(),  
    legend.position = "none" 
  ) 

print(forest_plot)

ggsave(
  "D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\cox_hrs_long_lowhigh_2010baseline.png",
  plot = forest_plot,
  width = 10, height = 6, dpi = 300
)

## interaction analysis by gender
surv_obj <- Surv(time = df_dementia$event_time, event = df_dementia$event_status)
model_dementia_interaction <- coxph(surv_obj ~ educational_mobility_pr * gender + age + Race + 
                                      lb_status + marital_status + wealth_quartile + drink_now + smoking + n_chronic_category + urbanicity + height2010 + stress + health + financial + warmth + phy_act + soc_activity2010, data = df_dementia)
summary(model_dementia_interaction)
exp(confint(model_dementia_interaction))


