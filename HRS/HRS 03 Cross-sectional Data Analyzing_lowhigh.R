library(tableone)
library(kableExtra)
library(CMAverse)
library(survival)
library(dplyr)
library(sjPlot)
library(broom)
library(ggplot2)
library(tidyr)

df_dementia <- read.csv("D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\imputed_hrs_sectional_lowhigh_2010baseline.csv")

#reference group
df_dementia$educational_mobility_pr <- factor(df_dementia$educational_mobility_pr, 
                                              levels = c("Stably low","Downward mobility", "Upward mobility", "Stably high"))
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
df_dementia$us_born <- relevel(factor(df_dementia$us_born), ref = "1")
df_dementia$phy_act <- factor(df_dementia$phy_act)
df_dementia$soc_activity2010 <- factor(df_dementia$soc_activity2010)


####table - baseline 2010
t1 = CreateTableOne(vars = c("age","gender","education_quan","education_p_quan","educational_mobility_pr", "lb_status","marital_status","drink_now","smoking","wealth_quartile","n_chronic_category","us_born","urbanicity","Race","stress","health","financial","warmth","height2010","phy_act", "soc_activity2010","depression"),   
                    data=df_dementia)
print(t1, showAllLevels = TRUE, catDigits=2, printToggle = FALSE, , nonnormal = c("height2010","age")) %>%
  knitr::kable(caption = "Descriptive charateristics at Baseline 2010", 
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

####model - outcome from 2012 onwards
library(survival)
library(survminer)

# Cross-sectional model using baseline 2010 cognitive function
model_cognition_2012 <- glm(cogfunction2010_dementia ~ educational_mobility_pr + age + gender + Race + 
                             + height2010 + stress + health + financial + warmth + us_born, 
                            data = df_dementia,
                            family = binomial(link = "logit"))
summary(model_cognition_2012)
tab_model(model_cognition_2012)

#gender interaction
model_cognition_2012_gender <- glm(cogfunction2010_dementia ~educational_mobility_pr + age + gender + Race + 
                             + height2010 + stress + health + financial + warmth + us_born,
                            data = df_dementia,
                            family = binomial(link = "logit"))

tab_model(model_cognition_2012_gender)

#visualization for 2012 outcome model
model_cog_results <- tidy(model_cognition_2012, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term %in% c(
    "educational_mobility_prDownward mobility",
    "educational_mobility_prUpward mobility",
    "educational_mobility_prStably high"
  )) %>%
  mutate(term = recode(term,
                       "educational_mobility_prDownward mobility" = "Downwardly mobile",
                       "educational_mobility_prUpward mobility" = "Upwardly mobile",
                       "educational_mobility_prStably high" = "Stably high"
  ))

model_cog_results <- bind_rows(
  tibble(term = "Stably low (ref)", estimate = 1, conf.low = 1, conf.high = 1),
  model_cog_results
) %>%
  mutate(term = factor(term, levels = rev(c(
    "Stably low (ref)", "Downwardly mobile", "Upwardly mobile", "Stably high"
  ))))

print(model_cog_results)

xsec_2012 <- ggplot(model_cog_results, aes(x = estimate, y = term)) +
  geom_point(shape = 18, size = 6, color = "#1f77b4") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#1f77b4", size = 1.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_x_continuous(
    limits = c(0.1, 2),
    breaks = c(0.5, 1, 1.5, 2)) +
  coord_trans(x = "log10") +
  labs(x = "Odds Ratio (95% CI)", y = NULL, title = "HRS (Baseline 2010, Outcome from 2012)") +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )
print(xsec_2012)

jpeg("D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\hrs_cross_sectional_dementia_lowhigh_2010baseline.jpeg", width = 10, height = 6, units = 'in', res = 300)
print(xsec_2012)

dev.off()
