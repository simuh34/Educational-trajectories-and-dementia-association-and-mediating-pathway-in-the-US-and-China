library(tidyverse) #data tidying
library(leaps) #data tidying
library(ggplot2) #visualizing data
library(corrplot) #visualizing data
library(plyr) #data tidying
library(dplyr) 
library(glmnet) #for modeling
library(caret) #for modeling
library(xgboost) #for modeling
library(randomForest) #for modeling
library(pander) #data collation
library(mice) #check missing values
library(ggalluvial)
library(haven)
library(MCM)
library(ggsci)
library(reshape2)

data_r <- read_dta('D:\\R project\\data\\randhrs1992_2020v2.dta')
data_h <- read_dta('D:\\R project\\data\\H_HRS_c.dta')
df_cognition <- read_dta("D:\\R project\\data\\hrs\\cogfinalimp_9520wide.dta")

subdat_r <- subset(data_r,
                   select = c(
                     # Interview status for all waves
                     inw1, inw2, inw3, inw4, inw5, inw6, inw7, inw8, inw9, inw10, inw11, inw12, inw13, inw14, inw15,
                     r1iwstat, r2iwstat, r3iwstat, r4iwstat, r5iwstat, r6iwstat, r7iwstat, r8iwstat, r9iwstat, 
                     r10iwstat, r11iwstat, r12iwstat, r13iwstat, r14iwstat, r15iwstat,
                     r10wtresp, # baseline weight
                     hhid, hhidpn,
                     
                     # Interview time for all waves
                     r1iwmid, r2iwmid, r3iwmid, r4iwmid, r5iwmid, r6iwmid, r7iwmid, r8iwmid, r9iwmid,
                     r10iwmid, r11iwmid, r12iwmid, r13iwmid, r14iwmid, r15iwmid,
                     
                     # Demographics - time-invariant
                     rabyear,   # birth year
                     ragender,  # gender
                     raracem, rahispan, # race and ethnicity
                     
                     # Age for all waves (time-varying)
                     r1agey_e, r2agey_e, r3agey_e, r4agey_e, r5agey_e, r6agey_e, r7agey_e, r8agey_e, r9agey_e,
                     r10agey_e, r11agey_e, r12agey_e, r13agey_e, r14agey_e, r15agey_e,
                     
                     # Household wealth for all waves (time-varying)
                     h1atotb, h2atotb, h3atotb, h4atotb, h5atotb, h6atotb, h7atotb, h8atotb, h9atotb,
                     h10atotb, h11atotb, h12atotb, h13atotb, h14atotb, h15atotb,
                     
                     # People living with for all waves (time-varying)
                     h1hhres, h2hhres, h3hhres, h4hhres, h5hhres, h6hhres, h7hhres, h8hhres, h9hhres,
                     h10hhres, h11hhres, h12hhres, h13hhres, h14hhres, h15hhres,
                     
                     # Labour force status for all waves (time-varying)
                     r1lbrf, r2lbrf, r3lbrf, r4lbrf, r5lbrf, r6lbrf, r7lbrf, r8lbrf, r9lbrf,
                     r10lbrf, r11lbrf, r12lbrf, r13lbrf, r14lbrf, r15lbrf,
                     
                     # Marital status for all waves (time-varying)
                     r1mstath, r2mstath, r3mstath, r4mstath, r5mstath, r6mstath, r7mstath, r8mstath, r9mstath,
                     r10mstath, r11mstath, r12mstath, r13mstath, r14mstath, r15mstath,
                     
                     # Number of chronic diseases for all waves (time-varying)
                     r1conde, r2conde, r3conde, r4conde, r5conde, r6conde, r7conde, r8conde, r9conde,
                     r10conde, r11conde, r12conde, r13conde, r14conde, r15conde,
                     
                     # Smoking - ever smoked (time-invariant once yes)
                     r1smokev, r2smokev, r3smokev, r4smokev, r5smokev, r6smokev, r7smokev, r8smokev, r9smokev,
                     r10smokev, r11smokev, r12smokev, r13smokev, r14smokev, r15smokev,
                     
                     # Smoking - current smoker for all waves (time-varying)
                     r1smoken, r2smoken, r3smoken, r4smoken, r5smoken, r6smoken, r7smoken, r8smoken, r9smoken,
                     r10smoken, r11smoken, r12smoken, r13smoken, r14smoken, r15smoken,
                     
                     # Drinking - ever drinks for all waves (time-varying)
                     r1drink, r2drink, r3drink, r4drink, r5drink, r6drink, r7drink, r8drink, r9drink,
                     r10drink, r11drink, r12drink, r13drink, r14drink, r15drink,
                     
                     # Drinking frequency for all waves (time-varying)
                     r3drinkd, r4drinkd, r5drinkd, r6drinkd, r7drinkd, r8drinkd, r9drinkd,
                     r10drinkd, r11drinkd, r12drinkd, r13drinkd, r14drinkd, r15drinkd,
                     
                     # Vigorous activity for all waves (time-varying)
                     r7vgactx, r8vgactx, r9vgactx,
                     r10vgactx, r11vgactx, r12vgactx, r13vgactx, r14vgactx, r15vgactx,
                     
                     # Moderate activity for all waves (time-varying)
                     r7mdactx, r8mdactx, r9mdactx,
                     r10mdactx, r11mdactx, r12mdactx, r13mdactx, r14mdactx, r15mdactx,
                     
                     #self report height
                     r1height,r2height,r3height,r4height,r5height,r6height,
                     
                     # Word recall for all waves (time-varying)
                     r3tr20, r4tr20, r5tr20, r6tr20, r7tr20, r8tr20, r9tr20,
                     r10tr20, r11tr20, r12tr20, r13tr20,
                     
                     #serial sevens test (0–5 points)
                     r10ser7,r11ser7,r12ser7,r13ser7,
                     
                     # nativity
                     rabplace,
                     
                     # Exposure - time-invariant
                     raedyrs,  # years of education
                     rameduc,  # mother's years education
                     rafeduc,  # father's years education
                     
                     # Mediator - CESD for all waves (no data in wave 1)
                     r2cesd, r3cesd, r4cesd, r5cesd, r6cesd, r7cesd, r8cesd, r9cesd,
                     r10cesd, r11cesd, r12cesd, r13cesd, r14cesd, r15cesd,
                     
                     # Outcome - Stroke for all waves
                     r1stroke, r2stroke, r3stroke, r4stroke, r5stroke, r6stroke, r7stroke, r8stroke, r9stroke,
                     r10stroke, r11stroke, r12stroke, r13stroke, r14stroke, r15stroke,
                     
                     # Outcome - Heart disease for all waves
                     r1hearte, r2hearte, r3hearte, r4hearte, r5hearte, r6hearte, r7hearte, r8hearte, r9hearte,
                     r10hearte, r11hearte, r12hearte, r13hearte, r14hearte, r15hearte,
                     
                     # Outcome - all-cause mortality
                     raddate, # death date
                     
                     #Backwards Counting
                     r10bwc20,r11bwc20,r12bwc20,r13bwc20
                   ))


subdat_h <- subset(data_h,
                   select = c(hhid, hhidpn,
                              # Covariates - orientation for all waves (from wave2-wave13)
                              r2orient, r2orient, r3orient, r4orient, r5orient, 
                              r6orient, r7orient, r8orient, r9orient, r10orient,
                              r11orient, r12orient, r13orient,
                              
                              # Urbanicity for all waves
                              h1rural, h2rural, h3rural, h4rural, h5rural,
                              h6rural, h7rural, h8rural, h9rural, h10rural,
                              h11rural, h12rural, h13rural, h14rural,
                              
                              # Social activity for all waves (from wave9)
                              r9socwk, r10socwk,
                              r11socwk, r12socwk, r13socwk, r14socwk,
                              
                              # BMI for all waves
                              r7mbmi, r8mbmi, r9mbmi, r10mbmi,
                              r11mbmi, r12mbmi, r13mbmi, r14mbmi,
                              
                              # Height for all waves
                              r7mheight, r8mheight, r9mheight, r10mheight,
                              r11mheight, r12mheight, r13mheight, r14mheight,
                              
                              # Exposure 
                              raeducl,    # r harmonized education level
                              ramomeducl, # r mother harmonized education level
                              radadeducl, # r father harmonized education level 
                              radadoccup  # father's occupation
                   ))

data01 <- merge(subdat_r, subdat_h, by = c("hhid","hhidpn"))
names(data01)
names(data01) <- c(
  # ID variables
  "hhid", "hhidpn", 
  
  # Interview status (wave 1-15)
  "inw1992", "inw1994", "inw1996", "inw1998", "inw2000", 
  "inw2002", "inw2004", "inw2006", "inw2008", "inw2010",
  "inw2012", "inw2014", "inw2016", "inw2018", "inw2020",
  
  # Interview wave status (wave 1-15)
  "iwstat1992", "iwstat1994", "iwstat1996", "iwstat1998", "iwstat2000",
  "iwstat2002", "iwstat2004", "iwstat2006", "iwstat2008", "iwstat2010",
  "iwstat2012", "iwstat2014", "iwstat2016", "iwstat2018", "iwstat2020",
  
  # Survey weight
  "s_weight",
  
  # Interview dates (wave 1-15)
  "r1iwmid", "r2iwmid", "r3iwmid", "r4iwmid", "r5iwmid",
  "r6iwmid", "r7iwmid", "r8iwmid", "r9iwmid", "r10iwmid",
  "r11iwmid", "r12iwmid", "r13iwmid", "r14iwmid", "r15iwmid",
  
  # Demographics - time-invariant
  "birth_year", "gender", "race", "hispanic",
  
  # Age (wave 1-15) - time-varying
  "age1992", "age1994", "age1996", "age1998", "age2000",
  "age2002", "age2004", "age2006", "age2008", "age2010",
  "age2012", "age2014", "age2016", "age2018", "age2020",
  
  # Household wealth (wave 1-15) - time-varying
  "household_wealth1992", "household_wealth1994", "household_wealth1996", "household_wealth1998", "household_wealth2000",
  "household_wealth2002", "household_wealth2004", "household_wealth2006", "household_wealth2008", "household_wealth2010",
  "household_wealth2012", "household_wealth2014", "household_wealth2016", "household_wealth2018", "household_wealth2020",
  
  # People living with (wave 1-15) - time-varying
  "people_living_with1992", "people_living_with1994", "people_living_with1996", "people_living_with1998", "people_living_with2000",
  "people_living_with2002", "people_living_with2004", "people_living_with2006", "people_living_with2008", "people_living_with2010",
  "people_living_with2012", "people_living_with2014", "people_living_with2016", "people_living_with2018", "people_living_with2020",
  
  # Labour force status (wave 1-15) - time-varying
  "lb_status1992", "lb_status1994", "lb_status1996", "lb_status1998", "lb_status2000",
  "lb_status2002", "lb_status2004", "lb_status2006", "lb_status2008", "lb_status2010",
  "lb_status2012", "lb_status2014", "lb_status2016", "lb_status2018", "lb_status2020",
  
  # Marital status (wave 1-15) - time-varying
  "marital_status1992", "marital_status1994", "marital_status1996", "marital_status1998", "marital_status2000",
  "marital_status2002", "marital_status2004", "marital_status2006", "marital_status2008", "marital_status2010",
  "marital_status2012", "marital_status2014", "marital_status2016", "marital_status2018", "marital_status2020",
  
  # Number of chronic diseases (wave 1-15) - time-varying
  "n_chronic1992", "n_chronic1994", "n_chronic1996", "n_chronic1998", "n_chronic2000",
  "n_chronic2002", "n_chronic2004", "n_chronic2006", "n_chronic2008", "n_chronic2010",
  "n_chronic2012", "n_chronic2014", "n_chronic2016", "n_chronic2018", "n_chronic2020",
  
  # Ever smoked (wave 1-15) - time-varying
  "smoke_ever1992", "smoke_ever1994", "smoke_ever1996", "smoke_ever1998", "smoke_ever2000",
  "smoke_ever2002", "smoke_ever2004", "smoke_ever2006", "smoke_ever2008", "smoke_ever2010",
  "smoke_ever2012", "smoke_ever2014", "smoke_ever2016", "smoke_ever2018", "smoke_ever2020",
  
  # Current smoker (wave 1-15) - time-varying
  "smoke_now1992", "smoke_now1994", "smoke_now1996", "smoke_now1998", "smoke_now2000",
  "smoke_now2002", "smoke_now2004", "smoke_now2006", "smoke_now2008", "smoke_now2010",
  "smoke_now2012", "smoke_now2014", "smoke_now2016", "smoke_now2018", "smoke_now2020",
  
  # Currently drinks (wave 1-15) - time-varying
  "drink_ever1992", "drink_ever1994", "drink_ever1996", "drink_ever1998", "drink_ever2000",
  "drink_ever2002", "drink_ever2004", "drink_ever2006", "drink_ever2008", "drink_ever2010",
  "drink_ever2012", "drink_ever2014", "drink_ever2016", "drink_ever2018", "drink_ever2020",
  
  # Drinking frequency (wave 3-15, missing 1-2) - time-varying
  "drink_frq1996", "drink_frq1998", "drink_frq2000",
  "drink_frq2002", "drink_frq2004", "drink_frq2006", "drink_frq2008", "drink_frq2010",
  "drink_frq2012", "drink_frq2014", "drink_frq2016", "drink_frq2018", "drink_frq2020",
  
  # Vigorous activity (wave 7-15, missing 1-6) - time-varying
  "vigo_act2004", "vigo_act2006", "vigo_act2008", "vigo_act2010",
  "vigo_act2012", "vigo_act2014", "vigo_act2016", "vigo_act2018", "vigo_act2020",
  
  # Moderate activity (wave 7-15, missing 1-6) - time-varying
  "mode_act2004", "mode_act2006", "mode_act2008", "mode_act2010",
  "mode_act2012", "mode_act2014", "mode_act2016", "mode_act2018", "mode_act2020",
  
  #self report height
  "height1992","height1994","height1996","height1998","height2000","height2002",
  
  # Word recall (wave 3-13, missing 1-2, 14-15) - time-varying
  "word_recall1996", "word_recall1998", "word_recall2000", "word_recall2002",
  "word_recall2004", "word_recall2006", "word_recall2008", "word_recall2010",
  "word_recall2012", "word_recall2014", "word_recall2016",
  
  "r10ser7","r11ser7","r12ser7","r13ser7",
  
  # nativity
  "us_born",
  
  # Education - time-invariant
  "years_edu", "years_edu_m", "years_edu_f",
  
  # Outcome - CESD (wave 2-15, missing wave 1)
  "cesd1994", "cesd1996", "cesd1998", "cesd2000", "cesd2002",
  "cesd2004", "cesd2006", "cesd2008", "cesd2010", "cesd2012",
  "cesd2014", "cesd2016", "cesd2018", "cesd2020",
  
  # Outcome - Stroke (wave 1-15)
  "stroke1992", "stroke1994", "stroke1996", "stroke1998", "stroke2000",
  "stroke2002", "stroke2004", "stroke2006", "stroke2008", "stroke2010",
  "stroke2012", "stroke2014", "stroke2016", "stroke2018", "stroke2020",
  
  # Outcome - Heart disease (wave 1-15)
  "hearte1992", "hearte1994", "hearte1996", "hearte1998", "hearte2000",
  "hearte2002", "hearte2004", "hearte2006", "hearte2008", "hearte2010",
  "hearte2012", "hearte2014", "hearte2016", "hearte2018", "hearte2020",
  
  # Death date
  "death_date",
  
  #back count
  "count2010","count2012","count2014","count2016",
  
  # Orientation (wave 2-13, with duplicate)
  "orient1994", "orient1994_dup", "orient1996", "orient1998", "orient2000",
  "orient2002", "orient2004", "orient2006", "orient2008", "orient2010",
  "orient2012", "orient2014", "orient2016",
  
  # Urbanicity (wave 1-14)
  "urbanicity1992", "urbanicity1994", "urbanicity1996", "urbanicity1998", "urbanicity2000",
  "urbanicity2002", "urbanicity2004", "urbanicity2006", "urbanicity2008", "urbanicity2010",
  "urbanicity2012", "urbanicity2014", "urbanicity2016", "urbanicity2018",
  
  # Social activity (wave 9-14)
  "soc_activity2008", "soc_activity2010", "soc_activity2012", 
  "soc_activity2014", "soc_activity2016", "soc_activity2018",
  
  # BMI (wave 7-14)
  "BMI2004", "BMI2006", "BMI2008", "BMI2010",
  "BMI2012", "BMI2014", "BMI2016", "BMI2018",
  
  # Height (wave 7-14)
  "height2004", "height2006", "height2008", "height2010",
  "height2012", "height2014", "height2016", "height2018",
  
  # Harmonized education levels
  "H_edu", "H_edu_m", "H_edu_f", "f_occupation"
)

# Interview time
base_date <- as.Date("1960-01-01") 

# Wave 1 (1992)
date_from_r1iwmid <- base_date + data01$r1iwmid 
data01$r1iwmid_year <- as.numeric(format(date_from_r1iwmid, "%Y"))
data01$r1iwmid_month <- as.numeric(format(date_from_r1iwmid, "%m"))
data01$riwmid_w1 <- data01$r1iwmid_year + data01$r1iwmid_month/12

# Wave 2 (1994)
date_from_r2iwmid <- base_date + data01$r2iwmid 
data01$r2iwmid_year <- as.numeric(format(date_from_r2iwmid, "%Y"))
data01$r2iwmid_month <- as.numeric(format(date_from_r2iwmid, "%m"))
data01$riwmid_w2 <- data01$r2iwmid_year + data01$r2iwmid_month/12

# Wave 3 (1996)
date_from_r3iwmid <- base_date + data01$r3iwmid 
data01$r3iwmid_year <- as.numeric(format(date_from_r3iwmid, "%Y"))
data01$r3iwmid_month <- as.numeric(format(date_from_r3iwmid, "%m"))
data01$riwmid_w3 <- data01$r3iwmid_year + data01$r3iwmid_month/12

# Wave 4 (1998)
date_from_r4iwmid <- base_date + data01$r4iwmid 
data01$r4iwmid_year <- as.numeric(format(date_from_r4iwmid, "%Y"))
data01$r4iwmid_month <- as.numeric(format(date_from_r4iwmid, "%m"))
data01$riwmid_w4 <- data01$r4iwmid_year + data01$r4iwmid_month/12

# Wave 5 (2000)
date_from_r5iwmid <- base_date + data01$r5iwmid 
data01$r5iwmid_year <- as.numeric(format(date_from_r5iwmid, "%Y"))
data01$r5iwmid_month <- as.numeric(format(date_from_r5iwmid, "%m"))
data01$riwmid_w5 <- data01$r5iwmid_year + data01$r5iwmid_month/12

# Wave 6 (2002)
date_from_r6iwmid <- base_date + data01$r6iwmid 
data01$r6iwmid_year <- as.numeric(format(date_from_r6iwmid, "%Y"))
data01$r6iwmid_month <- as.numeric(format(date_from_r6iwmid, "%m"))
data01$riwmid_w6 <- data01$r6iwmid_year + data01$r6iwmid_month/12

# Wave 7 (2004)
date_from_r7iwmid <- base_date + data01$r7iwmid 
data01$r7iwmid_year <- as.numeric(format(date_from_r7iwmid, "%Y"))
data01$r7iwmid_month <- as.numeric(format(date_from_r7iwmid, "%m"))
data01$riwmid_w7 <- data01$r7iwmid_year + data01$r7iwmid_month/12

# Wave 8 (2006)
date_from_r8iwmid <- base_date + data01$r8iwmid 
data01$r8iwmid_year <- as.numeric(format(date_from_r8iwmid, "%Y"))
data01$r8iwmid_month <- as.numeric(format(date_from_r8iwmid, "%m"))
data01$riwmid_w8 <- data01$r8iwmid_year + data01$r8iwmid_month/12

# Wave 9 (2008)
date_from_r9iwmid <- base_date + data01$r9iwmid 
data01$r9iwmid_year <- as.numeric(format(date_from_r9iwmid, "%Y"))
data01$r9iwmid_month <- as.numeric(format(date_from_r9iwmid, "%m"))
data01$riwmid_w9 <- data01$r9iwmid_year + data01$r9iwmid_month/12

# Wave 10 (2010)
date_from_r10iwmid <- base_date + data01$r10iwmid 
data01$r10iwmid_year <- as.numeric(format(date_from_r10iwmid, "%Y"))
data01$r10iwmid_month <- as.numeric(format(date_from_r10iwmid, "%m"))
data01$riwmid_w10 <- data01$r10iwmid_year + data01$r10iwmid_month/12

# Wave 11 (2012)
date_from_r11iwmid <- base_date + data01$r11iwmid 
data01$r11iwmid_year <- as.numeric(format(date_from_r11iwmid, "%Y"))
data01$r11iwmid_month <- as.numeric(format(date_from_r11iwmid, "%m"))
data01$riwmid_w11 <- data01$r11iwmid_year + data01$r11iwmid_month/12

# Wave 12 (2014)
date_from_r12iwmid <- base_date + data01$r12iwmid 
data01$r12iwmid_year <- as.numeric(format(date_from_r12iwmid, "%Y"))
data01$r12iwmid_month <- as.numeric(format(date_from_r12iwmid, "%m"))
data01$riwmid_w12 <- data01$r12iwmid_year + data01$r12iwmid_month/12

# Wave 13 (2016)
date_from_r13iwmid <- base_date + data01$r13iwmid 
data01$r13iwmid_year <- as.numeric(format(date_from_r13iwmid, "%Y"))
data01$r13iwmid_month <- as.numeric(format(date_from_r13iwmid, "%m"))
data01$riwmid_w13 <- data01$r13iwmid_year + data01$r13iwmid_month/12

# Wave 14 (2018)
date_from_r14iwmid <- base_date + data01$r14iwmid 
data01$r14iwmid_year <- as.numeric(format(date_from_r14iwmid, "%Y"))
data01$r14iwmid_month <- as.numeric(format(date_from_r14iwmid, "%m"))
data01$riwmid_w14 <- data01$r14iwmid_year + data01$r14iwmid_month/12

# Wave 15 (2020)
date_from_r15iwmid <- base_date + data01$r15iwmid 
data01$r15iwmid_year <- as.numeric(format(date_from_r15iwmid, "%Y"))
data01$r15iwmid_month <- as.numeric(format(date_from_r15iwmid, "%m"))
data01$riwmid_w15 <- data01$r15iwmid_year + data01$r15iwmid_month/12

data01$r10iwmid_year <- as.numeric(format(date_from_r10iwmid, "%Y"))
data01$r10iwmid_month <- as.numeric(format(date_from_r10iwmid, "%m"))

data01$r11iwmid_year <- as.numeric(format(date_from_r11iwmid, "%Y"))
data01$r11iwmid_month <- as.numeric(format(date_from_r11iwmid, "%m"))

data01$r12iwmid_year <- as.numeric(format(date_from_r12iwmid, "%Y"))
data01$r12iwmid_month <- as.numeric(format(date_from_r12iwmid, "%m"))

data01$r13iwmid_year <- as.numeric(format(date_from_r13iwmid, "%Y"))
data01$r13iwmid_month <- as.numeric(format(date_from_r13iwmid, "%m"))

data01$r14iwmid_year <- as.numeric(format(date_from_r14iwmid, "%Y"))
data01$r14iwmid_month <- as.numeric(format(date_from_r14iwmid, "%m"))

data01$r15iwmid_year <- as.numeric(format(date_from_r15iwmid, "%Y"))
data01$r15iwmid_month <- as.numeric(format(date_from_r15iwmid, "%m"))

data01$riwmid_1992 <- data01$r1iwmid_year + data01$r1iwmid_month/12
data01$riwmid_1994 <- data01$r2iwmid_year + data01$r2iwmid_month/12
data01$riwmid_1996 <- data01$r3iwmid_year + data01$r3iwmid_month/12
data01$riwmid_1998 <- data01$r4iwmid_year + data01$r4iwmid_month/12
data01$riwmid_2000 <- data01$r5iwmid_year + data01$r5iwmid_month/12
data01$riwmid_2002 <- data01$r6iwmid_year + data01$r6iwmid_month/12
data01$riwmid_2004 <- data01$r7iwmid_year + data01$r7iwmid_month/12
data01$riwmid_2006 <- data01$r8iwmid_year + data01$r8iwmid_month/12
data01$riwmid_2008 <- data01$r9iwmid_year + data01$r9iwmid_month/12
data01$riwmid_2010 <- data01$r10iwmid_year + data01$r10iwmid_month/12
data01$riwmid_2012 <- data01$r11iwmid_year + data01$r11iwmid_month/12
data01$riwmid_2014 <- data01$r12iwmid_year + data01$r12iwmid_month/12
data01$riwmid_2016 <- data01$r13iwmid_year + data01$r13iwmid_month/12
data01$riwmid_2018 <- data01$r14iwmid_year + data01$r14iwmid_month/12
data01$riwmid_2020 <- data01$r15iwmid_year + data01$r15iwmid_month/12

#write.csv(data01,'hrs.csv')

# subdat_socre <- subset(data_r,
#                    select = c(hhid,hhidpn,
#                               #dementia related
#                               r10imrc,r11imrc,r12imrc,
#                               r10dlrc,r11dlrc,r12dlrc,
#                               r10cog27,r11cog27,r12cog27,
#                               r10ser7,r11ser7,r12ser7,
#                               r10bwc20,r11bwc20,r12bwc20,
#                               raracem,rahispan,
#                               r10tr20,r11tr20,r12tr20,
#                               r10iwbeg,r11iwbeg,r12iwbeg,
#                               radyear,radmonth,raddate))


df_cognition$hhid <- sub("^0", "", df_cognition$hhid)
df_cognition$hhidpn <- paste(as.character(df_cognition$hhid), as.character(df_cognition$pn), sep = "")

data01$hhidpn <- as.numeric(data01$hhidpn)
df_cognition$hhidpn <- as.numeric(df_cognition$hhidpn)

df_dementia <- left_join(
  data01, df_cognition %>% select("hhidpn","cogfunction1996","cogfunction1998","cogfunction2000","cogfunction2002","cogfunction2004","cogfunction2006","cogfunction2008","cogfunction2010","cogfunction2012","cogfunction2014","cogfunction2016","cogfunction2018"),
  by = "hhidpn")

#df_dementia <- left_join(data01,df_dementia, by = "hhidpn")

write.csv(df_dementia,'D:\\R project\\Educational Mobility and dementia\\Analysis\\HRS\\hrs.csv')

