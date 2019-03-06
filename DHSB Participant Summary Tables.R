#####Digital Health-Seeking Behaviors
#####Creation of Participant Summary Tables

#####Read libraries
library(tidyverse)
library(sjlabelled)
library(rio)

#####Create data
source("DHSB Data Processing.R")

#####Format variables and rename
demo <- acasi %>%
  select(-EMPLOY, -RACE,
         -starts_with("DRUG"), -starts_with("MTU"), -starts_with("S56")) %>%
  rename(Site = SITE1, Age = SCREEN1) %>%
  mutate(`Age Groups` = as.factor(ifelse(Age < 25, "13-24 Years", "25-34 Years")),
         SCREEN5 = fct_recode(as.factor(SCREEN5),
                              "Within past 12 months" = "1", "More than 12 months ago" = "2",
                              "Don't know/Not sure"   = "7", "Refuse to answer"        = "8"),
         GENDER = fct_recode(as.factor(GENDER),
                             "Male (cis man)"            = "1", "Female (cis woman)" = "2",
                             "Trans man"                 = "3", "Trans woman"        = "4",
                             "Genderqueer/Nonconforming" = "5", "Other gender"       = "6",
                             "Refuse to answer"          = "8"),
         SEXBRTH = fct_recode(as.factor(SEXBRTH),
                              "Male" = "1", "Female" = "2", "Refuse to answer" = "8"),
         ORIENT = fct_recode(as.factor(ORIENT),
                             "Straight"  = "1", "Lesbian or gay"    = "2", "Bisexual"            = "3",
                             "Queer"     = "4", "Other orientation" = "5", "Don't know/Not sure" = "7",
                             "Refuse to answer"    = "8"),
         RELSTAT = fct_recode(as.factor(RELSTAT),
                              "Single"                                 = "1", "Dating, open relationship"            = "2", 
                              "Dating, closed relationship"            = "3", "Partnered/Married, open relationship" = "4",
                              "Partnered/Married, closed relationship" = "5", "Other relationship status"          = "6",
                              "Refuse to answer" = "8"),
         GRADE = fct_recode(as.factor(GRADE),
                            "Junior high or less"       = "1", "Some high school"  = "2", 
                            "High school or equivalent" = "3", "Some post-K12"     = "4", 
                            "Associate or trade cert"   = "5", "Bachelor's degree" = "6",
                            "Higher than bachelor's"    = "7", "Refuse to answer"  = "8"),
         STAY7D = fct_recode(as.factor(STAY7D),
                             "Rented house/apt/flat"      =  "1", "Family's house/apt/flat"              =  "2",
                             "Friend's house/apt/flat"    =  "3", "Lover or partner's house/apt/flat"    =  "4",
                             "Hotel/Motel/Boarding house" =  "5", "Halfway house/Drug treatment center"  =  "6",
                             "Homeless shelter/mission"   =  "7", "Domestic violence shelter/Safe house" =  "8",
                             "Hospital/Medical facility"  =  "9", "Jail/Juvenile detention"              = "10",
                             "Other residence"            = "11", "Refuse to answer"                     = "12"),
         MONEY = ifelse(MONEY %in% c(99997, 99998), NA, MONEY)) %>%
  mutate_at(vars(matches("EMPLOY|LATINO|RACE|INSURE|CARELHIV"), JAILL, -RACEFS, -INSURE, -INSUREHS), 
            funs(as.factor)) %>%
  mutate_at(vars(matches("EMPLOY|LATINO|RACE|INSURE|CARELHIV"), JAILL, -RACEFS, -INSURE, -INSUREHS), 
            funs(fct_recode), "Yes" = "1", "No" = "0", "Don't know/Not sure" = "7", "Refuse to answer" = "8") %>%
  mutate_at(vars(contains("CARE"), -CARELHIV), funs(as.factor(case_when(. == 0   ~ "No",
                                                                        . <= 99  ~ "Yes",
                                                                        . >  99  ~ "Refuse to answer")))) %>%
  unite("SitePID", Site, PID, remove = FALSE) %>%
  arrange(Site, PID) %>%
  rename(Gender = GENDER,
         `Sex at Birth` = SEXBRTH,
         Orientation = ORIENT,
         `Relationship Status` = RELSTAT,
         Education = GRADE,
         `Income, Last Month` = MONEY,
         `Residence, Last 7 Days` = STAY7D,
         `Ever Jailed` = JAILL,
         Student  = EMPLOYA, `Full-time employed`  = EMPLOYB, `Part-time employed`      = EMPLOYC,
         Disabled = EMPLOYD, `Unemployed, looking` = EMPLOYE, `Unemployed, not looking` = EMPLOYF,
         Latino = LATINO,
         `American Indian/Alaska Native`    = RACEA, Asian = RACEB, `Black/African American` = RACEC,
         `Native Hawaiian/Pacific Islander` = RACED, White = RACEE, `Other race`             = RACEF,
         `No insurance`                 = INSUREA, Medicaid            = INSUREB, Medicare         = INSUREC, 
         `Private or employer-provided` = INSURED, `Student insurance` = INSUREE, `Through parent` = INSUREF, 
         `Through partner`              = INSUREG, `Other insurance`   = INSUREH,
         `HIV Diagnosis` = SCREEN5,
         `Doctor's visit, non-HIV, 7-12 months ago` = CARED6_00M, 
         `ER or urgent care, non-HIV, 7-12 months ago` = CAREE6_00M,
         `Hospital stay, non-HIV, 7-12 months ago` = CAREH6_00M, `Doctor's visit, HIV, lifetime` = CARELHIV,
         `Doctor's visit, HIV, 7-12 months ago` = CAREHV06_00M, `Doctor's visit, HIV, 13-18 months ago` = CAREHV12,
         `Doctor's visit, non-HIV, 0-6 months ago` = CARED6, 
         `ER or urgent care, non-HIV, 0-6 months ago` = CAREE6,
         `Hospital stay, non-HIV, 0-6 months ago` = CAREH6,
         `Doctor's visit, HIV, 0-6 months ago` = CAREHV06)
