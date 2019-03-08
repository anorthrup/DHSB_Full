#####Digital Health-Seeking Behaviors
#####Creation of Participant Summary Tables

#####Read libraries
library(tidyverse)
library(sjlabelled)
library(rio)
library(openxlsx)
library(scales)

#####Create data
source("DHSB Data Processing.R")

#####Print "Other" text for each demographic variable
# wb <- createWorkbook()
# for (i in c("GENDERS", "RACEFS", "ORIENTS", "LIVED6LS", "STAY7DS", 
#            "INSUREHS", "SSNDOS", "CNEED3ES", "DISCJS")) {
#   addWorksheet(wb, i)
#   writeData(wb, i, acasi %>%
#               select(SITE1, PID, i) %>%
#               filter(!!sym(i) != "[Skipped]"))
# }
# saveWorkbook(wb, "ACASI Other Text.xlsx", overwrite = TRUE)

#####Format variables and rename
demo <- acasi2 %>%
  select(-EMPLOY, -RACE,
         -starts_with("DRUG"), -starts_with("MTU"), -starts_with("S56")) %>%
  rename(Site = SITE1, Age = AGE) %>%
  mutate(`Age Groups` = as.factor(case_when(Age < 18 ~ "Under 18 Years",
                                            Age < 25 ~ "18-24 Years",
                                            TRUE ~ "25 and older")),
         SCREEN5 = fct_recode(as.factor(SCREEN5),
                              "Within past 12 months" = "1", "More than 12 months ago" = "2",
                              "Don't know/Not sure"   = "7", "Refuse to answer"        = "8"),
         SEXBRTH = fct_recode(as.factor(SEXBRTH),
                              "Male" = "1", "Female" = "2", "Refuse to answer" = "8"),
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
  rename(Gender = GENDER_RC,
         `Sex at Birth` = SEXBRTH,
         Orientation = ORIENT_RC,
         `Relationship Status` = RELSTAT_RC,
         Education = GRADE_RC,
         `Income, Last Month` = MONEY,
         `Residence, Last 7 Days` = STAY7D_RC,
         `Ever Jailed` = JAILL,
         Race = RACE_RC,
         Student  = EMPLOYA, `Full-time employed`  = EMPLOYB, `Part-time employed`      = EMPLOYC,
         Disabled = EMPLOYD, `Unemployed` = EMPLOYE_RC,
         `No insurance`                 = INSUREA, Medicaid            = INSUREB, Medicare         = INSUREC, 
         `Private or employer-provided` = INSURED, `Student insurance` = INSUREE, `Through parent` = INSUREF, 
         `Through partner`              = INSUREG, `Other insurance`   = INSUREH,
         `HIV Diagnosis` = SCREEN5,
         `Doctor's visit, non-HIV, past 6 months` = CARED6, 
         `ER or urgent care, non-HIV, past 6 months` = CAREE6,
         `Hospital stay, non-HIV, past 6 months` = CAREH6,
         `Doctor's visit, HIV, past 6 months` = CAREHV06,
         `Doctor's visit, HIV, past 12 months` = CAREHV12,
         `Doctor's visit, HIV, lifetime` = CARELHIV)

#####Create summary tables
#####Table 1: participant characteristics summary
#Number of participants
tab1_n <- bind_rows(
  demo %>%
    summarize(Frequency = n()) %>%
    mutate(Site = "Overall"),
  demo %>%
    group_by(Site) %>%
    summarize(Frequency = n()) %>%
    mutate(Site = as.character(Site))
) %>%
  mutate(Frequency = as.character(Frequency)) %>%
  spread(Site, Frequency) %>%
  mutate(Variable = "Number of Participants") %>%
  select(Variable, Overall, everything())
#Age groups
tab1Multi <- function (x, varString) {
  varName <- str_replace(varString, " ", ".")
  bind_rows(x %>%
              select(varString) %>%
              table() %>%
              as.data.frame(., stringsAsFactors = FALSE) %>%
              setNames(c("Variable", "N")) %>%
              mutate(Percent = percent(N / sum(N), accuracy = 0.1),
                     Site = "Overall"),
            x %>%
              select(Site, !!sym(varString)) %>%
              table() %>%
              as.data.frame(., stringsAsFactors = FALSE) %>%
              setNames(c("Site", "Variable", "N")) %>%
              group_by(Site) %>%
              mutate(Percent = percent(N / sum(N), accuracy = 0.1))) %>%
    unite("Frequency", N, Percent, sep = " (") %>%
    mutate(Frequency = str_replace(Frequency, "(.*)", "\\1)")) %>%
    spread(Site, Frequency) %>%
    select(Variable, Overall, everything())
}
tab1_age <- tab1Multi(demo, varString = "Age Groups") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Under 18 Years")) %>%
  arrange(Variable)
#HIV Diagnosis
tab1_HIV <- tab1Multi(demo, varString = "HIV Diagnosis") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Within past 12 months",
                                "More than 12 months ago")) %>%
  arrange(Variable)
#Sex at birth
tab1_sex <- tab1Multi(demo, varString = "Sex at Birth") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Male")) %>%
  arrange(Variable)
#Gender
tab1_gender <- tab1Multi(demo, varString = "Gender") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Male (cis man)",
                                "Female (cis woman)",
                                "Trans person")) %>%
  arrange(Variable)
#Orientation
tab1_orientation <- tab1Multi(demo, varString = "Orientation") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Straight",
                                "Lesbian or gay",
                                "Bisexual")) %>%
  arrange(Variable)
#Relationship Status
tab1_relationship <- tab1Multi(demo, varString = "Relationship Status") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Single",
                                "Dating",
                                "Partnered/Married")) %>%
  arrange(Variable)
#Education
tab1_education <- tab1Multi(demo, varString = "Education") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "High school, equivalent or less",
                                "Some post-K12")) %>%
  arrange(Variable)
#Income
tab1_income <- bind_rows(
  demo %>%
    summarize(Metric = as.character(
      paste0(
        median(`Income, Last Month`, na.rm = TRUE), " [",
        paste(quantile(`Income, Last Month`, c(0.25, 0.75), na.rm = TRUE),
              collapse = ", "),
        "]"
      ))
    ) %>%
    mutate(Site = "Overall"),
  demo %>%
    group_by(Site) %>%
    summarize(Metric = as.character(
      paste0(
        median(`Income, Last Month`, na.rm = TRUE), " [",
        paste(quantile(`Income, Last Month`, c(0.25, 0.75), na.rm = TRUE),
              collapse = ", "),
        "]"
      ))
    ) %>%
    mutate(Site = as.character(Site))) %>%
  spread(Site, Metric) %>%
  mutate(Variable = "Income, Last Month") %>%
  select(Variable, Overall, everything())
#Residence
tab1_residence <- tab1Multi(demo, varString = "Residence, Last 7 Days") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Rented house/apt/flat",
                                "Other's house/apt/flat",
                                "Hotel/Shelter/Rehab",
                                "Hospital/Medical facility")) %>%
  arrange(Variable)
#Jailed
tab1_jail <- tab1Multi(demo, varString = "Ever Jailed") %>%
  filter(Variable == "Yes") %>%
  mutate(Variable = str_replace(Variable, "Yes", "Ever Jailed"))
#Ethnicity/Race
tab1_race <- tab1Multi(demo, varString = "Race") %>%
  mutate(Variable = fct_relevel(as.factor(Variable), 
                                "Latino",
                                "Black, Not Latino",
                                "White, Not Latino",
                                "White Mixed-Race, Not Latino or Black")) %>%
  arrange(Variable)
#Employment

