#####Digital Health-Seeking Behaviors
#####> Comparison of participants with complete baseline/6 months survey and those missing 6 months

#####Read libraries
library(tidyverse)
library(rio)

#####Load data
setwd("C:/Users/anorthrup/Box Sync/ETAC")
load("Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/acasi.RData")

#####Correct answers for participants who marked 'Other'
acasiOther <- left_join(
  acasiJoinInner,
  import("C:/Users/anorthrup/Box Sync/ETAC/Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/ACASI Other Text_2019-04-22.xlsx",
         sheet = "GENDERS"),
  by = c("SITE1", "PID")
) %>%
  left_join(
    .,
    import("C:/Users/anorthrup/Box Sync/ETAC/Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/ACASI Other Text_2019-04-22.xlsx",
           sheet = "RACEFS"),
    by = c("SITE1", "PID")
  ) %>%
  left_join(
    .,
    import("C:/Users/anorthrup/Box Sync/ETAC/Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/ACASI Other Text_2019-04-22.xlsx",
           sheet = "ORIENTS"),
    by = c("SITE1", "PID")
  ) %>%
  left_join(
    .,
    import("C:/Users/anorthrup/Box Sync/ETAC/Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/ACASI Other Text_2019-04-22.xlsx",
           sheet = "STAY7DS"),
    by = c("SITE1", "PID")
  ) %>%
  left_join(
    .,
    import("C:/Users/anorthrup/Box Sync/ETAC/Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/ACASI Other Text_2019-04-22.xlsx",
           sheet = "INSUREHS"),
    by = c("SITE1", "PID")
  ) %>%
  select(SITE1, PID, GENDERRECODE, RACERECODE, ORIENTRECODE, STAYRECODE, 
         INSURERECODE)

#####Combine data from acasiJoin00m (participants with only baseline), and acasiJoinInner (participants with both)
acasiCompare <- bind_rows(
  acasiJoinInner %>%
    select(colnames(acasiJoin00m)) %>%
    mutate(completeSurveys = 1),
  acasiJoin00m %>%
    mutate(
      completeSurveys = 0,
      SITE1 = fct_recode(as.factor(SITE1),
                         "CBW" = "1", "FRI" = "2", "NYSDA" = "3", 
                         "HBHC" = "4", "MHS"  = "5", "PSU" = "6", 
                         "PFC" = "7", "SFDPH" = "8", "WFU"  = "9", 
                         "WUSL" = "10"),
      SITE1 = as.character(SITE1)
    )
) %>%
  arrange(SITE1) %>%
  mutate(SITE1 = as.character(SITE1)) %>%
  filter(AGE >= 18) %>%
  left_join(., acasiOther, by = c("SITE1", "PID")) %>%
  mutate(
    AGE_RC = as.factor(case_when(AGE < 25 ~ "18-24 Years",
                                 AGE >= 25 ~ "25-34 Years")),
    #> Ethnicity & Race
    RACE_RC = case_when(LATINO == 1 ~ "Latino",
                        RACEC == 1 ~ "Black, Not Latino",
                        RACEE == 1 & RACE > 1 ~ 
                          "White Mixed-Race, Not Latino or Black",
                        RACEE == 1 ~ "White, Not Latino",
                        LATINO == 8 & RACE == 8 ~ "Refuse to answer", #None refused to answer
                        !is.na(RACERECODE) ~ RACERECODE,
                        TRUE ~ "Other race"),
    #> Gender Identity
    GENDER_RC = fct_recode(as.factor(GENDER),
                           "Male (cis man)"     = "1", 
                           "Female (cis woman)" = "2",
                           "Trans-identified"   = "3", 
                           "Trans-identified"   = "4",
                           "Other gender"       = "5", 
                           "Other gender"       = "6",
                           "Refuse to answer"   = "8"), #None refused to answer
    GENDER_RC = if_else(!is.na(GENDERRECODE), 
                        GENDERRECODE, as.character(GENDER_RC)),
    #> Sexual Orientation
    ORIENT_RC = fct_recode(as.factor(ORIENT),
                           "Straight"          = "1", 
                           "Gay or lesbian"    = "2", 
                           "Bisexual"          = "3",
                           "Other orientation" = "4", 
                           "Other orientation" = "5", 
                           "Other orientation" = "7",
                           "Refuse to answer"  = "8"), #None refused to answer
    ORIENT_RC = if_else(!is.na(ORIENTRECODE), 
                        ORIENTRECODE, as.character(ORIENT_RC)),
    #> Education
    GRADE_RC = fct_recode(as.factor(GRADE),
                          "High school, equivalent or less"         = "1", 
                          "High school, equivalent or less"         = "2", 
                          "High school, equivalent or less"         = "3", 
                          "Some post-K12"                           = "4", 
                          "College graduate or trade certification" = "5", 
                          "College graduate or trade certification" = "6",
                          "College graduate or trade certification" = "7", 
                          "Refuse to answer"                        = "8"), #None refused to answer
    #> Income
    MONEY_RC = ifelse(MONEY %in% c(99997, 99998), NA, MONEY),
    MONEY_RC_Log = log(MONEY_RC + 1),
    #> Residence, Last 7 Days
    STAY7D_RC = fct_recode(as.factor(STAY7D),
                           "Stable housing"   =  "1", 
                           "Unstable housing" =  "2",
                           "Unstable housing" =  "3", 
                           "Unstable housing" =  "4",
                           "Unstable housing" =  "5", 
                           "Unstable housing" =  "6",
                           "Unstable housing" =  "7", 
                           "Unstable housing" =  "8",
                           "Unstable housing" =  "9", 
                           "Unstable housing" = "10",
                           "Unstable housing" = "11", 
                           "Other residence"  = "12",
                           "Refuse to answer" = "98"), #None refused to answer
    STAY7D_RC = if_else(!is.na(STAYRECODE), 
                        STAYRECODE, as.character(STAY7D_RC)),
    #> Insurance
    INSURE_RC = case_when(INSUREA == 1 ~ "Not insured",
                          INSURE == 97 ~ "Don't know",
                          INSURE == 98 ~ "Refuse to answer",
                          INSURE == 99 ~ "Skipped", #None refused to answer
                          INSUREB == 1 | INSUREC == 1 | INSURED == 1 |
                            INSUREE == 1 | INSUREF == 1 | INSUREG == 1 ~ "Insured",
                          TRUE ~ INSURERECODE)
  ) %>%
  #Participants with incomplete 06m surveys needing 'Other' correction
  mutate(STAY7D_RC = replace(STAY7D_RC,
                             which(SITE1 == "MHS" & PID == "3714"),
                             "Unstable housing"))

#####Make comparisons
#Categorical variables
chiAcasi <- function (x, variable) {
  output <- x %>%
    select(completeSurveys, variable) %>%
    table() %>%
    chisq.test()
    tibble(
      Variable = variable,
      `X-squared` = output$statistic,
      df = output$parameter,
      p = output$p.value
    )
}

compareCategorical <- bind_rows(
  chiAcasi(acasiCompare, "AGE_RC"),
  chiAcasi(acasiCompare %>% 
             mutate(RACE_RC = replace(RACE_RC,
                                      which(RACE_RC == "White Mixed-Race, Not Latino or Black"),
                                      "White, Not Latino")),
             # filter(RACE_RC != "White Mixed-Race, Not Latino or Black"), 
           "RACE_RC"),
  chiAcasi(acasiCompare, "GENDER_RC"),
  chiAcasi(acasiCompare, "ORIENT_RC"),
  chiAcasi(acasiCompare, "GRADE_RC"),
  chiAcasi(acasiCompare, "STAY7D_RC"),
  chiAcasi(acasiCompare, "INSURE_RC")
) %>%
  mutate(Note = "",
         Note = replace(Note, which(Variable == "RACE_RC"),
                        "'White, Mixed' incorporated into 'White, Not Latino'"),
         Note = replace(Note, which(Variable == "STAY7D_RC"),
                        "'Institution' incorporated into 'Unstable housing'"))

#Continuous Variables
logitAcasi <- function (model, variable) {
  modelSum <- summary(model)
  modelCoef <- modelSum$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("Parameter")
  tibble(
    Variable = variable,
    `Z statistic` = modelCoef$`z value`[which(modelCoef$Parameter == variable)],
    p = modelCoef$`Pr(>|z|)`[which(modelCoef$Parameter == variable)]
  )
}

#AGE
logitAge <- glm(completeSurveys ~ AGE, data = acasiCompare, family = "binomial")
arm::binnedplot(x = predict(logitAge),
                y = resid(logitAge))
ggplot(acasiCompare, aes(x = MONEY_RC_Log, y = completeSurveys)) +
  geom_point() +
  geom_smooth(method = "loess")

#MONEY
logitMoney <- glm(completeSurveys ~ MONEY_RC_Log, data = acasiCompare, family = "binomial")
arm::binnedplot(x = predict(logitMoney),
                y = resid(logitMoney))
ggplot(acasiCompare, aes(x = MONEY_RC_Log, y = completeSurveys)) +
  geom_point() +
  geom_smooth(method = "loess")

compareContinuous <- bind_rows(
  logitAcasi(logitAge, "AGE"),
  logitAcasi(logitMoney, "MONEY_RC_Log")
)

##### Create Excel sheet
wb <- createWorkbook("Comparison of Included and Excluded Participants.xlsx")
wb <- loadWorkbook("Comparison of Included and Excluded Participants.xlsx")
#Categorical variables
removeWorksheet(wb, "Categorical")
addWorksheet(wb, "Categorical")
writeData(wb, sheet = "Categorical", compareCategorical)
setColWidths(wb, sheet = "Categorical", cols = 1:ncol(compareCategorical),
             widths = "auto")
#Continuous variables
removeWorksheet(wb, "Continuous")
addWorksheet(wb, "Continuous")
writeData(wb, sheet = "Continuous", compareContinuous)
setColWidths(wb, sheet = "Continuous", cols = 1:ncol(compareContinuous),
             widths = "auto")
saveWorkbook(wb, "Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/Comparison of Included and Excluded Participants.xlsx", overwrite = TRUE)
