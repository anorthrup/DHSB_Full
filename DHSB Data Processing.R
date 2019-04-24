#####Digital Health-Seeking Behaviors
#####> Data Processing and Variable Transformations

#####Read libraries
library(tidyverse)
library(lubridate)
library(sjlabelled)
library(rio)

#####Read data
#import() doesn't read all missing values the same, must write to and read from CSV and specify NA strings
#Need to save labels for codebook

#00m
data00mSAS <- import("Data merged across sites/acasibase.sas7bdat")
##Save labels
labels00m <- get_label(data00mSAS) %>% 
  as.data.frame(., stringsAsFactors = FALSE) %>%
  rownames_to_column("Variable") %>%
  setNames(c("Variable", "Description"))
##Write intermediate CSV
write.csv(data00mSAS, "acasi00mSAS.csv", row.names = FALSE)
##Read intermediate CSV
data00mRaw <- read.csv("acasi00mSAS.csv", na.strings = c("", "NA"), 
                       stringsAsFactors = FALSE)
##Delete intermediate CSV
unlink("acasi00mSAS.csv")

#06m
data06mSAS <- import("Data merged across sites/acasi06m.sas7bdat")
##Save labels
labels06m <- get_label(data06mSAS) %>% 
  as.data.frame(., stringsAsFactors = FALSE) %>%
  rownames_to_column("Variable") %>%
  setNames(c("Variable", "Description"))
##Write intermediate CSV
write.csv(data06mSAS, "acasi06mSAS.csv", row.names = FALSE)
##Read intermediate CSV
data06mRaw <- read.csv("acasi06mSAS.csv", na.strings = c("", "NA"), 
                       stringsAsFactors = FALSE)
##Delete intermediate CSV
unlink("acasi06mSAS.csv")

#Read FRI PIDs (PID and identifiers differ between ACASI and MCD)
fri <- import("Data from sites/Friends_Research_Institute/Text Me Girl_PIDs.xlsx",
              col_names = FALSE) %>%
  setNames(c("SiteSpecificID", "PID", "Withdrawn")) %>%
  select(-Withdrawn) %>%
  mutate(SiteSpecificID = as.character(SiteSpecificID))

#MCD
SasNumToDate <- function (x) {
  x %>%
    ungroup() %>%
    mutate_at(vars(contains("ReportingPeriod"), contains("Date"), -OriginDate), 
              list(~as.Date(as.numeric(.), origin = "1960-01-01")))
}
fri_rekey <- function (x) {
  x %>%
  {
    bind_rows(
      filter(., SiteID != "FRI") %>%
        rename(PID = SiteSpecificID),
      filter(., SiteID == "FRI") %>%
        left_join(., fri, by = "SiteSpecificID") %>%
        select(-SiteSpecificID)
    )
  } %>%
    rename(SITE1 = SiteID)
}
#> Participant History
mcd_history <- read_csv("Data merged across sites/MCD/MCD_Participant_Summary_History_W0-W3_SASdates.csv") %>%
  SasNumToDate() %>%
  mutate(SiteSpecificID = replace(SiteSpecificID, 
                                  which(SiteID == "WUSL"), 
                                  str_pad(SiteSpecificID[which(SiteID == "WUSL")], 4, "left", "0"))) %>%
  fri_rekey() #Change FRI SiteSpecificIDs to match PIDs in ACASI surveys
  #> Lab Test Results
mcd_labTests <- read_csv("Data merged across sites/MCD/MCD_Lab_Test_Results_W0-W3_SASdates.csv") %>%
  SasNumToDate() %>%
  select(SiteID, SiteSpecificID, ServiceDate, ViralSupp) %>%
  filter(!is.na(ViralSupp)) %>%
  group_by(SiteID, SiteSpecificID) %>%
  summarize(ViralSupp = ViralSupp[which.min(ServiceDate)]) %>%
  mutate(SiteSpecificID = replace(SiteSpecificID, 
                                  which(SiteID == "WUSL"), 
                                  str_pad(SiteSpecificID[which(SiteID == "WUSL")], 4, "left", "0"))) %>%
  fri_rekey() #Change FRI SiteSpecificIDs to match PIDs in ACASI surveys
  #> Ambulatory Visits
mcd_ambVisits <- read_csv("Data merged across sites/MCD/MCD_Ambulatory_Visits_W0-W3_SASdates.csv",
                          col_types = cols(
                            SiteSpecificID = col_character()
                          )) %>%
  SasNumToDate() %>%
  select(SiteID, SiteSpecificID, ServiceDate) %>%
  mutate(SiteSpecificID = replace(SiteSpecificID, 
                       which(SiteID == "WUSL"), 
                       str_pad(SiteSpecificID[which(SiteID == "WUSL")], 4, "left", "0"))) %>%
  fri_rekey() #Change FRI SiteSpecificIDs to match PIDs in ACASI surveys
  
#####Clean and combine data
#Check for NAs, split NAs and complete cases into separate data friends
acasi00mNa <- data00mRaw %>%
  filter(is.na(MTUEX1))
acasi06mNa <- data06mRaw %>%
  filter(is.na(S56_1) | grepl("\\.\\.\\.\\.\\.", S56_2S))
acasi00m <- data00mRaw %>%
  filter(!is.na(MTUEX1)) %>%
  mutate_all(~replace(., list = grep("\\.{5}", .), values = NA))
acasi06m <- data06mRaw %>%
  filter(!(is.na(S56_1) | grepl("\\.\\.\\.\\.\\.", S56_2S))) %>%
  mutate_all(~replace(., list = grep("\\.{5}", .), values = NA))
#Check whether NA cases are repeated in rest of acasi
##First paste SITE1 and PID together, then check if in
which(do.call(paste, c(acasi00m[, c("SITE1", "PID")], sep = "_")) %in%
        do.call(paste, c(acasi00mNa[, c("SITE1", "PID")], sep = "_")))
which(do.call(paste, c(acasi06m[, c("SITE1", "PID")], sep = "_")) %in%
        do.call(paste, c(acasi06mNa[, c("SITE1", "PID")], sep = "_")))
##None of NA cases are found in rest of acasi

#Remove variables in 00m which have only missing values
varRemove00m <- acasi00m %>%
  summarize_all(~length(which(is.na(.)))) %>%
  gather("Variable", "NumNA") %>%
  filter(NumNA == nrow(acasi00m))
acasi00m <- acasi00m %>%
  select(colnames(acasi00m)[!colnames(acasi00m) %in% varRemove00m$Variable]) %>%
  #Correct AGE for WUSL 0020 from -1 to 19; no other AGE values calculated incorrectly
  mutate(AGE = replace(AGE, 
                       which(SITE1 == 10, PID == "0020", AGE == -1), 
                       19))
#Note that AGE1 and AGE2 that differ from AGE by more than 1, sometimes by upwards of 80

#Order columns in each dataframe alphabetically
#This serves to order sub-questions alphabetically
#Original SAS files have variables out of order
##00m
##Exclude SITE1, PID, T00M
##Create list of root names of variables
root00m <- c("ADAP", "AGE", "AIDSDIAG", "ALCOHOL", "AMPH", "ANTIA", "ART", 
              "ASE", "BORNHIV", "CARE", "CARL", "CD4", "CNEED", "DIAGHIV",
              "DISC", "DOB", "DRUG", "ELPTIME", "EMPLOY", "END_DATE", "GENDER",
              "GETH", "GRADE", "HE", "INFECTN", "INJEC", "INSCHOOL", "INSURE",
              "INTRVWER", "JAIL", "LANG", "LATINO", "LIVED", "MENTALH", "MONEY",
              "MTU", "PID", "PRAC", "RACE", "RDAT", "RDCR", "RDCR", "RELSTAT",
              "SCREEN", "SEXBRTH", "SEXWTH12", "SITE", "SSND", "SSUSE",
              "STAY7D", "STIGMA", "surveylanguage", "T00m", "TIME", "TODAY",
              "VERSION", "VIRAL")
##Separate root names from sub-questions/options (branches) and sort
names00m <- data.frame(Names = 
                         colnames(acasi00m)[which(!colnames(acasi00m) %in%
                                                    c("SITE1", "PID"))]) %>%
  mutate(Root = gsub(paste0("(", paste(root00m, collapse = "|"), ")(_)?(.*)"), 
                     "\\1",
                     Names),
         Branch = gsub(paste0("(", paste(root00m, collapse = "|"), ")(_)?(.*)"), 
                       "\\3",
                       Names),
         First = gsub("([[:alpha:]]+)(\\d*)([[:alpha:]]*)|(\\d+)([[:alpha:]]*)(\\d*)",
                      "\\1\\4",
                      Branch),
         Second = gsub("([[:alpha:]]+)(\\d*)([[:alpha:]]*)|(\\d+)([[:alpha:]]*)(\\d*)",
                       "\\2\\5", 
                       Branch),
         Third = gsub("([[:alpha:]]+)(\\d*)([[:alpha:]]*)|(\\d+)([[:alpha:]]*)(\\d*)",
                      "\\3\\6",
                      Branch)) %>%
  arrange(Root, First, Second, Third) %>%
  select(-Branch)
##Sort columns by root, etc.
acasi00m <- acasi00m %>%
  select(SITE1, PID, as.character(names00m$Names))

##06m
##Exclude SITE1, PID, T06M
##Create list of root names of variables
root06m <- c("ADAP", "ART", "CARE", "CD4LST", "CNEED", "DISC", "DRUG", "ELPTIME",
           "EMPLOY", "END_DATE", "FOLLOWUP", "GENDER", "INJEC", "INSURE", "INTRVWER",
           "LANG", "LIVED", "MENTAL", "MONEY", "ORIENT", "PIDCHECK", "PRAC", "RDAT", 
           "RDCR", "S56", "SEXBRTH", "SEXWTHF", "SITE", "SSND", "SSUSE", "STAY",
           "STIGMA", "surveylanguage", "T06m", "TIME", "TODAY", "VERSION", "VIRALLST")
##Separate root names from sub-questions/options (branches) and sort
names06m <- data.frame(Names = 
                         colnames(acasi06m)[which(!colnames(acasi06m) %in%
                                                    c("SITE1", "PID"))]) %>%
  mutate(Root = gsub(paste0("(", paste(root06m, collapse = "|"), ")(_)?(.*)"), 
                     "\\1",
                     Names),
         Branch = gsub(paste0("(", paste(root06m, collapse = "|"), ")(_)?(.*)"), 
                       "\\3",
                       Names),
         First = gsub("([[:alpha:]]+)(\\d*)([[:alpha:]]*)|(\\d+)([[:alpha:]]*)(\\d*)",
                      "\\1\\4",
                      Branch),
         Second = gsub("([[:alpha:]]+)(\\d*)([[:alpha:]]*)|(\\d+)([[:alpha:]]*)(\\d*)",
                       "\\2\\5", 
                       Branch),
         Third = gsub("([[:alpha:]]+)(\\d*)([[:alpha:]]*)|(\\d+)([[:alpha:]]*)(\\d*)",
                      "\\3\\6",
                      Branch)) %>%
  arrange(Root, First, Second, Third) %>%
  select(-Branch)
##Sort columns by root, etc.
acasi06m <- acasi06m %>%
  select(SITE1, PID, as.character(names06m$Names))

#Consolidate answers from different versions of 06M survey 
#  (differences in S56_24 and S56_24X series)
##Survey1: no S56_24X series, all options were included in one screen; 
##  S56_24 includes additional variables: s56_24L:S
acasi06mSurvey1 <- acasi06m %>%
  filter(!is.na(S56_24L)) %>%
  mutate(S56_24XL = S56_24L,
         S56_24XM = S56_24M,
         S56_24XN = S56_24N,
         S56_24XO = S56_24O,
         S56_24XP = S56_24P,
         S56_24XQ = S56_24Q,
         S56_24XR = S56_24R,
         S56_24XS = S56_24S)
##Survey2: newer version, contains S56_24X series, but S56_24U, S56_24X, 
##  and S56_24XV are NA
##Lacks S56_24L:S56_24R, S56_24S
acasi06mSurvey2 <- acasi06m %>% 
  filter(is.na(S56_24L) & is.na(S56_24X)) %>%
  bind_rows(acasi06mSurvey1) %>%
  mutate(S56_24Sum = rowSums(select(., S56_24A:S56_24K), na.rm = TRUE),
         S56_24U = if_else(S56_23 == 7, 9, if_else(S56_24Sum > 0, 0, 1)),
         S56_24XSum = rowSums(select(., S56_24XL:S56_24XS), na.rm = TRUE),
         S56_24XV = if_else(S56_23 == 7, 9, if_else(S56_24XSum > 0, 0, 1))) %>%
  mutate(S56_24 = if_else(S56_24U == 1, 1, S56_24Sum),
         S56_24X = if_else(S56_24XV == 1, 1, S56_24XSum)) %>%
  select(-S56_24Sum, -S56_24XSum)
##Survey3: latest version, contains all of
##  S56_24X series, S56_24U, S56_24X, S56_24XV
##Lacks S56_24L:S56_24R, S56_24S; this will remove these columns
acasi06mSurvey3 <- acasi06m %>%
  filter(is.na(S56_24L) & !is.na(S56_24X)) %>%
  bind_rows(acasi06mSurvey2) %>%
  select(-S56_24L:-S56_24R, -S56_24S)

#####Remove unnecessary variables
#Remove 00m variables
acasi00mTrim <- acasi00m %>%
  select(SITE1, PID, TODAY,
         surveylanguage, INTRVWER, SITE, #SES: Survey
         SCREEN1, starts_with("AGE"), starts_with("DOB"), #SES: Age
         LATINO, starts_with("RACE"), #SES: Race
         SEXBRTH,
         starts_with("GENDER"), #SES: Sex/Gender
         starts_with("ORIENT"), #SES: Orientation
         INSCHOOL, GRADE, #SES: Education
         MONEY, #SES: Employment
         starts_with("EMPLOY"), #SES: Employment
         RELSTAT, #SES: Relationship
         starts_with("LIVED"), starts_with("STAY7"), #Housing
         BORNHIV, DIAGHIV, SCREEN5, #Length with HIV: First HIV Diagnosis
         starts_with("INSURE"), ADAP, #Healthcare utilization: Insurance
         matches("CARE[[:alpha:]]6"), #Healthcare utilization: Recent care
         CARELHIV, starts_with("CAREHV"), #Healthcare utilization: Retention in care
         ARTNOW, #Healthcare utilization: Treatment
         ARTADHR, #Healthcare utilization: Adherence
         starts_with("HE"), #Youth Health Engagement scale
         matches("CARE\\d{2}"), #Provider Empathy (CARE) scale
         starts_with("DISC"), #Disclosure
         starts_with("STIGMA"), #HIV-related stigma
         starts_with("MENTALH"), #Mental health
         matches("DRUG\\dL"), matches("DRUG\\d6"), #Substance use: non-injected
         INJECTL, #Substance use: injected
         starts_with("SOCIALS"), #Social support
         #Media Technology Usage and Attitudes Scale
         starts_with("MTUEX"),
         starts_with("MTUSPX"),
         starts_with("MTUIX"),
         starts_with("MTUSNX"),
         starts_with("MTUAX")) 

#Remove 06m variables
names06mS56 <- names06m %>%
  filter(Names %in% colnames(acasi06mSurvey3)) %>%
  filter(Root == "S56") %>%
  mutate(First = as.numeric(First)) %>%
  arrange(Root, First, Second, Third)
names06mTrim <- c(
  colnames(acasi00mTrim)[colnames(acasi00mTrim) %in% colnames(acasi06mSurvey3)],
  as.character(names06mS56$Names)
)

acasi06mTrim <- acasi06mSurvey3 %>%
    select(names06mTrim)

#####Combine data from 00m and 06m
acasiJoinInner <- inner_join(acasi00mTrim, 
                             acasi06mTrim %>%
                               select(SITE1, PID, starts_with("S56")),
                             by = c("SITE1", "PID")) %>%
  mutate(SITE1 = fct_recode(as.factor(SITE1),
                            "CBW" = "1", "FRI" = "2", "NYSDA" = "3", 
                            "HBHC" = "4", "MHS"  = "5", "PSU" = "6", 
                            "PFC" = "7", "SFDPH" = "8", "WFU"  = "9", 
                            "WUSL" = "10"),
         SITE1 = as.character(SITE1))
  
acasiJoin00m <- anti_join(acasi00mTrim, acasi06mTrim, by = c("SITE1", "PID"))
acasiJoin06m <- anti_join(acasi06mTrim, acasi00m, by = c("SITE1", "PID"))

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

#####Creation of new variables
#####Collapse existing demographic variables and create scales

acasi <- acasiJoinInner %>%
  arrange(SITE1) %>%
  mutate(SITE1 = as.character(SITE1)) %>%
  filter(AGE >= 18) %>%
  #Add MCD variables
  left_join(., 
            mcd_history %>%
              select(SITE1, PID, HIVDiagnosisYear) %>%
              rename(HIVDiagnosisYear_MCD = HIVDiagnosisYear), 
            by = c("SITE1", "PID")) %>%
  left_join(., 
            mcd_labTests %>%
              rename(ViralSupp_MCD = ViralSupp), 
            by = c("SITE1", "PID")) %>%
  left_join(., 
            left_join(acasiJoinInner %>% #Create binary variable from MCD to determine if there was an ambulatory visit within 6 months
                        select(SITE1, PID, TODAY),
                      mcd_ambVisits,
                      by = c("SITE1", "PID")) %>%
              mutate(TODAY = as.Date(TODAY, origin = "1960-01-01"),
                     DIFF = TODAY - ServiceDate,
                     CAREHV06_MCD = if_else(TODAY - ServiceDate <= 183 &
                                                 TODAY - ServiceDate >= 0, 1, 0)) %>%
              group_by(SITE1, PID) %>%
              summarize(CAREHV06_MCD = if_else(any(CAREHV06_MCD == 1), 1, 0)), 
            by = c("SITE1", "PID")) %>%
  #Add recodes for participants who marked 'Other'
  left_join(., acasiOther, by = c("SITE1", "PID")) %>%
  mutate(
    #Variable transformations
    #> Procedure:
    ##> 1) Create (if necessary) and re-level factors, collapse levels into fewer options (RC = Recode)
    ##> 2) Create dummy variables (RCD = Recode Dummy)
    ###>     Treat 'Refused to answer' as separate answer rather than NA
    ##> 3) Change 'Refused to answer' or 'Skipped' to NA for continuous variables
    ##> 4) Make other transformations to continuous variables
    
    #> Site
    SITE_RC = fct_recode(as.factor(SITE1),
                         "Corpus Christi" = "CBW", "Los Angeles" = "FRI", 
                         "New York" = "NYSDA", "Chicago" = "HBHC", 
                         "Cleveland"  = "MHS", "Hershey" = "PSU", 
                         "Philadelphia" = "PFC", "San Francisco" = "SFDPH", 
                         "Winston-Salem"  = "WFU", "St. Louis" = "WUSL"),
    SITE_RCD_FRI   = if_else(SITE1 == "FRI", 1, 0),
    SITE_RCD_NYSDA = if_else(SITE1 == "NYSDA", 1, 0),
    SITE_RCD_HBHC  = if_else(SITE1 == "HBHC", 1, 0),
    SITE_RCD_MHS   = if_else(SITE1 == "MHS", 1, 0),
    SITE_RCD_PSU   = if_else(SITE1 == "PSU", 1, 0),
    SITE_RCD_PFC   = if_else(SITE1 == "PFC", 1, 0),
    SITE_RCD_SFDPH = if_else(SITE1 == "SFDPH", 1, 0),
    SITE_RCD_WFU   = if_else(SITE1 == "WFU", 1, 0),
    SITE_RCD_WUSL  = if_else(SITE1 == "WUSL", 1, 0),
    #> Survey language
    surveylanguage_RCD_Eng = if_else(surveylanguage == "English", 1, 0),
    #> Ethnicity & Race
    RACE_RC = case_when(LATINO == 1 ~ "Latino",
                        RACEC == 1 ~ "Black, Not Latino",
                        RACEE == 1 & RACE > 1 ~ 
                          "White Mixed-Race, Not Latino or Black",
                        RACEE == 1 ~ "White, Not Latino",
                        LATINO == 8 & RACE == 8 ~ "Refuse to answer", #None refused to answer
                        !is.na(RACERECODE) ~ RACERECODE,
                        TRUE ~ "Other race"),
    RACE_RCD_Latino   = if_else(RACE_RC == "Latino", 1, 0),
    RACE_RCD_Black    = if_else(RACE_RC == "Black, Not Latino", 1, 0),
    RACE_RCD_WhiteMix = if_else(RACE_RC == "White Mixed-Race, Not Latino or Black", 1, 0),
    RACE_RCD_Other    = if_else(RACE_RC == "Other race", 1, 0),
    RACE_RCD_Missing  = if_else(RACE_RC == "Refuse to answer", 1, 0),
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
    GENDER_RCD_Female  = if_else(GENDER_RC == "Female (cis woman)", 1, 0),
    GENDER_RCD_Trans   = if_else(GENDER_RC == "Trans-identified", 1, 0),
    GENDER_RCD_Other   = if_else(GENDER_RC == "Other gender", 1, 0),
    GENDER_RCD_Missing = if_else(GENDER_RC == "Refuse to answer", 1, 0),
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
    ORIENT_RCD_Gay     = if_else(ORIENT_RC == "Gay or lesbian", 1, 0),
    ORIENT_RCD_Bi      = if_else(ORIENT_RC == "Bisexual", 1, 0),
    ORIENT_RCD_Other   = if_else(ORIENT_RC == "Other orientation", 1, 0),
    ORIENT_RCD_Missing = if_else(ORIENT_RC == "Refuse to answer", 1, 0),
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
    GRADE_RCD_PostK   = if_else(GRADE_RC == "Some post-K12", 1, 0),
    GRADE_RCD_Grad    = if_else(GRADE_RC == "College graduate or trade certification", 1, 0),
    GRADE_RCD_Missing = if_else(GRADE_RC == "Refuse to answer", 1, 0),
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
    STAY7D_RCD_Stable      = if_else(STAY7D_RC == "Stable housing", 1, 0),
    STAY7D_RCD_Institution = if_else(STAY7D_RC == "Institution", 1, 0),
    STAY7D_RCD_Missing     = if_else(STAY7D_RC == "Refuse to answer", 1, 0),
    #> HIV History
    DIAGHIV_RC = case_when(DIAGHIV <= 2019 ~ DIAGHIV),
    TIMESINCEHIV = year(TODAY) - DIAGHIV_RC, #Does not include those born with HIV
    BORNHIV_MCD = if_else(DOBY == HIVDiagnosisYear_MCD, 1, 0),
    TIMESINCEHIV_MCD = year(TODAY) - HIVDiagnosisYear_MCD, #Includes those born with HIV
    #> Insurance
    INSURE_RC = case_when(INSUREA == 1 ~ "Not insured",
                          INSURE == 97 ~ "Don't know",
                          INSURE == 98 ~ "Refuse to answer",
                          INSURE == 99 ~ "Skipped", #None refused to answer
                          INSUREB == 1 | INSUREC == 1 | INSURED == 1 |
                            INSUREE == 1 | INSUREF == 1 | INSUREG == 1 ~ "Insured",
                          TRUE ~ INSURERECODE),
    INSURE_RCD_Insured = if_else(INSURE_RC == "Insured", 1, 0),
    INSURE_RCD_Unknown = if_else(INSURE_RC == "Don't know", 1, 0),
    INSURE_RCD_Missing = if_else(INSURE_RC == "Refuse to answer" |
                                   INSURE_RC == "Skipped", 1, 0),
    #> Medical Care
    CARED6_RCD_Yes     = if_else(CARED6 > 0 & CARED6 < 998, 1, 0),
    CARED6_RCD_Missing = if_else(CARED6 >= 998, 1, 0),
    CAREHV06_RC = case_when(CARELHIV == 1 ~ CAREHV06,
                            CARELHIV == 0 ~ 0L,
                            CARELHIV == 8 ~ 998L),
    # CAREHV06_RCD_Yes = if_else(CAREHV06 > 0 & CAREHV06 <= 99, 1, 0),
    # CAREHV06_RCD_Missing = if_else(CAREHV06 == 998 | CAREHV06 == 999, 1, 0),
    CAREHV06_MCD_RCD_Yes = if_else(!is.na(CAREHV06_MCD) &
                                     CAREHV06_MCD > 0, 1, 0),
    CAREHV06_MCD_RCD_Missing = if_else(is.na(CAREHV06_MCD), 1, 0),
    #> ART Usage
    ARTNOW_RCD_Yes     = if_else(ARTNOW == 1, 1, 0),
    ARTNOW_RCD_Missing = if_else(ARTNOW == 8 | ARTNOW == 9, 1, 0),
    ARTADHR_RC = fct_recode(as.factor(ARTADHR),
                            "Negative" = "1",
                            "Negative" = "2",
                            "Neutral"  = "3",
                            "Positive" = "4",
                            "Positive" = "5",
                            "Positive" = "6",
                            "Missing"  = "8", #None refused
                            "Missing"  = "9"), #Many skipped
    ARTADHR_RCD_Neutral  = if_else(ARTADHR_RC == "Neutral", 1, 0),
    ARTADHR_RCD_Positive = if_else(ARTADHR_RC == "Positive", 1, 0),
    ARTADHR_RCD_Missing  = if_else(ARTADHR_RC == "Missing", 1, 0),
    #> HIV Disclosure
    DISC_RCD_Partner = if_else(DISCB == 1 | DISCC == 1, 1, 0),
    DISC_RCD_Family  = if_else(DISCD == 1 | DISCE == 1, 1, 0),
    DISC_RCD_Other   = if_else(DISCF == 1 | DISCG == 1 |
                                 DISCH == 1 | DISCI == 1 |
                                 DISCJ == 1, 1, 0),
    DISC_RCD_Missing = if_else(!DISC %in% c(1:10), 1, 0),
    #> Substance Use
    DRUG_RCD_Alcohol    = if_else(DRUG1LA == 1, 1, 0),
    DRUG_RCD_Tobacco    = if_else(DRUG1LB == 1, 1, 0),
    DRUG_RCD_Marijuana  = if_else(DRUG1LC == 1, 1, 0),
    DRUG_RCD_Other      = if_else(
      rowSums(
        select(.,
               one_of(paste0("DRUG1L", LETTERS[4:12]),
                      paste0("DRUG2L", LETTERS[1:10]))               
        ) == 1) > 0, 1, 0),
    DRUG_RCD_Missing    = if_else(DRUG1L == 98 & DRUG2L == 98, 1, 0),
    INJECTL_RCD_Inject  = if_else(INJECTL == 1, 1, 0),
    INJECTL_RCD_Missing = if_else(!INJECTL %in% c(1, 0), 1, 0)
  ) %>%
    
  #Scales
  #>Procedure:
  ##> 1) Convert any values not included in the item to NA ("Refuse to answer", "Don't know", or "Skipped")
  ###>     Each variable coded as integers; values above certain integer are generally refuse, don't know, skip
  ##> 2) Check whether all items are NA (Does number of NA = number of items?)
  ##> 3) If not, sum all items (ignoring NA values: na.rm = TRUE)
  
  #> Social support, 3 items
  mutate_at(vars(matches("SOCIALS\\d{1}")),
            list(RC = ~replace(., which(. > 10), NA))) %>% #Values of 0-10 expected; above that = Refuse/Skip
  mutate(
    SOCIALS_RC = case_when(
      rowSums(is.na(select(., matches("SOCIALS\\d{1}_RC")))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., matches("SOCIALS\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> HIV-related Stigma, 10 items
  mutate_at(vars(matches("STIGMA\\d{1}")),
            list(RC = ~replace(., which(. > 4), NA))) %>% #Values of 0-4 expected; above that = Refuse
  mutate(
    STIGMA_RC = case_when(
      rowSums(is.na(select(., matches("STIGMA\\d+_RC")))) < 10 ~ #Is number of NA < number of items?
        rowSums(select(., matches("STIGMA\\d+_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Youth Health Engagement, 10 items
  ##> Health Access Literacy (HAL): HE01-HE05; exclude HE05 because all but 17 participants skipped (17 under age of 18)
  ##> Health Self-Efficacy (HSE): HE06-HE10
  mutate_at(vars(starts_with("HE"), -HE05),
            list(RC = ~replace(., which(. > 4), NA))) %>% #Values of 1-4 expected; 7 = Don't Know, 8 = Refuse, 9 = Skip
  mutate(
    HE_RC_HAL = case_when(
      rowSums(is.na(select(., one_of(paste0("HE0", 1:4, "_RC"))))) < 4 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("HE0", 1:4, "_RC"))), na.rm = TRUE) #If so, sum columns
    ),
    HE_RC_HSE = case_when(
      rowSums(is.na(select(., one_of(c(paste0("HE0", 6:9, "_RC"), "HE10_RC"))))) < 5 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(c(paste0("HE0", 6:9, "_RC"), "HE10_RC"))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Provider Empathy (CARE), 10 items
  mutate_at(vars(matches("CARE\\d{2}")),
            list(RC = ~replace(., which(. > 5), NA))) %>% #Values of 1-5 expected; 8 = refuse, 9 = "Not Applicable"
  mutate(
    CARE_RC = case_when(
      rowSums(is.na(select(., matches("CARE\\d{2}_RC")))) < 10 ~ #Is number of NA < number of items?
        rowSums(select(., matches("CARE\\d{2}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Physical and Mental Health, 4 items
  ##> Exclude MENTALH4 because it differs from MENTALH1-3
  mutate_at(vars(starts_with("MENTALH")),
            list(RC = ~replace(., which(. > 6), NA))) %>% #Values of 1-6 expected; 8 = refuse to answer
  mutate(MENTALH3_RC = 7 - MENTALH3_RC) %>% #Reverse code MENTALH3 because it is negatively correlated with MENTALH1/2
  mutate(
    MENTALH_RC = case_when(
      rowSums(is.na(select(., matches("(MENTALH)(1|2|3)(_RC)")))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., matches("(MENTALH)(1|2|3)(_RC)")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Email Usage, 4 items
  mutate_at(vars(starts_with("MTUEX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUEX_RC = case_when(
      rowSums(is.na(select(., matches("MTUEX\\d{1}_RC")))) < 4 ~ #Is number of NA < number of items?
        rowSums(select(., matches("MTUEX\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Text Usage, 3 items (MTUSPX01, MTUSPX02, MTUSPX12)
  mutate_at(vars(starts_with("MTUSPX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUSPX_RC_Text = case_when(
      rowSums(is.na(select(., MTUSPX01_RC, MTUSPX02_RC, MTUSPX12_RC))) < 3 ~
        rowSums(select(., MTUSPX01_RC, MTUSPX02_RC, MTUSPX12_RC))
    )
  ) %>%
  #> Mobile Phone Usage, 9 items (MTUSPX03 through MTUSPX11)
  mutate(
    MTUSPX_RC_Smartphone = case_when(
      rowSums(is.na(select(., one_of(paste0("MTUSPX",
                                            str_pad(3:11, width = 2, pad = 0),
                                            "_RC"
      ))))) < 9 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("MTUSPX",
                                        str_pad(3:11, width = 2, pad = 0),
                                        "_RC"
        ))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Internet Search, 4 items (MTUIX5 and MTUIX6 excluded from scale: added for this study, not part of original subscale)
  mutate_at(vars(starts_with("MTUIX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUIX_RC = case_when(
      rowSums(is.na(select(., one_of(paste0("MTUIX", c(1:4), "_RC"))))) < 4 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("MTUIX", c(1:4), "_RC"))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> General Social Media Usage, 9 items (Excluded MTUSNX10:MTUSNX12; added for this study, not part of original subscale)
  mutate_at(vars(starts_with("MTUSNX")),
            list(RC = ~replace(., which(. == 99), 0))) %>% #99 = skipped; change 99 to 0 as those skipped indicated not using social media
  mutate_at(vars(matches("MTUSNX\\d+_RC")),
            list(~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUSNX_RC = case_when(
      rowSums(is.na(select(., one_of(paste0("MTUSNX0", 1:9, "_RC"))))) < 9 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("MTUSNX0", 1:9, "_RC"))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Positive Attitudes Toward Technology, 6 items (MTUAX01, MTUAX03, MTUAX04, MTUAX09:MTUAX11)
  mutate_at(vars(matches("MTUAX\\d{2}")),
            list(RC = ~replace(., which(. > 5), NA))) %>% #Values of 0-5 expected; 8 = refuse to answer; 9 = skipped (none)
  mutate(
    MTUAX_RC_Pos = case_when(
      rowSums(is.na(select(., one_of(
        paste0("MTUAX", str_pad(c(1, 3:4, 9:11), width = 2, pad = 0), "_RC")
      )))) < 6 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(
          paste0("MTUAX", str_pad(c(1, 3:4, 9:11), width = 2, pad = 0), "_RC")
        )),
        na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Anxiety About Being Without Technology or Dependence on Technology, 3 items (MTUAX05, MTUAX06, MTUAX08)
  mutate(
    MTUAX_RC_Anx = case_when(
      rowSums(is.na(select(., one_of(
        paste0("MTUAX", str_pad(c(5:6, 8), width = 2, pad = 0), "_RC")
      )))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(
          paste0("MTUAX", str_pad(c(5:6, 8), width = 2, pad = 0), "_RC")
        )),
        na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Negative Attitudes Toward Technology, 3 items (MTUAX12:MTUAX14)
  mutate(
    MTUAX_RC_Neg = case_when(
      rowSums(is.na(select(., one_of(
        paste0("MTUAX", str_pad(12:14, width = 2, pad = 0), "_RC")
      )))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(
          paste0("MTUAX", str_pad(12:14, width = 2, pad = 0), "_RC")
        )),
        na.rm = TRUE) #If so, sum columns
    )
  ) %>%

  #Outcomes
  mutate(
    #> Internet Searches for Sexual Health (Lifetime)
    outcome_Search_SexHealth = if_else(
        rowSums(
          select(., S56_25B, S56_25C, S56_25D, S56_25E, S56_25F, S56_25G) == 1
        ) > 0, 1, 0
      ),
    #> Internet Searches for General Health (Lifetime)
    outcome_Search_GenHealth = if_else(
      rowSums(
        select(., S56_25A, S56_25H, S56_25I, S56_25J, S56_25K, S56_25L) == 1
      ) > 0, 1, 0
    ),
    #> Communication about Sexual Health
    outcome_Comms_SexHealth = if_else(
      rowSums(select(., 
                     paste0("S56_6", c("K", "M")),
                     paste0("S56_9", c("K", "M")),
                     paste0("S56_15", c("K", "M")),
                     paste0("S56_18", c("K", "M"))) == 1) > 0, 1, 0
    )
  )

##### Create a data set for analysis excluding original variables
acasi_analysis <- acasi %>%
  select(PID,
         SITE_RCD_FRI, SITE_RCD_NYSDA, SITE_RCD_HBHC, SITE_RCD_MHS,
         SITE_RCD_PFC, SITE_RCD_PSU, SITE_RCD_SFDPH, SITE_RCD_WFU, SITE_RCD_WUSL, #Site
         surveylanguage_RCD_Eng,
         AGE, #Age
         RACE_RCD_Latino, RACE_RCD_Black, RACE_RCD_WhiteMix, 
         RACE_RCD_Other, RACE_RCD_Missing, #Ethnicity & Race
         GENDER_RCD_Female, GENDER_RCD_Trans, 
         GENDER_RCD_Other, GENDER_RCD_Missing, #Gender
         ORIENT_RCD_Gay, ORIENT_RCD_Bi, ORIENT_RCD_Other, #Orientation
         GRADE_RCD_PostK, GRADE_RCD_Grad, #Education
         MONEY_RC_Log, #Income
         STAY7D_RCD_Stable, STAY7D_RCD_Institution, STAY7D_RCD_Missing, #Housing
         BORNHIV, TIMESINCEHIV,
         BORNHIV_MCD, TIMESINCEHIV_MCD,
         ViralSupp_MCD,
         INSURE_RCD_Insured, INSURE_RCD_Unknown, INSURE_RCD_Missing, #Healthcare utilization: Insurance
         CARED6_RCD_Yes, CARED6_RCD_Missing, #Healthcare utilization: Recent care
         CAREHV06_MCD_RCD_Yes, CAREHV06_MCD_RCD_Missing,
         ARTNOW_RCD_Yes, ARTNOW_RCD_Missing, #Healthcare utilization: Treatment
         ARTADHR_RCD_Neutral, ARTADHR_RCD_Positive, ARTADHR_RCD_Missing, #Healthcare utilization: Adherence
         HE_RC_HAL, HE_RC_HSE, #Youth Health Engagement scale
         CARE_RC, #Provider Empathy (CARE) scale
         STIGMA_RC, #HIV-related stigma
         DISC_RCD_Partner, DISC_RCD_Family, DISC_RCD_Other, DISC_RCD_Missing, #Disclosure
         MENTALH_RC, MENTALH4_RC,#Mental health
         DRUG_RCD_Alcohol, DRUG_RCD_Tobacco, DRUG_RCD_Marijuana, DRUG_RCD_Other, 
         DRUG_RCD_Missing, #Substance use: non-injected
         INJECTL_RCD_Inject, INJECTL_RCD_Missing, #Substance use: injected
         SOCIALS_RC, #Social support
         #Media Technology Usage and Attitudes Scale
         MTUEX_RC, 
         MTUSPX_RC_Text, 
         MTUSPX_RC_Smartphone, 
         MTUIX_RC, MTUIX5_RC, MTUIX6_RC,
         MTUSNX_RC,
         MTUAX_RC_Pos, MTUAX_RC_Anx, MTUAX_RC_Neg, MTUAX02_RC, MTUAX07_RC,
         starts_with("outcome")
  ) %>%
  select_if(~length(which(. == 0)) < length(.))

#Insurance 'Other' included as 'Insured' until further notice.

save(acasi, acasi_analysis, acasiJoin00m, acasiJoin06m, acasiJoinInner,
     file = "Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/acasi.RData")


