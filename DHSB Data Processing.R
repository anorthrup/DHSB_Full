#####Digital Health-Seeking Behaviors
#####> Data Processing and Variable Transformations

#####Read libraries
library(tidyverse)
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
  fri_rekey()
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
  fri_rekey()
  #> Ambulatory Visits
mcd_ambVisits <- read_csv("Data merged across sites/MCD/MCD_Ambulatory_Visits_W0-W3_SASdates.csv") %>%
  SasNumToDate() %>%
  select(SiteID, SiteSpecificID, ServiceDate) %>%
  mutate(SiteSpecificID = replace(SiteSpecificID, 
                       which(SiteID == "WUSL"), 
                       str_pad(SiteSpecificID[which(SiteID == "WUSL")], 4, "left", "0"))) %>%
  fri_rekey()
  
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
         # SEXBRTH, #SES: Sex/Gender #> Removed
         starts_with("GENDER"), #SES: Sex/Gender
         LATINO, starts_with("RACE"), #SES: Race
         INSCHOOL, GRADE, #SES: Education
         MONEY, #SES: Employment
         starts_with("EMPLOY"), #SES: Employment
         RELSTAT, #SES: Relationship
         starts_with("ORIENT"), #SES: Orientation
         starts_with("LIVED"), starts_with("STAY7"), #Housing
         JAILL, #JAILLX, JAIL6X, #Incarceration 
         BORNHIV, DIAGHIV, SCREEN5, #Length with HIV: First HIV Diagnosis
         matches("CARE[[:alpha:]]6"), #Healthcare utilization: Recent care
         CARELHIV, CARLHIVA, CD4FST, VIRALFST, #Healthcare utilization: Engagement in care
         starts_with("CAREHV"), #Healthcare utilization: Retention in care
         ends_with("LST"), CD4LOW, INFECTN, AIDSDIAG, #Healthcare utilization: Quality of care
         ARTPRESC, ARTL, ARTLAGE, ARTREC, ARTNOW, #Healthcare utilization: Treatment
         ARTADHR, #Healthcare utilization: Adherence
         starts_with("INSURE"), ADAP, #Healthcare utilization: Insurance
         starts_with("HE"), #Youth Health Engagement scale
         matches("CARE\\d{2}"), #Provider Empathy (CARE) scale
         starts_with("SSND"), starts_with("SSUSE"), #Support services needed and used ###
         starts_with("CNEED"), -CNEED3ES, -CNEED4, -CNEED5, CNEED3ES, CNEED4, CNEED5, #Competing needs ###
         starts_with("DISC"), #Disclosure
         starts_with("STIGMA"), #HIV-related stigma
         starts_with("MENTALH"), #Mental health
         matches("DRUG\\dL"), matches("DRUG\\d6"), #Substance use: non-injected
         INJECTL, INJECT6, starts_with("INJEC6X"), #Substance use: injected
         starts_with("SOCIALS"), #Social support
         #Media Technology Usage and Attitudes Scale
         starts_with("MTUEX"),
         starts_with("MTUSPX"),
         starts_with("MTUIX"),
         starts_with("MTUSN"), -MTUSNJS, -starts_with("MTUSNX"), MTUSNJS, starts_with("MTUSNX"),
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

#####Creation of new variables
#####Collapse existing demographic variables and create scales

acasi <- acasiJoinInner %>%
  arrange(SITE1) %>%
  mutate(SITE1 = as.character(SITE1)) %>%
  filter(AGE >= 18) %>%
  #Add MCD
  left_join(., 
            mcd_history %>%
              select(SITE1, PID, HIVDiagnosisYear), 
            by = c("SITE1", "PID")) %>%
  left_join(., mcd_labTests, by = c("SITE1", "PID")) %>%
  left_join(., 
            left_join(acasiJoinInner %>% #Create binary variable from MCD to determine if there was an ambulatory visit within 6 months
                        select(SITE1, PID, TODAY),
                      mcd_ambVisits,
                      by = c("SITE1", "PID")) %>%
              mutate(TODAY = as.Date(TODAY, origin = "1960-01-01"),
                     DIFF = TODAY - ServiceDate,
                     CAREHV06_RC_MCD = if_else(TODAY - ServiceDate <= 183 &
                                                 TODAY - ServiceDate >= 0, 1, 0)) %>%
              group_by(SITE1, PID) %>%
              summarize(CAREHV06_RC_MCD = if_else(any(CAREHV06_RC_MCD == 1), 1, 0)), 
            by = c("SITE1", "PID")) %>%
  mutate(
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
    #Variable transformations
    #> Procedure:
    ##> 1) Create (if necessary) and re-level factors, collapse levels into fewer options (RC = Recode)
    ##> 2) Create dummy variables (RCD = Recode Dummy)
    ###>     Treat 'Refused to answer' as separate answer rather than NA
    ##> 3) Change 'Refused to answer' or 'Skipped' to NA for continuous variables
    ##> 4) Make other transformations to continuous variables
    
    #> Gender Identity
    GENDER_RC = fct_recode(as.factor(GENDER),
                           "Male (cis man)"     = "1", 
                           "Female (cis woman)" = "2",
                           "Trans-identified"   = "3", 
                           "Trans-identified"   = "4",
                           "Other gender"       = "5", 
                           "Other gender"       = "6",
                           "Refuse to answer"   = "8"), #None refused to answer
    GENDER_RCD_Female = if_else(GENDER_RC == "Female (cis woman)", 1, 0),
    GENDER_RCD_Trans  = if_else(GENDER_RC == "Trans-identified", 1, 0),
    GENDER_RCD_Other  = if_else(GENDER_RC == "Other gender", 1, 0),
    #> Sexual Orientation
    ORIENT_RC = fct_recode(as.factor(ORIENT),
                           "Straight"          = "1", 
                           "Gay or lesbian"    = "2", 
                           "Bisexual"          = "3",
                           "Other orientation" = "4", 
                           "Other orientation" = "5", 
                           "Other orientation" = "7",
                           "Refuse to answer"  = "8"), #None refused to answer
    ORIENT_RCD_Gay   = if_else(ORIENT_RC == "Gay or lesbian", 1, 0),
    ORIENT_RCD_Bi    = if_else(ORIENT_RC == "Bisexual", 1, 0),
    ORIENT_RCD_Other = if_else(ORIENT_RC == "Other orientation", 1, 0),
    #> Relationship Status
    RELSTAT_RC = fct_recode(as.factor(RELSTAT),
                            "Single"            = "1",
                            "Dating"            = "2",
                            "Dating"            = "3",
                            "Partnered/Married" = "4",
                            "Partnered/Married" = "5",
                            "Other status"      = "6",
                            "Refuse to answer"  = "8"), #None refused to answer
    RELSTAT_RCD_Dating    = if_else(RELSTAT_RC == "Dating", 1, 0),
    RELSTAT_RCD_Partnered = if_else(RELSTAT_RC == "Partnered/Married", 1, 0),
    RELSTAT_RCD_Other     = if_else(RELSTAT_RC == "Other status", 1, 0),
    #> Education
    INSCHOOL_RCD = if_else(INSCHOOL == 1, 1, 0), #None refused to answer
    GRADE_RC = fct_recode(as.factor(GRADE),
                          "High school, equivalent or less"         = "1", 
                          "High school, equivalent or less"         = "2", 
                          "High school, equivalent or less"         = "3", 
                          "Some post-K12"                           = "4", 
                          "College graduate or trade certification" = "5", 
                          "College graduate or trade certification" = "6",
                          "College graduate or trade certification" = "7", 
                          "Refuse to answer"                        = "8"), #None refused to answer
    GRADE_RCD_PostK = if_else(GRADE_RC == "Some post-K12", 1, 0),
    GRADE_RCD_Grad  = if_else(GRADE_RC == "College graduate or trade certification", 1, 0),
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
                           "Institution"      =  "9", 
                           "Institution"      = "10",
                           "Other residence"  = "11", 
                           "Refuse to answer" = "12"),
    STAY7D_RCD_Stable      = if_else(STAY7D_RC == "Stable housing", 1, 0),
    STAY7D_RCD_Institution = if_else(STAY7D_RC == "Institution", 1, 0),
    STAY7D_RCD_Other       = if_else(STAY7D_RC == "Other residence", 1, 0),
    STAY7D_RCD_Refuse      = if_else(STAY7D_RC == "Refuse to answer", 1, 0),
    #> Ethnicity & Race
    RACE_RC = case_when(LATINO == 1 ~ "Latino",
                        RACEC == 1 ~ "Black, Not Latino",
                        RACEE == 1 & RACE > 1 ~ 
                          "White Mixed-Race, Not Latino or Black",
                        RACEE == 1 ~ "White, Not Latino",
                        LATINO == 8 & RACE == 8 ~ "Refuse to answer", #None refused to answer
                        TRUE ~ "Other race"),
    RACE_RCD_Latino   = if_else(RACE_RC == "Latino", 1, 0),
    RACE_RCD_Black    = if_else(RACE_RC == "Black, Not Latino", 1, 0),
    RACE_RCD_WhiteMix = if_else(RACE_RC == "White Mixed-Race, Not Latino or Black", 1, 0),
    RACE_RCD_Other    = if_else(RACE_RC == "Other race", 1, 0),
    #> Employment
    EMPLOY_RC = case_when(EMPLOYA == 1 ~ "Employed/Student",
                          EMPLOYB == 1 ~ "Employed/Student",
                          EMPLOYC == 1 ~ "Employed/Student",
                          EMPLOYD == 1 ~ "Unemployed/Disabled",
                          EMPLOYE == 1 ~ "Unemployed/Disabled",
                          EMPLOYF == 1 ~ "Unemployed/Disabled",
                          EMPLOY == 8 ~ "Refuse to answer"), #None refused to answer
    EMPLOY_RCD_Employed = if_else(EMPLOY_RC == "Employed/Student", 1, 0),
    #> Income
    MONEY_RC = ifelse(MONEY %in% c(99997, 99998), NA, MONEY),
    MONEY_RC_Log = log(MONEY_RC + 1),
    #> HIV History
    BORNHIV_RCD = if_else(BORNHIV == 1, 1, 0), #None refused to answer
    DIAGHIV_RC = case_when(DIAGHIV <= 2019 ~ DIAGHIV),
    #> Insurance
    INSURE_RC = case_when(INSUREA == 1 ~ "Not insured",
                          INSURE == 97 ~ "Don't know",
                          INSURE == 98 ~ "Refuse to answer", #None refused to answer
                          TRUE ~ "Insured"), 
    INSURE_RCD_Insured = if_else(INSURE_RC == "Insured", 1, 0),
    INSURE_RCD_Unknown = if_else(INSURE_RC == "Don't know", 1, 0),
    ADAP_RCD_Yes     = if_else(ADAP == 1, 1, 0),
    ADAP_RCD_Unknown = if_else(ADAP == 7, 1, 0), #'Skipped' treated as 'No'
    #> HIV Disclosure
    DISC_RC = case_when(DISCA == 1 ~ "No one",
                        DISCB == 1 | DISCC == 1 ~ "Partner/Sex partner",
                        DISCD == 1 | DISCE == 1 ~ "Friends/Family",
                        DISCF == 1 | DISCG == 1 |
                          DISCH == 1 | DISCI == 1 |
                          DISCJ == 1 ~ "Other person",
                        TRUE ~ "Refuse to answer"), #None refused to answer
    DISC_RCD_Partner = if_else(DISC_RC == "Partner/Sex partner", 1, 0),
    DISC_RCD_Family  = if_else(DISC_RC == "Friends/Family", 1, 0),
    DISC_RCD_Other   = if_else(DISC_RC == "Other person", 1, 0),
    #> Substance Use
    DRUG_RC = case_when(DRUG1LA == 1 ~ "Alcohol",
                        DRUG1LB == 1 ~ "Tobacco",
                        DRUG1LC == 1 ~ "Marijuana",
                        rowSums(
                          select(.,
                                 one_of(paste0("DRUG1L", LETTERS[4:12]),
                                        paste0("DRUG2L", LETTERS[1:10]))               
                          ) == 1) > 0 ~ "Other drug(s)",
                        DRUG1LM == 1 & DRUG2LK == 1 ~ "None",
                        TRUE ~ "Refuse to answer"),
    DRUG_RCD_Alcohol   = if_else(DRUG_RC == "Alcohol", 1, 0),
    DRUG_RCD_Tobacco   = if_else(DRUG_RC == "Tobacco", 1, 0),
    DRUG_RCD_Marijuana = if_else(DRUG_RC == "Marijuana", 1, 0),
    DRUG_RCD_Other     = if_else(DRUG_RC == "Other drug(s)", 1, 0),
    DRUG_RCD_None      = if_else(DRUG_RC == "None", 1, 0),
    DRUG_RCD_Refuse    = if_else(DRUG_RC == "Refuse to answer", 1, 0),
    INJECTL_RCD_Inject = if_else(INJECTL == 1, 1, 0),
    INJECTL_RCD_Refuse = if_else(INJECTL == 8, 1, 0)
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
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer; 99 = skipped
  mutate(
    MTUSNX_RC = case_when(
      rowSums(is.na(select(., one_of(paste0("MTUSNX0", 1:9, "_RC"))))) < 9 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("MTUSNX0", 1:9, "_RC"))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  #> Positive Attitudes Toward Technology, 6 items (MTUAX01, MTUAX03, MTUAX04, MTUAX09:MTUAX11)
  mutate_at(vars(matches("MTUAX\\d{2}")),
            list(RC = ~replace(., which(. > 5), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer; 99 = skipped
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
  #> Sexual Health (Lifetime)
  mutate_at(vars(S56_25B, S56_25C, S56_25D, S56_25E, S56_25F, S56_25G),
            list(RC = ~replace(., which(. > 1), NA))) %>%
  mutate(Outcome_SexHealth = if_else(
    rowSums(
      select(., one_of(
        paste0("S56_25", c("B", "C", "D", "E", "F", "G"), "_RC")
      )),
      na.rm = TRUE) > 0,
    1, 0)
  ) %>%
  #> General Health (Lifetime)
  mutate_at(vars(S56_25A, S56_25H, S56_25I, S56_25J, S56_25K, S56_25L),
            list(RC = ~replace(., which(. > 1), NA))) %>%
  mutate(Outcome_GenHealth = if_else(
    rowSums(
      select(., one_of(
        paste0("S56_25", c("A", "H", "I", "J", "K", "L"), "_RC")
      )),
      na.rm = TRUE) > 0,
    1, 0)
  )

##### Create a data set for analysis excluding original variables
acasi_analysis <- acasi %>%
  select(PID,
         SITE_RCD_FRI, SITE_RCD_NYSDA, SITE_RCD_HBHC, SITE_RCD_MHS,
         SITE_RCD_PFC, SITE_RCD_PSU, SITE_RCD_SFDPH, SITE_RCD_WFU, SITE_RCD_WUSL, #Site
         AGE, #Age
         # SEXBRTH, #SES: Sex/Gender #> Removed
         GENDER_RCD_Female, GENDER_RCD_Trans, GENDER_RCD_Other, #Gender
         RACE_RCD_Latino, RACE_RCD_Black, RACE_RCD_WhiteMix, RACE_RCD_Other, #Ethnicity & Race
         INSCHOOL_RCD, #Education
         GRADE_RCD_PostK, GRADE_RCD_Grad, #Education
         MONEY_RC_Log, #Income
         EMPLOY_RCD_Employed, #Employment
         RELSTAT_RCD_Dating, RELSTAT_RCD_Partnered, RELSTAT_RCD_Other, #Relationship
         ORIENT_RCD_Gay, ORIENT_RCD_Bi, ORIENT_RCD_Other, #Orientation
         # starts_with("LIVED"), #Housing
         STAY7D_RCD_Stable, STAY7D_RCD_Institution, STAY7D_RCD_Other, STAY7D_RCD_Refuse, #Housing
         # JAILL, #JAILLX, JAIL6X, #Incarceration 
         BORNHIV_RCD, DIAGHIV_RC, #Length with HIV: First HIV Diagnosis
         # matches("CARE[[:alpha:]]6"), #Healthcare utilization: Recent care
         # CARELHIV, CARLHIVA, CD4FST, VIRALFST, #Healthcare utilization: Engagement in care
         # starts_with("CAREHV"), #Healthcare utilization: Retention in care
         # ends_with("LST"), CD4LOW, INFECTN, AIDSDIAG, #Healthcare utilization: Quality of care
         # ARTPRESC, ARTL, ARTLAGE, ARTREC, ARTNOW, #Healthcare utilization: Treatment
         # ARTADHR, #Healthcare utilization: Adherence
         INSURE_RCD_Insured, INSURE_RCD_Unknown, ADAP_RCD_Yes, ADAP_RCD_Unknown, #Healthcare utilization: Insurance
         HE_RC_HAL, HE_RC_HSE, #Youth Health Engagement scale
         CARE_RC, #Provider Empathy (CARE) scale
         # starts_with("SSND"), starts_with("SSUSE"), #Support services needed and used ###
         # starts_with("CNEED"), -CNEED3ES, -CNEED4, -CNEED5, CNEED3ES, CNEED4, CNEED5, #Competing needs ###
         DISC_RCD_Partner, DISC_RCD_Family, DISC_RCD_Other, #Disclosure
         STIGMA_RC, #HIV-related stigma
         MENTALH_RC, MENTALH4_RC,#Mental health
         DRUG_RCD_Alcohol, DRUG_RCD_Tobacco, DRUG_RCD_Marijuana, DRUG_RCD_Other, 
         DRUG_RCD_None, DRUG_RCD_Refuse, #Substance use: non-injected
         INJECTL_RCD_Inject, INJECTL_RCD_Refuse, #Substance use: injected
         SOCIALS_RC, #Social support
         #Media Technology Usage and Attitudes Scale
         MTUEX_RC, 
         MTUSPX_RC_Text, 
         MTUSPX_RC_Smartphone, 
         MTUIX_RC, MTUIX5_RC, MTUIX6_RC,
         MTUSNX_RC, #MTUSN also available
         MTUAX_RC_Pos, MTUAX_RC_Anx, MTUAX_RC_Neg, MTUAX02_RC, MTUAX07_RC,
         starts_with("Outcome")
  )

#Insurance 'Other' included as 'Insured' until further notice.

save(acasi, acasi_analysis, file = "Analyses/Digital Health-Seeking Behaviors/ETAC_DHSB/acasi.RData")

x2 <- inner_join(acasi00m %>%
                   select(SITE1, PID, SCREEN7) %>%
                   mutate(SITE1 = fct_recode(as.factor(SITE1),
                                             "CBW" = "1", "FRI" = "2", "NYSDA" = "3", 
                                             "HBHC" = "4", "MHS"  = "5", "PSU" = "6", 
                                             "PFC" = "7", "SFDPH" = "8", "WFU"  = "9", 
                                             "WUSL" = "10"),
                          ViralSupp_Screen = case_when(SCREEN7 == 1 ~ 0,
                                                       SCREEN7 == 2 ~ 1)) %>%
                   filter(!is.na(ViralSupp_Screen)) %>%
                   select(-SCREEN7),
                 acasi2 %>%
                   select(SITE1, PID, ViralSupp) %>%
                   filter(!is.na(ViralSupp)),
                 by = c("SITE1", "PID"))
chisq.test(x2$ViralSupp, x2$ViralSupp_Screen)
