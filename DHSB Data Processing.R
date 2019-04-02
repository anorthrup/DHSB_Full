#####Digital Health-Seeking Behaviors
#####> Data Processing and Variable Transformations

#####Read libraries
library(tidyverse)
library(sjlabelled)
library(rio)
library(lubridate)

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
  select(SITE1, PID,
         surveylanguage, INTRVWER, SITE, #SES: Survey
         SCREEN1, starts_with("AGE"), starts_with("DOB"), #SES: Age
         SEXBRTH, starts_with("GENDER"), #SES: Gender
         LATINO, starts_with("RACE"), #SES: Race
         INSCHOOL, GRADE, #SES: Education
         MONEY, starts_with("EMPLOY"), #SES: Employment
         RELSTAT, #SES: Relationship
         starts_with("ORIENT"), #SES: Orientation
         starts_with("LIVED"), starts_with("STAY7"), #Housing
         JAILL, JAILLX, JAIL6X, #Incarceration
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
                             by = c("SITE1", "PID"))
acasiJoin00m <- anti_join(acasi00mTrim, acasi06mTrim, by = c("SITE1", "PID"))
acasiJoin06m <- anti_join(acasi06mTrim, acasi00m, by = c("SITE1", "PID"))
#Do not include cases from 00m that do not exist in 06m
acasi <- bind_rows(acasiJoinInner) %>%
  mutate(SITE1 = fct_recode(as.factor(SITE1),
                            "CBW" = "1", "FRI" = "2", "NYSDA" = "3", 
                            "HBHC" = "4", "MHS"  = "5", "PSU" = "6", 
                            "PFC" = "7", "SFDPH" = "8", "WFU"  = "9", 
                            "WUSL" = "10")) %>%
  arrange(SITE1)

#####Creation of new variables
#####Collapse existing demographic variables and create scales

#Demographic variables
acasi2 <- acasi %>%
  mutate(
    SITE_RC = fct_recode(as.factor(SITE1),
                         "Corpus Christi" = "CBW", "Los Angeles" = "FRI", 
                         "New York" = "NYSDA", "Chicago" = "HBHC", 
                         "Cleveland"  = "MHS", "Hershey" = "PSU", 
                         "Philadelphia" = "PFC", "San Francisco" = "SFDPH", 
                         "Winston-Salem"  = "WFU", "St. Louis" = "WUSL"),
    ##Demographic variables
    GENDER_RC = fct_recode(as.factor(GENDER),
                           "Male (cis man)"     = "1", 
                           "Female (cis woman)" = "2",
                           "Trans person"       = "3", 
                           "Trans person"       = "4",
                           "Other gender"       = "5", 
                           "Other gender"       = "6",
                           "Refuse to answer"   = "8"),
    ORIENT_RC = fct_recode(as.factor(ORIENT),
                           "Straight"          = "1", 
                           "Gay or lesbian"    = "2", 
                           "Bisexual"          = "3",
                           "Other orientation" = "4", 
                           "Other orientation" = "5", 
                           "Other orientation" = "7",
                           "Refuse to answer"  = "8"),
    RELSTAT_RC = fct_recode(as.factor(RELSTAT),
                            "Single"            = "1", 
                            "Dating"            = "2", 
                            "Dating"            = "3", 
                            "Partnered/Married" = "4",
                            "Partnered/Married" = "5", 
                            "Other status"      = "6",
                            "Refuse to answer"  = "8"),
    GRADE_RC = fct_recode(as.factor(GRADE),
                          "High school, equivalent or less"  = "1", 
                          "High school, equivalent or less"  = "2", 
                          "High school, equivalent or less"  = "3", 
                          "Some post-K12"                    = "4", 
                          "Associate, trade cert, or higher" = "5", 
                          "Associate, trade cert, or higher" = "6",
                          "Associate, trade cert, or higher" = "7", 
                          "Refuse to answer"                 = "8"),
    STAY7D_RC = fct_recode(as.factor(STAY7D),
                           "Rented house/apt/flat"     =  "1", 
                           "Other's house/apt/flat"    =  "2",
                           "Other's house/apt/flat"    =  "3", 
                           "Other's house/apt/flat"    =  "4",
                           "Hotel/Shelter/Rehab"       =  "5", 
                           "Hotel/Shelter/Rehab"       =  "6",
                           "Hotel/Shelter/Rehab"       =  "7", 
                           "Hotel/Shelter/Rehab"       =  "8",
                           "Hospital/Medical facility" =  "9", 
                           "Jail/Juvenile detention"   = "10",
                           "Other residence"           = "11", 
                           "Refuse to answer"          = "12"),
    RACE_RC = case_when(LATINO == 1 ~ "Latino",
                        RACEC == 1 ~ "Black, Not Latino",
                        RACEE == 1 & RACE > 1 ~ 
                          "White Mixed-Race, Not Latino or Black",
                        RACEE == 1 ~ "White, Not Latino",
                        TRUE ~ "Other race"),
    EMPLOY_RC = case_when(EMPLOYA == 1 ~ "Employed/Student",
                          EMPLOYB == 1 ~ "Employed/Student",
                          EMPLOYC == 1 ~ "Employed/Student",
                          EMPLOYD == 1 ~ "Unemployed/Disabled",
                          EMPLOYE == 1 ~ "Unemployed/Disabled",
                          EMPLOYF == 1 ~ "Unemployed/Disabled"),
    #Change INSURE to categorical: Insured, Not insured, Don't know; first look at combinations of options
    INSUREA_RC = replace(as.character(INSUREA), which(INSUREA == 1), "A"),
    INSUREB_RC = replace(as.character(INSUREB), which(INSUREB == 1), "B"),
    INSUREC_RC = replace(as.character(INSUREC), which(INSUREC == 1), "C"),
    INSURED_RC = replace(as.character(INSURED), which(INSURED == 1), "D"),
    INSUREE_RC = replace(as.character(INSUREE), which(INSUREE == 1), "E"),
    INSUREF_RC = replace(as.character(INSUREF), which(INSUREF == 1), "F"),
    INSUREG_RC = replace(as.character(INSUREG), which(INSUREG == 1), "G"),
    INSUREH_RC = replace(as.character(INSUREH), which(INSUREH == 1), "H")
  ) %>%
  unite("INSURE_RC", sep = "", remove = FALSE,
        INSUREA_RC, INSUREB_RC, INSUREC_RC, INSURED_RC, 
        INSUREE_RC, INSUREF_RC, INSUREG_RC, INSUREH_RC) %>%
  select(-matches("^INSURE[[:alpha:]]_RC")) %>%
  mutate(INSURE_RC = str_replace_all(INSURE_RC, "0", "")) %>%
  mutate(INSURE_RC = case_when(INSURE_RC == "A" ~ "Not insured",
                               INSURE_RC == "77777777" ~ "Don't know", #If participant marked "Don't know", all answers = 7
                               TRUE ~ "Insured")) %>%
  #Scales
  ##Procedure:
  ##> 1) Convert any values not included in the item to NA ("Refuse to answer", "Don't know", or "Skipped")
  ##>>     Each variable coded as integers; values above certain integer are generally refuse, don't know, skip
  ##> 2) Check whether all items are NA (Does number of NA = number of items?)
  ##> 3) If not, sum all items (ignoring NA values: na.rm = TRUE)
  
  ##Social support, 3 items
  mutate_at(vars(matches("SOCIALS\\d{1}")),
            list(RC = ~replace(., which(. > 10), NA))) %>% #Values of 0-10 expected; above that = Refuse/Skip
  mutate(
    SOCIALS_RC = case_when(
      rowSums(is.na(select(., matches("SOCIALS\\d{1}_RC")))) < 3 ~ #Is number of NA < number of items?
        rowSums(select(., matches("SOCIALS\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  ##HIV-related Stigma, 10 items
  mutate_at(vars(matches("STIGMA\\d{1}")),
            list(RC = ~replace(., which(. > 4), NA))) %>% #Values of 0-4 expected; above that = Refuse
  mutate(
    STIGMA_RC = case_when(
      rowSums(is.na(select(., matches("STIGMA\\d+_RC")))) < 10 ~ #Is number of NA < number of items?
        rowSums(select(., matches("STIGMA\\d+_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  ##Youth Health Engagement, 10 items
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
  ##Provider Empathy (CARE), 10 items
  mutate_at(vars(matches("CARE\\d{2}")),
            list(RC = ~replace(., which(. > 5), NA))) %>% #Values of 1-5 expected; 8 = refuse, 9 = "Not Applicable"
  mutate(
    CARE_RC = case_when(
      rowSums(is.na(select(., matches("CARE\\d{2}_RC")))) < 10 ~ #Is number of NA < number of items?
        rowSums(select(., matches("CARE\\d{2}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  ##Physical and Mental Health, 4 items
  ##> Exclude MENTALH4 because it differs from MENTALH1-3
  mutate_at(vars(starts_with("MENTALH"), -MENTALH4),
            list(RC = ~replace(., which(. > 6), NA))) %>% #Values of 1-6 expected; 8 = refuse to answer
  mutate(
    MENTALH3_RC = 7 - MENTALH3, #Reverse code MENTALH3 because it is negatively correlated with MENTALH1/2
    MENTALH_RC = case_when(
      rowSums(is.na(select(., matches("MENTALH\\d{1}_RC")))) < 10 ~ #Is number of NA < number of items?
        rowSums(select(., matches("MENTALH\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  ##Email Usage, 4 items
  mutate_at(vars(starts_with("MTUEX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUEX_RC = case_when(
      rowSums(is.na(select(., matches("MTUEX\\d{1}_RC")))) < 4 ~ #Is number of NA < number of items?
        rowSums(select(., matches("MTUEX\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  ##Text Usage, 3 items
  mutate_at(vars(starts_with("MTUSPX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUSPX_RC_Text = case_when(
      rowSums(is.na(select(., MTUSPX01, MTUSPX02, MTUSPX12))) < 3 ~
        rowSums(is.na(select(., MTUSPX01, MTUSPX02, MTUSPX12)))
    )
  ) %>%
  ##Mobile Phone Usage, 9 items
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
  ##Internet Search, 6 items
  mutate_at(vars(starts_with("MTUIX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer
  mutate(
    MTUIX_RC = case_when(
      rowSums(is.na(select(., matches("MTUIX\\d{1}_RC")))) < 6 ~ #Is number of NA < number of items?
        rowSums(select(., matches("MTUIX\\d{1}_RC")), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  ##General Social Media Usage, 9 items
  mutate_at(vars(starts_with("MTUSNX")),
            list(RC = ~replace(., which(. > 9), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer; 99 = skipped
  mutate(
    MTUSNX_RC = case_when(
      rowSums(is.na(select(., one_of(paste0("MTUSNX0", 1:9))))) < 9 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(paste0("MTUSNX0", 1:9))), na.rm = TRUE) #If so, sum columns
    )
  ) %>%
  ##MTUAX01-04, 9-11  (Positive Attitudes Technology), 7 items
  mutate_at(vars(one_of(paste0("MTUAX", 
                               str_pad(c(1:4, 9:11), width = 2, pad = 0)))),
            list(RC = ~replace(., which(. > 5), NA))) %>% #Values of 0-9 expected; 98 = refuse to answer; 99 = skipped
  mutate(
    MTUAX_RC_Pos = case_when(
      rowSums(is.na(select(., one_of(
        paste0("MTUAX", str_pad(c(1:4, 9:11), width = 2, pad = 0))
      )))) < 7 ~ #Is number of NA < number of items?
        rowSums(select(., one_of(
          paste0("MTUAX", str_pad(c(1:4, 9:11), width = 2, pad = 0))
        )),
        na.rm = TRUE) #If so, sum columns
    )
  )

#Insurance 'Other' included as 'Insured' until further notice.

#SOCIALS: 1 Refuse (SOCIALS1)
#STIGMA: 1 Refuse STIGMA06, 1 Refuse STIGMA08
#HE: 717 skipped (HE05): exclude HE05 from HAL subscale; 0-26 "Don't know" in each item
#CARE: 1 Refuse (CARE09)
#CARE01, CARE03: 116 'Not Applicable'; CARE04, CARE08-CARE10: 117; CARE05-CARE07: 118; CARE02: 119
#MENTALH: 1 Refuse (MENTALH1)
#MTUEX: 1 Refuse MTUEX2
#MTUSPX: 0 Refuse
#MTUIX: 0 Refuse
#MTUSNX: 1 Refuse (MTUSNX05), 67 skipped; excluded MTUSNX10-12 (sex partners/relationships, sex health, trans-specific)
#MTUAX: 1 Refuse (MTUAX 08, 09, 13, 14)

acasi2 %>% 
  select(one_of(c("SOCIALS_RC", "STIGMA_RC", "HE_RC_HAL", "HE_RC_HSE", 
                  "CARE_RC", "MENTALH_RC", "MTUEX_RC", "MTUSPX_RC",
                  "MTUIX_RC", "MTUSNX_RC"))) %>%
  map(~length(which(is.na(.))))

#Check all that apply that still need to be recoded:
#> DISC
#> Substance use

# save(acasi2, file = "acasi.RData")



