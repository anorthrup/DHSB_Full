#####Digital Health-Seeking Behaviors
#####> Calculation of Cronbach's Alpha for scales

#####Load libraries
library(tidyverse)
library(psych)

#####Load data
load("acasi.RData")

#####Calculate Cronbach's alpha for scales
alphaSOCIALS <- alpha(acasi2 %>% select(matches("SOCIALS\\d{1}_RC")), 
                      cumulative = TRUE)
alphaSTIGMA <- alpha(acasi2 %>% select(matches("STIGMA\\d+_RC")), 
                     cumulative = TRUE)
alphaHAL <- alpha(acasi2 %>% select(one_of(paste0("HE0", 1:4, "_RC"))), 
                  cumulative = TRUE)
alphaHSE <- alpha(acasi2 %>% select(one_of(c(paste0("HE0", 6:9, "_RC"), "HE10_RC"))), 
                  cumulative = TRUE)
alphaCARE <- alpha(acasi2 %>% select(matches("CARE\\d{2}_RC")), 
                   cumulative = TRUE)
alphaMENTALH <- alpha(acasi2 %>% select(matches("MENTALH\\d{1}_RC")), 
                      cumulative = TRUE)
#MTUAS Subscales
alphaMTU_Email <- alpha(acasi2 %>% select(matches("MTUEX\\d{1}_RC")), 
                        cumulative = TRUE)
alphaMTU_Text <- alpha(acasi2 %>% select(MTUSPX01_RC, MTUSPX02_RC, MTUSPX12_RC),
                       cumulative = TRUE)
alphaMTU_Smartphone <- alpha(acasi2 %>% select(
  one_of(paste0("MTUSPX", str_pad(3:11, width = 2, pad = 0), "_RC"))),
  cumulative = TRUE)
alphaMTU_Internet <- alpha(acasi2 %>% select(matches("MTUIX\\d{1}_RC")), 
                    cumulative = TRUE)
alphaMTU_SocialMed <- alpha(acasi2 %>% select(one_of(paste0("MTUSNX0", 1:9, "_RC"))),
                     cumulative = TRUE)
alphaMTU_Pos <- alpha(acasi2 %>% select(one_of(
  paste0("MTUAX", str_pad(c(1, 3:4, 9:11), width = 2, pad = 0), "_RC"))),
  cumulative = TRUE)
alphaMTU_Anx <- alpha(acasi2 %>% select(one_of(
  paste0("MTUAX", str_pad(c(5:6, 8), width = 2, pad = 0), "_RC"))),
  cumulative = TRUE)
alphaMTU_Neg <- alpha(acasi2 %>% select(one_of(
  paste0("MTUAX", str_pad(12:14, width = 2, pad = 0), "_RC"))),
  cumulative = TRUE)

alphaTable <- function (alpha, description) {
  tribble(
    ~Scale,      ~Alpha,                     ~Items,     ~Variable,
    description, alpha[["total"]]$raw_alpha, alpha$nvar, paste(names(alpha$keys), collapse = ", ")
  )
}
alphaValues <- bind_rows(
  alphaTable(alphaSOCIALS,        "Social Support"),
  alphaTable(alphaSTIGMA,         "HIV-related Stigma"),
  alphaTable(alphaHAL,            "Health Access Literacy"),
  alphaTable(alphaHSE,            "Health Self-Efficacy"),
  alphaTable(alphaCARE,           "Provider Empathy"),
  alphaTable(alphaMENTALH,        "Physical and Mental Health"),
  alphaTable(alphaMTU_Email,      "E-mailing"),
  alphaTable(alphaMTU_Text,       "Text Messaging"),
  alphaTable(alphaMTU_Smartphone, "Smartphone Usage"),
  alphaTable(alphaMTU_Internet,   "Internet Searching"),
  alphaTable(alphaMTU_SocialMed,  "General Social Media Usage"),
  alphaTable(alphaMTU_Pos,        "Positive Attitudes Toward Technology"),
  alphaTable(alphaMTU_Anx,        "Anxiety About Being Without Technology or Dependence on Technology"),
  alphaTable(alphaMTU_Neg,        "Negative Attitudes Toward Techology")
)
alphaValues

# write_csv(alphaValues, "Subscale Cronbachs Alphas.csv", na = "")
