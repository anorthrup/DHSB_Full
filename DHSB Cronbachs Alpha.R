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
alphaMTUEX <- alpha(acasi2 %>% select(matches("MTUEX\\d{1}_RC")), 
                    cumulative = TRUE)
alphaMTUSPX <- alpha(acasi2 %>% select(
  one_of(paste0("MTUSPX", str_pad(c(4:9, 10:12), width = 2, pad = 0), "_RC"))),
  cumulative = TRUE)
alphaMTUIX <- alpha(acasi2 %>% select(matches("MTUIX\\d{1}_RC")), 
                    cumulative = TRUE)
alphaMTUSNX <- alpha(acasi2 %>% select(one_of(paste0("MTUSNX0", 1:9, "_RC"))),
                     cumulative = TRUE)
alphaMTUAXPos <- alpha(acasi2 %>% select(one_of(
  paste0("MTUAX", str_pad(c(1:4, 9:11), width = 2, pad = 0), "_RC")
)),
cumulative = TRUE)
alphaTable <- function (alpha, description) {
  tribble(
    ~Scale,      ~Alpha,                     ~Items,     ~Variable,
    description, alpha[["total"]]$raw_alpha, alpha$nvar, unique(str_replace(names(alpha$keys), "([[:alpha:]]*)(\\d+)(_RC)", "\\1"))
  )
}
bind_rows(
  alphaTable(alphaSOCIALS,  "Social Support"),
  alphaTable(alphaSTIGMA,   "HIV-related Stigma"),
  alphaTable(alphaHAL,      "Health Access Literacy"),
  alphaTable(alphaHSE,      "Health Self-Efficacy"),
  alphaTable(alphaCARE,     "Provider Empathy"),
  alphaTable(alphaMENTALH,  "Physical and Mental Health"),
  alphaTable(alphaMTUEX,    "Email Usage"),
  alphaTable(alphaMTUSPX,   "Mobile Phone Usage"),
  alphaTable(alphaMTUIX,    "Internet Search"),
  alphaTable(alphaMTUSNX,   "Social Media Usage"),
  alphaTable(alphaMTUAXPos, "Positive Technology Attitudes")
)