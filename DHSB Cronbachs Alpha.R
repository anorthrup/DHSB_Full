#####Digital Health-Seeking Behaviors
#####> Calculation of Cronbach's Alpha for scales

#####Load libraries
library(tidyverse)
library(psych)

#####Load data
load("acasi.RData")

#####Calculate Cronbach's alpha for scales
alphaSOCIALS <- alpha(acasi %>% 
                        select(matches("SOCIALS\\d{1}_RC")) %>%
                        filter(rowSums(is.na(.)) < 3), 
                      cumulative = TRUE)
identical(alphaSOCIALS$scores, 
          acasi %>%
            select(SOCIALS_RC) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaSTIGMA <- alpha(acasi %>% 
                       select(matches("STIGMA\\d+_RC")) %>%
                       filter(rowSums(is.na(.)) < 10), 
                     cumulative = TRUE)
identical(alphaSTIGMA$scores, 
          acasi %>%
            select(STIGMA_RC) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaHAL <- alpha(acasi %>% 
                    select(one_of(paste0("HE0", 1:4, "_RC"))) %>%
                    filter(rowSums(is.na(.)) < 4), 
                  cumulative = TRUE)
identical(alphaHAL$scores, 
          acasi %>%
            select(HE_RC_HAL) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaHSE <- alpha(acasi %>% 
                    select(one_of(c(paste0("HE0", 6:9, "_RC"), "HE10_RC"))) %>%
                    filter(rowSums(is.na(.)) < 5), 
                  cumulative = TRUE)
identical(alphaHSE$scores, 
          acasi %>%
            select(HE_RC_HSE) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaCARE <- alpha(acasi %>% 
                     select(matches("CARE\\d{2}_RC")) %>%
                     filter(rowSums(is.na(.)) < 10), 
                   cumulative = TRUE)
identical(alphaCARE$scores, 
          acasi %>%
            select(CARE_RC) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMENTALH <- alpha(acasi %>% 
                        select(matches("(MENTALH)(1|2|3)(_RC)")) %>%
                        filter(rowSums(is.na(.)) < 3), 
                      cumulative = TRUE)
identical(alphaMENTALH$scores, 
          acasi %>%
            select(MENTALH_RC) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

#MTUAS Subscales
alphaMTU_Email <- alpha(acasi %>% 
                          select(matches("MTUEX\\d{1}_RC")) %>%
                          filter(rowSums(is.na(.)) < 4), 
                        cumulative = TRUE)
identical(alphaMTU_Email$scores, 
          acasi %>%
            select(MTUEX_RC) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMTU_Text <- alpha(acasi %>% 
                         select(MTUSPX01_RC, MTUSPX02_RC, MTUSPX12_RC) %>%
                         filter(rowSums(is.na(.)) < 3),
                       cumulative = TRUE)
identical(alphaMTU_Text$scores, 
          acasi %>%
            select(MTUSPX_RC_Text) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMTU_Smartphone <- alpha(acasi %>% 
                               select(
                                 one_of(
                                   paste0(
                                     "MTUSPX", 
                                     str_pad(3:11, width = 2, pad = 0), 
                                     "_RC"
                                   )
                                 )
                               ) %>%
                               filter(rowSums(is.na(.)) < 9),
  cumulative = TRUE)
identical(alphaMTU_Smartphone$scores, 
          acasi %>%
            select(MTUSPX_RC_Smartphone) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMTU_Internet <- alpha(acasi %>% 
                             select(one_of(paste0("MTUIX", c(1:4), "_RC"))) %>%
                             filter(rowSums(is.na(.)) < 4), 
                    cumulative = TRUE)
identical(alphaMTU_Internet$scores, 
          acasi %>%
            select(MTUIX_RC) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMTU_SocialMed <- alpha(acasi %>% 
                              select(one_of(paste0("MTUSNX0", 1:9, "_RC"))) %>%
                              filter(rowSums(is.na(.)) < 9),
                     cumulative = TRUE)
identical(alphaMTU_SocialMed$scores, 
          acasi %>%
            select(MTUSNX_RC) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMTU_Pos <- alpha(acasi %>% 
                        select(
                          one_of(
                            paste0(
                              "MTUAX", 
                              str_pad(c(1, 3:4, 9:11), width = 2, pad = 0), 
                              "_RC"
                            )
                          )
                        ) %>%
                        filter(rowSums(is.na(.)) < 6),
  cumulative = TRUE)
identical(alphaMTU_Pos$scores, 
          acasi %>%
            select(MTUAX_RC_Pos) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMTU_Anx <- alpha(acasi %>% 
                        select(
                          one_of(
                            paste0(
                              "MTUAX", 
                              str_pad(c(5:6, 8), width = 2, pad = 0), 
                              "_RC"
                            )
                          )
                        ) %>%
                        filter(rowSums(is.na(.)) < 3),
  cumulative = TRUE)
identical(alphaMTU_Anx$scores, 
          acasi %>%
            select(MTUAX_RC_Anx) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

alphaMTU_Neg <- alpha(acasi %>% 
                        select(
                          one_of(
                            paste0(
                              "MTUAX", 
                              str_pad(12:14, width = 2, pad = 0), 
                              "_RC"
                            )
                          )
                        ) %>%
                        filter(rowSums(is.na(.)) < 3),
  cumulative = TRUE)
identical(alphaMTU_Neg$scores, 
          acasi %>%
            select(MTUAX_RC_Neg) %>%
            filter(!is.na(.)) %>%
            unlist() %>%
            unname())

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
  alphaTable(alphaMTU_Neg,        "Negative Attitudes Toward Technology")
)
alphaValues

# write_csv(alphaValues, "Subscale Cronbachs Alphas.csv", na = "")
save(alphaValues, file = "Cronbachs.RData")
