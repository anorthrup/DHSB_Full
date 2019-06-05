#####Digital Health-Seeking Behaviors
#####> Comparison of participants with complete baseline/6 months survey and those missing 6 months

#####Read libraries
library(tidyverse)
library(openxlsx)
library(rio)

#####Load data
load("acasi.RData")

#####Make comparisons
#Categorical variables
chiAcasi <- function (x, variable) {
  output <- x %>%
    select(Set, variable) %>%
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
  chiAcasi(acasi, "AGE_RC"),
  chiAcasi(acasi %>% 
             mutate(RACE_RC = replace(RACE_RC,
                                      which(RACE_RC == "White Mixed-Race, Not Latino or Black"),
                                      "White, Not Latino")),
             # filter(RACE_RC != "White Mixed-Race, Not Latino or Black"), 
           "RACE_RC"),
  chiAcasi(acasi, "GENDER_RC"),
  chiAcasi(acasi, "ORIENT_RC"),
  chiAcasi(acasi, "GRADE_RC"),
  chiAcasi(acasi, "STAY7D_RC"),
  chiAcasi(acasi, "INSURE_RC")
) %>%
  mutate(Note = "",
         Note = replace(Note, which(Variable == "RACE_RC"),
                        "'White, Mixed' incorporated into 'White, Not Latino'"))

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
logitAge <- glm(Set ~ AGE, data = acasi, family = "binomial")
arm::binnedplot(x = predict(logitAge),
                y = resid(logitAge))
ggplot(acasi, aes(x = MONEY_RC_Log, y = Set)) +
  geom_point() +
  geom_smooth(method = "loess")

#MONEY
logitMoney <- glm(Set ~ MONEY_RC_Log, data = acasi, family = "binomial")
arm::binnedplot(x = predict(logitMoney),
                y = resid(logitMoney))
ggplot(acasi, aes(x = MONEY_RC_Log, y = Set)) +
  geom_point() +
  geom_smooth(method = "loess")

compareContinuous <- bind_rows(
  logitAcasi(logitAge, "AGE"),
  logitAcasi(logitMoney, "MONEY_RC_Log")
)

##### Create Excel sheet
wb <- createWorkbook("Comparison of Included and Excluded Participants.xlsx")
# wb <- loadWorkbook("Comparison of Included and Excluded Participants.xlsx")
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
saveWorkbook(wb, "Comparison of Included and Excluded Participants.xlsx", overwrite = TRUE)
