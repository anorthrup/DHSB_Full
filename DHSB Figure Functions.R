#####Digital Health-Seeking Behaviors
#####Functions for summarizing behaviors

#####Read libraries
library(tidyverse)

#Function to transform any positive answer to 1 (and negatives to 0) for summing
MTUIXtransform <- function(MTUIX) ifelse(MTUIX > 0 & MTUIX < 10, 1, 0)
#Transform negative answers to 0 for summing
S56transform <- function(S56) ifelse(S56 == 1, 1, 0)

#####Summarize health information sought on internet
hlthSeekCat <- function(x, ...){
  groupVars <- quos(...)
  #Find the total number who answered each question to use as a denominator for each question
  hsTot <- x %>%
    select(SitePID, AgeGroup, S56_24XL:S56_24XN, S56_25A:S56_25L, S56_26A:S56_26N) %>%
    group_by(!!! groupVars) %>%
    summarise_at(vars(matches("S56")), list(~sum(!is.na(.)))) %>% #Rows that are NA mean subject did not take survey or question is incorrectly marked as NA
    gather(Variable, Total, matches("S56")) %>%
    group_by(!!! groupVars) %>%
    mutate(Total = case_when(grepl("S56", Variable) ~ max(Total[grepl("S56", Variable)]))) #Replace denominator with maximum number of non-NA responses (representing number who took survey)
  #Find number who answered positively to individual health information seeking questions
  hsSum <- x %>%
    select(SitePID, AgeGroup, S56_24XL:S56_24XN, S56_25A:S56_25L, S56_26A:S56_26N) %>%
    #Change answers to either 1 or 0 for the purpose of summarizing (by summing all answers)
    mutate_at(vars(contains("S56")), list(~S56transform)) %>%
    #Sum all relevant columns (column sum is equivalent to total number who answered positively)
    group_by(!!! groupVars) %>%
    summarise_at(vars(matches("S56")),
                 list(~sum), na.rm = TRUE) %>%
    gather(Variable, Count, S56_24XL:S56_24XN, S56_25A:S56_25L, S56_26A:S56_26N) %>%
    #Full join with dataframe containing total number who answered question
    full_join(hsTot, by = intersect(colnames(hsTot), colnames(.))) %>%
    #Calculate proportion of positive answers
    mutate(Proportion = round(Count / Total, 3)) %>%
    #Create new general category variable to characterize groups of questions
    mutate(`Health Category` = case_when(Variable %in% c("S56_24XM", "S56_24XN", "S56_26A",
                                                         "S56_26B") ~ "Trans Health",
                                         Variable %in% c("S56_24XL", "S56_25B", "S56_25C", "S56_25D", 
                                                         "S56_25E", "S56_25F", "S56_25G", "S56_26D", "S56_26E", 
                                                         "S56_26F", "S56_26G", "S56_26H", "S56_26I") ~ "Sexual Health",
                                         Variable %in% c("S56_25A", "S56_25", "S56_25H", "S56_25I", "S56_25J",
                                                         "S56_25K", "S56_25L", "S56_26C", "S56_26J", "S56_26K",
                                                         "S56_26L", "S56_26M", "S56_26N") ~ "General Health")) %>% 
    #Create new specific topic and timeframe variable to characterize each question
    mutate(Topic = case_when(Variable %in% c("S56_24XM", "S56_26A") ~ "Hormone Therapy",
                             Variable %in% c("S56_24XN", "S56_26B") ~ "Trans Surgeries/Procedures",
                             Variable == "S56_24XL" ~ "Sex and Sexuality",
                             Variable %in% c("S56_25B", "S56_26D") ~ "HIV Testing",
                             Variable %in% c("S56_25C", "S56_26E") ~ "STD Symptoms",
                             Variable %in% c("S56_25D", "S56_26F") ~ "STD Testing/Treatment",
                             Variable %in% c("S56_25E", "S56_26G") ~ "PrEP",
                             Variable %in% c("S56_25F", "S56_26H") ~ "PEP",
                             Variable %in% c("S56_25G", "S56_26I") ~ "Free Condoms",
                             Variable %in% c("S56_25A", "S56_26C") ~ "Healthcare Services",
                             Variable %in% c("S56_25H", "S56_26J") ~ "Diet/Nutrition",
                             Variable %in% c("S56_25I", "S56_26K") ~ "Exercise",
                             Variable %in% c("S56_25J", "S56_26L") ~ "Cold/Flu Symptoms",
                             Variable %in% c("S56_25K", "S56_26M") ~ "Medication",
                             Variable %in% c("S56_25L", "S56_26N") ~ "Other"),
           Timeframe = case_when(grepl("S56_24|S56_25", Variable) ~ "Lifetime",
                                 grepl("S56_26", Variable) ~ "Last 6 Mo"))
  #Find number of positive answers for S56_25 and S56_26, assign appropriate categories/topics to questions
  hsSum <- x %>%
    select(SitePID, AgeGroup, S56_25A:S56_25L, S56_26A:S56_26N) %>%
    #Change answers to either 1 or 0 for the purpose of summarizing (by summing all answers)
    mutate_at(vars(contains("S56")), list(~S56transform)) %>%
    #Sum all patients who chose at least one positive answer in each question
    group_by(!!! groupVars) %>%
    summarise(S56_25.Count = sum(S56_25B == 1 | S56_25C == 1 | S56_25D == 1 | S56_25E == 1 |
                                   S56_25F == 1 | S56_25G, na.rm = TRUE),
              S56_26.Count = sum(S56_26D == 1 | S56_26E == 1 | S56_26F == 1 | S56_26G == 1 | 
                                   S56_26H == 1 | S56_26I, na.rm = TRUE),
              S56_25.Total = n(), #Calculate total number of subjects who took T06m survey
              S56_26.Total = n()) %>%
    #Manipulate data to appropriate form to match rest of questions
    gather(Variable, Count, contains(".")) %>%
    separate(Variable, c("Variable", "CountType"), sep = "\\.") %>%
    spread(CountType, Count) %>%
    #Calculate proportion of positive answers
    mutate(Proportion = round(Count / Total, 3)) %>%
    #Assign appropriate categories/topics
    mutate(`Health Category` = "Sexual Health",
           Topic = "Sexual Health Info",
           Timeframe = rep(c("Lifetime", "Last 6 Mo"), n()/2)) %>%
    #Add to results of individual answers
    bind_rows(hsSum, .) %>%
    mutate(Proportion = replace(Proportion, which(Proportion == "NaN"), NA)) %>%
    arrange(`Health Category`, Topic, Timeframe)
}

hlthSeekSum <- function(x, ...){
  groupVars <- quos(...)
  #Create indicator variable for whether information is searched for
  xSub <- x %>%
    select(AgeGroup, S56_24XM:S56_24XN, S56_25A:S56_25L, S56_26A:S56_26N) %>%
    mutate_at(vars(contains("S56")), list(~S56transform)) %>%
    mutate(TransLife   = ifelse(rowSums(select(., S56_24XM, S56_24XN)), 1, 0),
           Trans6Mo    = ifelse(rowSums(select(., S56_26A, S56_26B)), 1, 0),
           SexualLife  = ifelse(rowSums(select(., S56_25B, S56_25C, S56_25D, S56_25E, S56_25F, S56_25G)),
                                1, 0),
           Sexual6Mo   = ifelse(rowSums(select(., S56_26D, S56_26E, S56_26F, S56_26G, S56_26H, S56_26I)),
                                1, 0),
           GeneralLife = ifelse(rowSums(select(., S56_25A, S56_25H, S56_25I, S56_25J, S56_25K, S56_25L)),
                                1, 0),
           General6Mo  = ifelse(rowSums(select(., S56_26C, S56_26J, S56_26K, S56_26L, S56_26M, S56_26N)), 
                                1, 0))
  hsSum <- xSub %>%
    group_by(!!! groupVars) %>%
    summarise_at(vars(TransLife:General6Mo), list(~sum), na.rm = TRUE) %>%
    gather(Variable, Count, matches("Trans|Sexual|General")) %>%
    full_join(., xSub %>%
                group_by(!!! groupVars) %>%
                summarise_at(vars(TransLife:General6Mo), list(~sum(!is.na(.)))) %>%
                gather(Variable, Total, matches("Trans|Sexual|General"))) %>%
    mutate(Proportion = round(Count / Total, 3),
           Category  = paste(gsub("(.*)(Life)|(.*)(6Mo)", "\\1\\3", Variable), "Health"),
           Timeframe = case_when(grepl("Life", Variable) ~ "Lifetime",
                                 grepl("6Mo", Variable)  ~ "6 Months"))
}

#####Summarize social networking, private messaging, texting, email activity
socNetworkCat <- function(x, ...) {
  groupVars <- quos(...)
  #Find the total number who answered each question to use as a denominator for each question
  snTot <- x %>%
    select(SitePID, AgeGroup, S56_6A:S56_6R, S56_9A:S56_9R, S56_12M:S56_12Q, -S56_12N, 
           S56_15A:S56_15R, S56_18A:S56_18R) %>%
    group_by(!!! groupVars) %>%
    summarise_at(vars(contains("S56")), list(~n())) %>%
    gather(Variable, Total, contains("S56"))
  #Find number who answered positively to social networking questions
  snSum <- x %>%
    select(SitePID, AgeGroup, S56_6A:S56_6R, S56_9A:S56_9R, S56_12M:S56_12Q, -S56_12N, 
           S56_15A:S56_15R, S56_18A:S56_18R) %>%
    #Change answers to either 1 or 0 for the purpose of summarizing (by summing all answers)
    mutate_at(vars(contains("S56")), list(~S56transform)) %>%
    #Sum all relevant columns (column sum is equivalent to total number who answered positively)
    group_by(!!! groupVars) %>%
    summarise_at(vars(contains("S56")), list(~sum), na.rm = TRUE) %>%
    gather(Variable, Count, contains("S56")) %>%
    #Full join with dataframe containing total number who answered question
    full_join(snTot, intersect(colnames(snTot), colnames(.))) %>%
    #Calculate proportion of positive answers
    mutate(Proportion = round(Count / Total, 3)) %>%
    #Create new general category variable to characterize groups of questions
    mutate(Category = case_when(Variable %in% c("S56_6A", "S56_9A", "S56_6B", "S56_9B", "S56_6C", "S56_9C", 
                                                "S56_6D", "S56_9D", "S56_6E", "S56_9E", "S56_6F", "S56_9F",
                                                "S56_6G", "S56_9G", "S56_6O", "S56_9O", "S56_6R", "S56_9R", 
                                                "S56_12Q", "S56_15A", "S56_18A", "S56_15B", "S56_18B", "S56_15C", 
                                                "S56_18C", "S56_15D", "S56_18D", "S56_15E", "S56_18E", "S56_15F", 
                                                "S56_18F", "S56_15G", "S56_18G", "S56_15O", "S56_18O", "S56_15R", 
                                                "S56_18R", "S56_12Q") ~ "Lifestyle",
                                Variable %in% c("S56_6H", "S56_9H", "S56_6I", "S56_9I", "S56_6J", "S56_9J", 
                                                "S56_15H", "S56_18H", "S56_15I", "S56_18I", "S56_15J", 
                                                "S56_18J") ~ "Relationships/Sex",
                                Variable %in% c("S56_12P", "S56_6K", "S56_9K", "S56_6M", 
                                                "S56_9M", "S56_15K", "S56_18K", "S56_15M", 
                                                "S56_18M") ~ "Sexual Health",
                                Variable %in% c("S56_12M", "S56_12O", "S56_6L", "S56_9L", "S56_6N", "S56_9N", 
                                                "S56_6P", "S56_9P", "S56_6Q", "S56_9Q", "S56_15L", "S56_18L", 
                                                "S56_15N", "S56_18N", "S56_15P", "S56_18P", "S56_15Q", 
                                                "S56_18Q") ~ "LGBTQ Topics")) %>% 
    #Create new specific topic and platform variable to characterize each question
    mutate(Topic = case_when(Variable %in% c("S56_12M") ~ "Support LGBTQ Groups/Causes*",
                             Variable %in% c("S56_12O") ~ "Trans/Non-Conforming Procedures*",
                             Variable %in% c("S56_12P") ~ "HIV Information/Care*",
                             Variable %in% c("S56_12Q") ~ "Play Games*",
                             Variable %in% c("S56_6A", "S56_9A", "S56_15A", "S56_18A") ~ "Music/Movies/Television",
                             Variable %in% c("S56_6B", "S56_9B", "S56_15B", "S56_18B") ~ "Work",
                             Variable %in% c("S56_6C", "S56_9C", "S56_15C", "S56_18C") ~ "School",
                             Variable %in% c("S56_6D", "S56_9D", "S56_15D", "S56_18D") ~ "Drinking/Drugs/Partying",
                             Variable %in% c("S56_6E", "S56_9E", "S56_15E", "S56_18E") ~ "Friends/Family Positives",
                             Variable %in% c("S56_6F", "S56_9F", "S56_15F", "S56_18F") ~ "Friends/Family Negatives",
                             Variable %in% c("S56_6G", "S56_9G", "S56_15G", "S56_18G") ~ "Social Services",
                             Variable %in% c("S56_6H", "S56_9H", "S56_15H", "S56_18H") ~ "Love/Relationships",
                             Variable %in% c("S56_6I", "S56_9I", "S56_15I", "S56_18I") ~ "Sex",
                             Variable %in% c("S56_6J", "S56_9J", "S56_15J", "S56_18J") ~ "Sex Work (Clients/Dates)",
                             Variable %in% c("S56_6K", "S56_9K", "S56_15K", "S56_18K") ~ "Safe Sex",
                             Variable %in% c("S56_6L", "S56_9L", "S56_15L", "S56_18L") ~ "Sexual Identity/Attraction",
                             Variable %in% c("S56_6M", "S56_9M", "S56_15M", "S56_18M") ~ "HIV Status/Care",
                             Variable %in% c("S56_6N", "S56_9N", "S56_15N", "S56_18N") ~ "LGBTQ Videos",
                             Variable %in% c("S56_6O", "S56_9O", "S56_15O", "S56_18O") ~ "Non-LGBTQ Videos",
                             Variable %in% c("S56_6P", "S56_9P", "S56_15P", "S56_18P") ~ "Hormone Therapy",
                             Variable %in% c("S56_6Q", "S56_9Q", "S56_15Q", "S56_18Q") ~ "Other Gender Identity",
                             Variable %in% c("S56_6R", "S56_9R", "S56_15R", "S56_18R") ~ "Other"),
           Platform = case_when(grepl("S56_6", Variable) ~ "Text Messaging",
                                grepl("S56_9", Variable) ~ "Email",
                                grepl("S56_12|S56_15", Variable) ~ "Social Media",
                                grepl("S56_18", Variable) ~ "Private Messaging")) %>%
    arrange(Category, Topic, Platform)
}

socNetworkSum <- function(x, ...){
  groupVars <- quos(...)
  #Create indicator variable for whether information is searched for
  xSub <- x %>%
    select(AgeGroup, S56_6A:S56_6R, S56_9A:S56_9R, S56_12M:S56_12Q, -S56_12N, 
           S56_15A:S56_15R, S56_18A:S56_18R) %>%
    mutate_at(vars(contains("S56")), list(~S56transform)) %>%
    mutate(LGBTQSMS      = ifelse(rowSums(select(., S56_6L, S56_6N, S56_6P, S56_6Q)), 1, 0),
           LGBTQEmail    = ifelse(rowSums(select(., S56_9L, S56_9N, S56_9P, S56_9Q)), 1, 0),
           LGBTQSN       = ifelse(rowSums(select(., S56_15L, S56_15N, S56_15P, S56_15Q)), 1, 0),
           LGBTQPM       = ifelse(rowSums(select(., S56_18L, S56_18N, S56_18P, S56_18Q)), 1, 0),
           LifestyleSMS      = ifelse(rowSums(select(., S56_6A, S56_6B, S56_6C, S56_6D, S56_6E, S56_6F, 
                                                     S56_6G, S56_6O)), 1, 0),
           LifestyleEmail    = ifelse(rowSums(select(., S56_9A, S56_9B, S56_9C, S56_9D, S56_9E, S56_9F, 
                                                     S56_9G, S56_9O)), 1, 0),
           LifestyleSN       = ifelse(rowSums(select(., S56_15A, S56_15B, S56_15C, S56_15D, S56_15E, 
                                                     S56_15F, S56_15G, S56_15O)), 1, 0),
           LifestylePM       = ifelse(rowSums(select(., S56_18A, S56_18B, S56_18C, S56_18D, S56_18E, 
                                                     S56_18F, S56_18G, S56_18O)), 1, 0),
           RelationsSMS      = ifelse(rowSums(select(., S56_6H, S56_6I, S56_6J)), 1, 0),
           RelationsEmail    = ifelse(rowSums(select(., S56_9H, S56_9I, S56_9J)), 1, 0),
           RelationsSN       = ifelse(rowSums(select(., S56_15H, S56_15I, S56_15J)), 1, 0),
           RelationsPM       = ifelse(rowSums(select(., S56_18H, S56_18I, S56_18J)), 1, 0),
           SexualSMS      = ifelse(rowSums(select(., S56_6K, S56_6M)), 1, 0),
           SexualEmail    = ifelse(rowSums(select(., S56_9K, S56_9M)), 1, 0),
           SexualSN       = ifelse(rowSums(select(., S56_15K, S56_15M)), 1, 0),
           SexualPM       = ifelse(rowSums(select(., S56_18K, S56_18M)), 1, 0)
           )
  snSum <- xSub %>%
    group_by(!!! groupVars) %>%
    summarise_at(vars(LGBTQSMS:SexualPM), list(~sum), na.rm = TRUE) %>%
    gather(Variable, Count, matches("SMS|Email|SN$|PM$")) %>%
    full_join(., 
              xSub %>%
                group_by(!!! groupVars) %>%
                summarise_at(vars(LGBTQSMS:SexualPM), list(~sum(!is.na(.)))) %>%
                gather(Variable, Total, matches("SMS|Email|SN$|PM$")),
              by = c("Variable", str_replace(as.character(groupVars), "~", ""))) %>%
    mutate(Proportion = round(Count / Total, 3),
           Category  = case_when(grepl("LGBTQ", Variable) ~ "LGBTQ Topics",
                                 grepl("Lifestyle", Variable) ~ "Lifestyle",
                                 grepl("Relations", Variable) ~ "Relationships/Sex",
                                 grepl("Sexual", Variable) ~ "Sexual Health"),
           Platform = case_when(grepl("SMS", Variable) ~ "Text\nMessaging",
                                grepl("Email", Variable)  ~ "Email",
                                grepl("SN$", Variable) ~ "Social\nMedia",
                                grepl("PM$", Variable) ~ "Private\nMessaging"))
}

#####Plots
#Plots by specific category, no grouping
plotCat <- function(data, xVar = "Topic", yVar = "Proportion", fillVar, facetRVar = ".", facetCVar, title, subtitle, caption, colors){
  ggplot(data, aes_string(x = xVar, y = yVar, fill = fillVar)) +
    facet_grid(as.formula(paste(facetRVar, "~", facetCVar)), scales = "free_x", space = "free_x") +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = title, subtitle = subtitle, caption = caption) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust= 1)) +
    scale_fill_discrete(guide = guide_legend()) + theme(legend.position = "top") +
    scale_fill_manual(values = brewer.pal(9, "BuGn")[colors])
}

#Plots of summary by broad category, no grouping
plotSum <- function(data, xVar = "Category", yVar = "Proportion", fillVar, title, subtitle, caption, colors){
  ggplot(data, aes_string(x = xVar, y = yVar, fill = fillVar)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(x = "", title = title, caption = caption) +
    scale_fill_manual(values = brewer.pal(9, "BuGn")[colors]) +
    scale_y_continuous(limits = c(0, 1))
    # guides(fill = guide_legend(reverse = TRUE)) +
    # coord_flip()
}

#Plots of summary by broad category, with grouping
plotSumGroup <- function(data, xVar = "Category", yVar = "Proportion", fillVar, fillVarTitle, facetRVar, facetCVar = ".", title, subtitle, caption, colors){
  ggplot(data, aes_string(x = xVar, y = yVar, fill = fillVar)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(as.formula(paste(facetRVar, "~", facetCVar)), scales = "free", space = "free") +
    labs(x = "", title = title, caption = caption) +
    scale_fill_manual(values = brewer.pal(9, "BuGn")[colors]) +
    scale_y_continuous(limits = c(0, 1)) + 
    guides(fill = guide_legend(title = fillVarTitle))#, reverse = TRUE)) +
    # coord_flip()
}
