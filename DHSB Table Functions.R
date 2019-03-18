##### DHSB Table-building Functions
#Data generally falls into two categories for now; will need to be changed for analysis
#1) Many binary variables or many variables with 1, 0, or some other option: skipped/don't know/etc.
#2) One factor with 2 or more options

#Of the above, variables that exist in form (1) will have to be transformed 
#in order to create dummy variables for analysis; these variables come from 'Check all that apply' questions
#To create dummy variables, all combinations of the options must be considered (or collapsed)

table_ManyBinary <- function (x, header, variables, n = NULL, keep = NULL) {
  ##variables = named vector with names as column names, values as strings
  ##n = number of names to keep, rest will be lumped into 'Other'
  if (is.null(n) & !is.null(keep)) {
    print("'keep' is unnecessary when 'n' is not supplied")
    keep = NULL
  }
  
  #Document non-binary values
  y <- x %>% 
    select(names(variables)) %>%
    map(~unique(.[!. %in% c(0, 1)])) %>%
    unlist()
  if (any(y > 0)) {
    print("Rows with the following values in the listed columns were removed:")
    print(y)
    #Count number of skipped/refused
    nonbinary <- x %>%
      select(SITE_RC, names(variables)) %>%
      mutate_at(
        vars(one_of(names(variables))), 
        funs(replace(., which(!. %in% c(0, 1)), NA))) %>%
      mutate(NumNA = if_else(rowSums(is.na(.)) > 0, 1, 0)) %>%
      {
        bind_cols(
          summarize(., Overall = as.character(sum(NumNA))),
          group_by(., SITE_RC) %>%
            summarize(N = as.character(sum(NumNA))) %>%
            spread(SITE_RC, N)
        )
      } %>%
      mutate(Variable = "Skipped/Refuse/Missing")
  } else {
    nonbinary <- NULL
  }
  
  x <- x %>%
    #Narrow x to only necessary columns
    select(SITE_RC, names(variables)) %>%
    #Remove rows with non-binary values (refused to answer, skipped)
    filter_if(is.numeric, all_vars(. %in% c(0, 1)))
  #List responses in descending ranked order
  freqCols <- x %>% 
    select(-SITE_RC) %>%
    map_int(sum) %>%
    sort(., decreasing = TRUE) %>%
    names()
  #Combine lower ranked responses if there are more than (n + 1)
  if (!is.null(n)){
    #If n is set, remove 'keep' from freqCols so that it doesn't get lumped into 'Other'
    if (!is.null(keep)) freqCols <- freqCols[which(!freqCols %in% keep)]
    #Lump low frequency variables into 'Other' when freqCols has more than one variable to lump
    if (length(freqCols) > (n + 1)) {
      x <- x %>%
        mutate(Other = rowSums(select(., freqCols[(n + 1):length(freqCols)]))) %>%
        mutate(Other = if_else(Other > 0, 1L, 0L))
      if (!is.null(keep)) {
        freqCols <- c(freqCols[0:n], "Other", keep)
      } else {
        freqCols <- c(freqCols[0:n], "Other")
      }
      x <- x %>%
        select(SITE_RC, freqCols)
    }
  } else {
    n <- length(freqCols)
  }
  variables <- c(
    variables[which(names(variables) %in% freqCols)],
    freqCols[which(!freqCols %in% names(variables))]
  )
  if("Other" %in% variables & 
     names(variables)[which(variables == "Other")] == "") 
    names(variables)[which(variables == "Other")] <- "Other"
  #Summarize responses overall and by site
  full_join( #Create two data frames and join them together: summary of overall, summary by site
    #Summarize overall
    x %>% 
      select(-SITE_RC) %>%
      {
        bind_cols(map_int(., sum) %>% 
                    as.data.frame() %>%
                    rownames_to_column("Variable"),
                  map_dbl(., mean) %>%
                    as.data.frame())
      } %>%
      setNames(c("Variable", "Sum", "Mean")) %>%
      mutate(Mean = scales::percent(Mean, accuracy = 0.1)) %>%
      unite("Overall", Sum, Mean, sep = " (") %>%
      mutate(Overall = gsub("(.*)", "\\1)", Overall)),
    #Summarize by site
    x %>%
      group_by(SITE_RC) %>%
      summarize_all(funs(sum, mean)) %>%
      gather("Key", "Value", -SITE_RC) %>%
      {
        full_join(filter(., str_detect(Key, "_sum")) %>%
                    rename(Sum = Value) %>%
                    mutate(Key = gsub("_sum", "", Key)),
                  filter(., str_detect(Key, "_mean")) %>%
                    rename(Mean = Value) %>%
                    mutate(Key = gsub("_mean", "", Key)),
                  by = c("SITE_RC", "Key"))
      } %>%
      rename(Variable = Key) %>%
      mutate(Mean = scales::percent(Mean, accuracy = 0.1)) %>%
      unite("Value", Sum, Mean, sep = " (") %>%
      mutate(Value = gsub("(.*)", "\\1)", Value)) %>%
      spread(SITE_RC, Value),
    by = "Variable"
  ) %>%
    #Overwrite variable names with readable names
    mutate(Variable = variables[
      map_int(Variable, ~which(names(variables) %in% .))
      ]) %>%
    #Row containing number of skipped/refused/missing
      {
        bind_rows(
          nonbinary,
          select(., everything()) #Previous table
        )
      } %>%
    select(Variable, Overall, `Corpus Christi`, `Los Angeles`, `New York`, 
           Chicago, Cleveland, Hershey, Philadelphia, `San Francisco`, 
           `Winston-Salem`, `St. Louis`) %>%
    mutate(Variable = str_replace(Variable, "(.*)", "   \\1")) %>%
    add_row(Variable = paste0(header, ", N (%)"), .before = 1)
}

table_OneFactor <- function (x, varString, header = varString, 
                             n = NULL, keep = NULL, varRelevel = NULL) {
  if (is.null(n) & !is.null(keep)) {
    print("'keep' is unnecessary when 'n' is not supplied")
    keep = NULL
  }
  
  varQuo <- quo(!!sym(varString))
  #Print 'Refuse to answer' values
  y <- x %>% 
    select(varString) %>%
    map(~which(. == "Refuse to answer" | . == "Skipped" | is.na(.))) %>%
    unlist() %>%
    unname()
  if (any(y > 0)) {
    print("The following rows contain contain 'Refuse to answer', 'Skipped' or 'NA':")
    print(y)
    missing <- x %>%
      select(SITE_RC, !!varQuo) %>%
      mutate_at(
        vars(one_of(varString)), 
        funs(replace(., 
                     which(. == "Refuse to answer" | . == "Skipped"), 
                     NA))) %>%
      mutate(NumNA = if_else(rowSums(is.na(.)) > 0, 1, 0)) %>%
      {
        bind_cols(
          summarize(., Overall = as.character(sum(NumNA))),
          group_by(., SITE_RC) %>%
            summarize(N = as.character(sum(NumNA))) %>%
            spread(SITE_RC, N)
        )
      } %>%
      mutate(Variable = "Skipped/Refuse/Missing")
  } else {
    missing <- NULL
  }
  
  x <- x %>%
    select(SITE_RC, varString) %>%
    filter(!!varQuo != "Skipped" & !!varQuo != "Refuse to answer") %>%
    mutate_at(vars(one_of(varString)), funs(factor))
  
  freqLevels <- x %>%
    select(varString) %>%
    table() %>%
    sort(., decreasing = TRUE) %>%
    names()
  
  if (!is.null(n)) {
    if (!is.null(keep)) {
      freqLevels <- freqLevels[which(!freqLevels %in% keep)]
    }
    if(length(freqLevels) > (n + 1)){
      levRecode <- freqLevels[(n + 1):length(freqLevels)]
      names(levRecode) <- rep("Other", length(levRecode))
    } else {
      levRecode <- NULL
    }
  } else {
    levRecode <- NULL
  }
  
  x %>%
    mutate_at(vars(one_of(varString)), 
              funs(fct_recode(factor(., levels = c(freqLevels, keep)),
                              !!!levRecode))) %>%
                              { #Create two data frames and bind them together: summary of overall, summary by site
                                full_join(
                                  select(., varString) %>% #Overall summary
                                    table() %>%
                                    as.data.frame(., stringsAsFactors = FALSE) %>%
                                    setNames(c("Variable", "N")) %>%
                                    mutate(Percent = scales::percent(N / sum(N), accuracy = 0.1)),
                                  select(., SITE_RC, varString) %>% #Summary by site
                                    table() %>%
                                    as.data.frame(., stringsAsFactors = FALSE) %>%
                                    setNames(c("Site", "Variable", "N")) %>%
                                    group_by(Site) %>%
                                    mutate(Percent = scales::percent(N / sum(N), accuracy = 0.1)) %>%
                                    unite("Frequency", N, Percent, sep = " (") %>%
                                    mutate(Frequency = str_replace(Frequency, "(.*)", "\\1)"),
                                           Variable = as.character(Variable)) %>%
                                    spread(Site, Frequency),
                                  by = "Variable"
                                )
                              } %>%
    unite("Overall", N, Percent, sep = " (") %>%
    mutate(Overall = str_replace(Overall, "(.*)", "\\1)")) %>%
    mutate(Variable = fct_relevel(factor(Variable, levels = unique(Variable)),
                                  varRelevel)) %>%
    arrange(Variable) %>%
    mutate(Variable = as.character(Variable)) %>%
    {
      bind_rows( #Create totals row and bind to previous summary table
        missing,
        select(., everything()) #Previous summary table
      )
    } %>%
    mutate(Variable = str_replace(Variable, "(.*)", "   \\1")) %>%
    select(Variable, Overall, `Corpus Christi`, `Los Angeles`, `New York`, 
           Chicago, Cleveland, Hershey, Philadelphia, `San Francisco`, 
           `Winston-Salem`, `St. Louis`) %>%
    add_case(Variable = paste0(header, ", N (%)"), .before = 1)
}
