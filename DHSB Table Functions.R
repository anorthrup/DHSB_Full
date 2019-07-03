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
  
  x <- x %>%
    #Narrow x to only necessary columns
    select(SITE_RC, names(variables)) %>%
    mutate_if(is.double, list(as.integer)) %>%
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
  if ("Other" %in% variables) {
    if (names(variables)[which(variables == "Other")] == "") {
      names(variables)[which(variables == "Other")] <- "Other"
    }
  } 

  #Summarize responses overall and by site
  full_join( #Create two data frames and join them together: summary of overall, summary by site
    #Summarize overall
    x %>% 
      select(-SITE_RC) %>%
      {
        bind_cols(map_int(., sum) %>% 
                    as.data.frame() %>%
                    rownames_to_column("Variable"),
                  map_int(., length) %>%
                    as.data.frame(),
                  map_dbl(., mean) %>%
                    as.data.frame())
      } %>%
      setNames(c("Variable", "Sum", "Length", "Mean")) %>%
      mutate(Mean = scales::percent(Mean, accuracy = 0.1)) %>%
      unite("Fraction", Sum, Length, sep = "/") %>%
      unite("Overall", Fraction, Mean, sep = " (") %>%
      mutate(Overall = gsub("(.*)", "\\1)", Overall)),
    #Summarize by site
    x %>%
      group_by(SITE_RC) %>%
      summarize_all(list(~sum, ~mean)) %>%
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
    select(Variable, Overall, levels(x$SITE_RC)[levels(x$SITE_RC) %in% colnames(.)]) %>%
           # `Corpus Christi`, `Los Angeles`, `New York`, 
           # Chicago, Cleveland, Hershey, Philadelphia, `San Francisco`, 
           # `Winston-Salem`, `St. Louis`) %>%
    mutate(Variable = str_replace(Variable, "(.*)", "   \\1")) %>%
    add_row(Variable = paste0(header, ", N (%)"), .before = 1) %>%
    mutate(Header = Variable[1]) %>%
    select(Header, everything())
}

table_OneFactor <- function (x, varString, header = varString, 
                             n = NULL, keep = NULL, varRelevel = NULL) {
  if (is.null(n) & !is.null(keep)) {
    print("'keep' is unnecessary when 'n' is not supplied")
    keep = NULL
  }
  
  varQuo <- quo(!!sym(varString))
  x <- x %>%
    select(SITE_RC, varString) %>%
    filter(!(!!varQuo %in% c("Skipped", "Refuse to answer", "Missing"))) %>%
    mutate_at(vars(one_of(varString)), list(factor))
  
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
              list(~fct_recode(factor(., levels = c(freqLevels, keep)), !!!levRecode))) %>%
    { #Create two data frames and bind them together: summary of overall, summary by site
      bind_rows(
        select(., varString) %>% #Overall summary
          table() %>%
          as.data.frame(., stringsAsFactors = FALSE) %>%
          setNames(c("Variable", "N")) %>%
          mutate(Site = "Overall",
                 Percent = scales::percent(N / sum(N), accuracy = 0.1),
                 N = paste0(N, "/", sum(N))),
        select(., SITE_RC, varString) %>% #Summary by site
          table() %>%
          as.data.frame(., stringsAsFactors = FALSE) %>%
          setNames(c("Site", "Variable", "N")) %>%
          group_by(Site) %>%
          mutate(Percent = scales::percent(N / sum(N), accuracy = 0.1)) %>%
          mutate(N = as.character(N))
      )
    } %>%
    unite("Frequency", N, Percent, sep = " (") %>%
    mutate(Frequency = str_replace(Frequency, "(.*)", "\\1)")) %>%
    spread(Site, Frequency) %>%
    {
      if (!is.null(n)) {
        mutate(., Variable = fct_relevel(factor(Variable, levels = c(freqLevels[1:n], keep, "Other")),
                                         varRelevel))
      }else {
        mutate(., Variable = fct_relevel(factor(Variable, levels = c(freqLevels, keep)),
                                         varRelevel))
      }
    } %>%
    # mutate(Variable = fct_relevel(factor(Variable, levels = c(freqLevels, keep)),
    #                               varRelevel)) %>%
    arrange(Variable) %>%
    mutate(Variable = str_replace(as.character(Variable), "(.*)", "   \\1")) %>%
    select(Variable, Overall, levels(x$SITE_RC)[levels(x$SITE_RC) %in% colnames(.)]) %>%
    # `Corpus Christi`, `Los Angeles`, `New York`, 
    # Chicago, Cleveland, Hershey, Philadelphia, `San Francisco`, 
    # `Winston-Salem`, `St. Louis`) %>%
    add_case(Variable = paste0(header, ", N (%)"), .before = 1) %>%
    mutate(Header = Variable[1]) %>%
    select(Header, everything())
}

table_Continuous <- function(x, variable, stat = "mean",
                             name = variable, header = variable) {
  varQuo <- quo(!!sym(variable))
  siteSum <- function(x) {
    x %>%
    {
      if (stat == "mean") {
        summarize(.,
                  Metric = paste0(round(mean(!!varQuo, na.rm = TRUE), 1), 
                                  " (", round(sd(!!varQuo, na.rm = TRUE), 1), ")"),
                  Min = min(!!varQuo, na.rm = TRUE),
                  Max = max(!!varQuo, na.rm = TRUE)
        )
      } else {
        summarize(.,
                  Metric = paste0(median(!!varQuo, na.rm = TRUE), 
                                  " [",
                                  paste(quantile(!!varQuo, c(0.25, 0.75), na.rm = TRUE), collapse = "-"),
                                  "]"
                  ),
                  Min = min(!!varQuo, na.rm = TRUE),
                  Max = max(!!varQuo, na.rm = TRUE)
        )
      } 
    }
  }
  bind_rows(
    siteSum(x) %>%
      mutate(SITE_RC = "Overall"),
    x %>%
      group_by(SITE_RC) %>%
      siteSum(.) %>%
      mutate(SITE_RC = as.character(SITE_RC))
    ) %>%
    mutate(Min = min(Min),
           Max = max(Max),
           Range = paste0("[", Min, "-", Max, "]")) %>%
    spread(SITE_RC, Metric) %>%
    mutate(Variable = paste0("   ", name, " ", Range)) %>%
    select(Variable, Overall, levels(x$SITE_RC)[levels(x$SITE_RC) %in% colnames(.)]) %>%
    {
      if (stat == "mean") {
        add_row(.,
                Variable = paste(header, "[Range],", "Mean (SD)"), 
                .before = 1)
      } else if (stat == "median") {
        add_row(.,
                Variable = paste(header, "[Range],", "Median [IQR]"), 
                .before = 1)
      } else {
        stop ("'stat' must be set to 'mean' or 'median' only.")
      }
    } %>%
    mutate(Header = Variable[1]) %>%
    select(Header, everything())
}

#Chi-square test for levels of variable in tables
chisq.dhsb <- function(x) {
  output <- data.frame(matrix(NA, nrow = length(unique(x$Header)), ncol = 5)) %>%
    set_names(c("Variable", "Chi-Squared Statistic", "DF", "p-Value", "Warning")) %>%
    mutate(Variable = unique(x$Header))
  chisq.quiet <- quietly(chisq.test)
  for (i in 1:nrow(output)) {
    testResult <- x %>%
      filter(Header %in% output$Variable[i]) %>%
      select(-Header, -Variable, -Overall) %>%
      chisq.quiet()
    output$`Chi-Squared Statistic`[i] <- testResult$result$statistic
    output$DF[i] <- testResult$result$parameter
    output$`p-Value`[i] <- testResult$result$p.value
    if (length(testResult$warnings)) {
      output$Warning[i] <- "Count values too low, Chi-squared approximation may be incorrect"
    } else {
      output$Warning[i] <- ""
    }
  }
  return(output)
}

#ANOVA test for continuous variables
anova.dhsb <- function(x) {
  output <- data.frame(matrix(NA, nrow = ncol(x) - 1, ncol = 5)) %>%
    set_names(c("Variable", "F Statistic", "DF Numerator", "DF Denominator", "p-Value")) %>%
    mutate(Variable = colnames(x)[2:ncol(x)])
  for (i in 2:ncol(x)) {
    testResult <- oneway.test(as.formula(paste(colnames(x)[i], "~ SITE1")), data = x)
    output$`F Statistic`[i - 1] <- testResult$statistic
    output$`DF Numerator`[i - 1] <- testResult$parameter[1]
    output$`DF Denominator`[i - 1] <- testResult$parameter[2]
    output$`p Value`[i - 1] <- testResult$p.value
  }
  return(output)
}
