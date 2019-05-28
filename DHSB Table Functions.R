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
              list(~fct_recode(factor(., levels = c(freqLevels, keep)),
                              !!!levRecode))) %>%
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
    mutate(Variable = fct_relevel(factor(Variable, levels = c(freqLevels, keep)),
                                  varRelevel)) %>%
    arrange(Variable) %>%
    mutate(Variable = str_replace(as.character(Variable), "(.*)", "   \\1")) %>%
    select(Variable, Overall, `Corpus Christi`, `Los Angeles`, `New York`, 
           Chicago, Cleveland, Hershey, Philadelphia, `San Francisco`, 
           `Winston-Salem`, `St. Louis`) %>%
    add_case(Variable = paste0(header, ", N (%)"), .before = 1)
}

table_Continuous <- function(x, variable, stat = "mean",
                             name = variable, header = NULL) {
  varQuo <- quo(!!sym(variable))
  siteSum <- function(x) {
    x %>%
    {
      if (stat == "mean") {
        summarize(.,
                  Metric = paste0(round(mean(!!varQuo, na.rm = TRUE), 1), 
                                  " (", round(sd(!!varQuo, na.rm = TRUE), 1), ")"),
                  Max = max(!!varQuo, na.rm = TRUE)
        )
      } else if (stat == "median") {
        summarize(.,
                  Metric = paste0(median(!!varQuo, na.rm = TRUE), 
                                  " [",
                                  paste(quantile(!!varQuo, c(0.25, 0.75), na.rm = TRUE), collapse = ", "),
                                  "]"
                  ),
                  Max = max(!!varQuo, na.rm = TRUE)
        )
      } else {
        stop ("'stat' must be set to 'mean' or 'median' only.")
      }
    }
  }
  bind_rows(
    siteSum(x) %>%
      mutate(SITE_RC = "Overall"),
    x %>%
      group_by(SITE_RC) %>%
      siteSum(.) %>%
      mutate(SITE_RC = as.character(SITE_RC))) %>%
    mutate(Max = max(Max)) %>%
    spread(SITE_RC, Metric) %>%
    mutate(Variable = paste0("   ", name, " [", Max, "]")) %>%
    select(Variable, Overall, levels(x$SITE_RC)) %>%
    {
      if (!is.null(header)) add_row(., 
                                    Variable = str_replace(header, "(.*)(, Mean \\(SD\\))", "\\1 [Max Value]\\2"), 
                                    .before = 1) else .
    }
}

#Chi-square test for levels of variable in tables
chisq.dhsb <- function(x, varName, varLevels) {
  testResult <- chisq.test(
    x = as.matrix(x %>%
                    filter(Variable %in% varLevels) %>%
                    select(-Variable, -Overall))
  )
  tribble(
    ~Variable, ~`Chi-Squared Statistic`, ~Df,                  ~`p Value`,
    varName,   testResult$statistic,     testResult$parameter, testResult$p.value
  )
}

#ANOVA test for continuous variables
anova.dhsb <- function(x, variable, name) {
  testResult <- oneway.test(as.formula(paste(variable, "~ SITE1")), data = demo)
  tribble(
    ~Variable, ~`F Statistic`,       ~`DF Numerator`,         ~`DF Denominator`,        ~`p Value`,
    name,      testResult$statistic, testResult$parameter[1], testResult$parameter[2], testResult$p.value
  )
}
