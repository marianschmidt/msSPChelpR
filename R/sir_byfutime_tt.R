
#' Calculate standardized incidence ratios with costum grouping variables by follow-up time
#'
#' @param df dataframe in wide format
#' @param dattype can be "zfkd" or "seer" or empty. Will set default variable names from dataset.
#' @param expcount_src can bei either "refrates" or "cohort"
#' @param futime_src Source of follow-up time for calculating reference ratecan bei either "refpop" or "cohort"
#' @param ybreak_vars variables from df by which SIRs should be stratified in result df. Multiple variables will result in
#'                    appended rows in result df. 
#'                    Careful: do not chose any variables that are dependent on occurence of count_var (e.g. Histology of second cancer).
#'                    If y_break_vars = "none", no stratification is performed. Default is "none".
#' @param xbreak_var One variable from df by which SIRs should be stratified as a second dimension in result df. This variable will be added as
#'                    a second stratification dimension to ybreak_vars and all variables will be calculated for subpopulations of x and y combinations. 
#'                    Careful: do not chose any variables that are dependent on occurence of count_var (e.g. Year of second cancer).
#'                    If y_break_vars = "none", no stratification is performed. Default is "none".                
#' @param futime_breaks vector that indicates split points for follow-up time groups (in years) that will be used as xbreak_var.
#'                      Default is c(0, .5, 1, 5, 10, Inf) that will result in 5 groups (up to 6 months, 6-12 months, 1-5 years, 5-10 years, 10+ years). 
#'                      If you don't want to split by follow-up time, use futime_breaks = "none".
#' @param collapse_ci If TRUE upper and lower confidence interval will be collapsed into one column separated by "-". Default is FALSE.
#' @param calc_total_row option to calculate a row of totals. Can be either FALSE for not adding such a row or TRUE for adding it at the first row. Default is TRUE.
#' @param calc_total_fu option to calculate totals for follow-up time. Can be either FALSE for not adding such a column or TRUE for adding. Default is TRUE.
#' @param count_var variable to be counted as observed case. Cases are usually the second cancers. Should be 1 for case to be counted.
#' @param refrates_df df where reference rate from general population are defined. It is assumed that refrates_df has the columns 
#'                  "region" for region, "sex" for gender, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "incidence_crude_rate" for incidence rate in the respective age/gender/year cohort.
#'                  refrates_df must use the same category coding of age, sex, region, year and icdcat as agegroup_var, sex_var, region_var, year_var and site_var. 
#' @param region_var variable in df that contains information on region where case was incident. Default is set if dattype is given.
#' @param agegroup_var variable in df that contains information on age-group. Default is set if dattype is given.
#' @param sex_var variable in df that contains information on gender. Default is set if dattype is given.
#' @param year_var variable in df that contains information on year or year-period when case was incident. Default is set if dattype is given.
#' @param site_var variable in df that contains information on ICD code of case diagnosis. Cases are usually the second cancers. Default is set if dattype is given.
#' @param futime_var variable in df that contains follow-up time per person between date of first cancer and any of death, date of event (case), end of FU date (in years; whatever event comes first). Default is set if dattype is given.
#' @param pyar_var variable in refpop_df that contains person-years-at-risk in reference population (can only be used with futime_src = "refpop") Default is set if dattype is given.
#' @param alpha signifcance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @param std_pop can be either "ESP2013, ESP1976, WHO1960
#' @param truncate_std_pop if TRUE standard population will be truncated for all age-groups that do not occur in df
#' @param stdpop_df df where standard population is defined. It is assumed that stdpop_df has the columns "sex" for gender, "age" for age-groups,
#'                  "standard_pop" for name of standard population (e.g. "European Standard Population 2013) and "population_n" for size of standard population age-group.
#'                  stdpop_df must use the same category coding of age and sex as agegroup_var and sex_var.
#' @param refpop_df df where reference population data is defined. Only required if option futime = "refpop" is chosen. It is assumed that refpop_df has the columns 
#'                  "region" for region, "sex" for gender, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "population_pyar" for person-years at risk in the respective age/gender/year cohort.
#'                  refpop_df must use the same category coding of age, sex, region, year and icdcat as agegroup_var, sex_var, region_var, year_var and site_var. 
#' @return df
#' @importFrom rlang .data
#' @export
#'
#'


sir_byfutime_tt <- function(df,
                            dattype = "zfkd",
                            expcount_src = "refrates",
                            futime_src = "cohort",
                            xbreak_var = "none",
                            ybreak_vars = "none",
                            futime_breaks = c(0, .5, 1, 5, 10, Inf),
                            count_var,
                            refrates_df = rates,
                            calc_total_row = TRUE,
                            calc_total_fu = TRUE,
                            collapse_ci = FALSE,
                            stdpop_df = standard_population, #optional for indirect standardization
                            refpop_df = population,        #optional for indirect standardization
                            std_pop = "ESP2013",           #optional for indirect standardization
                            truncate_std_pop = NULL,        #optional for indirect standardization
                            region_var = NULL,
                            agegroup_var = NULL,
                            sex_var = NULL,
                            year_var = NULL,
                            site_var = NULL,
                            futime_var = NULL,
                            pyar_var = NULL,              #optional for indirect standardization
                            alpha = 0.05) {
  
  
  ###----  prepwork
  
  #setting default parameters
  na_explicit <- "zzz_NA_explicit" # string for explicit NAs
  
  
  # getting and setting names / preferences
  
  count_var <- rlang::ensym(count_var)
  futime_breaks_quo <- rlang::enquo(futime_breaks)
  
  if(ybreak_vars[1] != "none"){
    yb <- TRUE
    ybreak_vars <- rlang::enquo(ybreak_vars)
    ybreak_var_names <- rlang::eval_tidy(ybreak_vars)
    length_yb <- length(ybreak_var_names)
  } else{
    yb <- FALSE
    length_yb <- 1
  }
  
  if(xbreak_var[1] != "none"){
    xb <- TRUE
    xbreak_var <- rlang::enquo(xbreak_var)
    xbreak_var_names <- rlang::eval_tidy(xbreak_var)
    length_xb <- length(xbreak_var_names)
    x <- 1
  } else{
    xb <- FALSE
    length_xb <- 1}
  
  
  #futime_option
  if(futime_breaks[1] != "none"){
    fu <- TRUE
  } else{
    fu <- FALSE
  }
  
  #check param calc_total
  if(!is.logical(calc_total_fu)){
    rlang::inform("Parameter `calc_total_fu` should be logical (TRUE or FALSE). Default `calc_total_fu = TRUE` will be used instead.")
    calc_total_fu <- TRUE
  }
  
  if(calc_total_fu == TRUE){
    ft <- TRUE
  } else{ft <- FALSE} #dummy to show loop for Total line
  
  
  # setting default var names and values for SEER data --> still need to update to final names!
  if (dattype == "seer") {
    if (is.null(region_var)) {
      region_var <- rlang::sym("p_region.1")
    } else{
      region_var <- rlang::ensym(region_var)
    }
    if (is.null(agegroup_var)) {
      agegroup_var <- rlang::sym("t_agegroup.1")
    } else{
      agegroup_var <- rlang::ensym(agegroup_var)
    }
    if (is.null(sex_var)) {
      sex_var <- rlang::sym("SEX.1")
    } else{
      sex_var <- rlang::ensym(sex_var)
    }
    if (is.null(year_var)) {
      year_var <- rlang::ensym("t_yeardiag.1")
    } else{
      year_var <- rlang::ensym(year_var)
    }
    if (is.null(site_var)) {
      site_var <- rlang::sym("t_icdcat.2")
    } else{
      site_var <- rlang::ensym(site_var)
    }
    if (is.null(futime_var)) {
      futime_var <- rlang::sym("p_futimeyrs.1")
    } else{
      futime_var <- rlang::ensym(futime_var)
    }
  }
  
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd") {
    if (is.null(region_var)) {
      region_var <- rlang::sym("p_region.1")
    } else{
      region_var <- rlang::ensym(region_var)
    }
    if (is.null(agegroup_var)) {
      agegroup_var <- rlang::sym("t_agegroupdiag.1")
    } else{
      agegroup_var <- rlang::ensym(agegroup_var)
    }
    if (is.null(sex_var)) {
      sex_var <- rlang::sym("SEX.1")
    } else{
      sex_var <- rlang::ensym(sex_var)
    }
    if (is.null(year_var)) {
      year_var <- rlang::sym("t_yeardiag.1")
    } else{
      year_var <- rlang::ensym(year_var)
    }
    if (is.null(site_var)) {
      site_var <- rlang::sym("t_icdcat.2")
    } else{
      site_var <- rlang::ensym(site_var)
    }
    if (is.null(futime_var)) {
      futime_var <- rlang::sym("p_futimeyrs.1")
    } else{
      futime_var <- rlang::ensym(futime_var)
    }
  }
  
  
  if (expcount_src == "cohort") {
    if (is.null(pyar_var)) {
      pyar_var <- rlang::sym("population_pyar")
    } else{
      pyar_var <- rlang::ensym(pyar_var)
    }
  }
  
  
  # remove all labels from dfs and change factors to character to avoid warning messages
  df <- df%>%
    tidytable::mutate_across.(.cols = where(is.factor), .fns = as.character)
  
  df[] <- lapply(df, function(x) { attributes(x) <- NULL; x })
  
  
  if(expcount_src == "refrates"){
    refrates_df <- refrates_df %>%
      tidytable::mutate_across.(.cols = where(is.factor), .fns = as.character)
    
    refrates_df[] <- lapply(refrates_df, function(x) { attributes(x) <- NULL; x })
  }
  
  if(expcount_src == "cohort"){
    stdpop_df <- stdpop_df %>%
      tidytable::mutate_across.(.cols = where(is.factor), .fns = as.character)
    
    stdpop_df[] <- lapply(stdpop_df, function(x) { attributes(x) <- NULL; x })
  }
  
  if(expcount_src == "cohort"){
    refpop_df <- refpop_df %>%
      tidytable::mutate_across.(.cols = where(is.factor), .fns = as.character)
    
    refpop_df[] <- lapply(refpop_df, function(x) { attributes(x) <- NULL; x })
  }
  
  #create empty objects for possible warnings and errors
  
  problems_pyar_attr <- c()
  problems_not_empty_attr <- c()
  problems_missing_ref_strata_attr <- c()
  problems_missing_futime_attr <- c()
  
  
  #create vector with basic matching variables age, sex, region, icdcat, year
  
  strata_var_names <- c(rlang::as_string(agegroup_var), rlang::as_string(sex_var), rlang::as_string(region_var), rlang::as_string(site_var), rlang::as_string(year_var))
  
  #add additional options for cohort calculations
  
  
  #CHK2: check whether all required variables are defined and present in dataset
  defined_vars <-
    c(
      rlang::as_string(region_var),
      rlang::as_string(agegroup_var),
      rlang::as_string(sex_var),
      rlang::as_string(year_var),
      rlang::as_string(site_var),
      rlang::as_string(count_var),
      rlang::as_string(futime_var)
    )
  
  not_found <- defined_vars[!(defined_vars %in% colnames(df))]
  
  
  if (length(not_found) > 0) {
    rlang::abort(
      paste0(
        "The following variables defined are not found in the provided dataframe df: ",
        paste(not_found, collapse = ", ")
      )
    )
  }
  
  #CHK3: check whether all cases used for analysis have futime calculated
  
  if (fu){
    
    problems_missing_futime <- df %>%
      tidytable::filter.(is.na(rlang::eval_tidy(!!futime_var)))
    
    if (nrow(problems_missing_futime) > 0) {
      rlang::inform(
        paste0(
          "There are ", nrow(problems_missing_futime), "rows in the data set for which futime_var is missing.", 
          "\nPlease make sure that you have: ", 
          "\n - calculated FU time for all cases where the index event occured and", 
          "\n - have removed all cases from the dataset that do not count at baseline."
        )
      )
      problems_missing_futime_attr <- c(problems_missing_futime_attr, 
                                        paste0(
                                          "There are ", nrow(problems_missing_futime), "rows in the data set for which futime_var is missing.", 
                                          "\nPlease make sure that you have: ", 
                                          "\n - calculated FU time for all cases where the index event occured and", 
                                          "\n - have removed all cases from the dataset that do not count at baseline."
                                        ))
    }
    
  }
  
  #####---- doing everything for the first y
  
  #1a: prepare df
  
  #make all important variables characters and make NAs explicit (for better matching)
  df <- df %>%
    tidytable::mutate.(
      age = as.character(!!agegroup_var),
      sex = as.character(!!sex_var),
      region = as.character(!!region_var),
      year = as.character(!!year_var),
      t_icdcat = as.character(!!site_var)) %>%
    tidytable::mutate_across.(.cols = c(age, sex, region, year, t_icdcat), 
                              .fns = ~tidytable::replace_na.(., na_explicit))
  
  #make all important variables characters and make NAs explicit for ybreak_vars (for better matching)
  if(yb){
    df <- df %>%
      tidytable::mutate_across.(.cols = tidyselect::all_of(ybreak_var_names), .fns = ~as.character(.)) %>%
      tidytable::mutate_across.(.cols = tidyselect::all_of(ybreak_var_names), .fns = ~tidytable::replace_na.(., na_explicit))
  }
  
  #make all important variables characters and make NAs explicit for xbreak_var (for better matching)
  if(xb){
    df <- df %>%
      tidytable::mutate_across.(.cols = tidyselect::all_of(xbreak_var_names), .fns = ~as.character(.)) %>%
      tidytable::mutate_across.(.cols = tidyselect::all_of(xbreak_var_names), .fns = ~tidytable::replace_na.(., na_explicit))
  }
  
  #1b: prepare calc_total_row option
  
  if(!is.logical(calc_total_row)){
    rlang::inform("Parameter `calc_total_row` should be logical (TRUE or FALSE). Default `calc_total_row = TRUE` will be used instead.")
    calc_total_row <- TRUE
  }
  
  if(calc_total_row == TRUE){
    
    df <- df %>%
      tidytable::mutate.(total_var = "Overall")
    
    if(yb){
      length_yb <- length_yb + 1
      
      ybreak_var_names <- c("total_var", ybreak_var_names) #add total before all other ybreak_vars
      
    } else{
      yb <- TRUE
      length_yb <- 1
      ybreak_var_names <- c("total_var")
    }
  }
  
  #1c: prepare futime
  
  
  if(fu){
    
    
    df <- df %>%
      tidytable::mutate.(futimegroup = cut(!!futime_var, breaks = !!futime_breaks, right = FALSE))
    
    
    # capture and change futime levels to make grouping readable 
    futimegroup_levels <- levels(df$futimegroup) %>% 
      stringr::str_replace("\\[0,", "to ") %>% 
      stringr::str_replace(",Inf\\)", "\\+ years") %>% 
      stringr::str_replace("\\[", "") %>% 
      stringr::str_replace(",", "-")  %>% 
      stringr::str_replace("\\)", " years") %>% 
      stringr::str_replace("to 0.0833 years", "to 1 month") %>% 
      stringr::str_replace("to 0.167 years", "to 2 months") %>% 
      stringr::str_replace("to 0.25 years", "to 3 months") %>% 
      stringr::str_replace("to 0.333 years", "to 4 months") %>% 
      stringr::str_replace("0.0833-0.5 years", "1-6 months") %>% 
      stringr::str_replace("0.167-0.5 years", "2-6 months") %>% 
      stringr::str_replace("0.25-0.5 years", "3-6 months") %>% 
      stringr::str_replace("0.333-0.5 years", "4-6 months") %>% 
      stringr::str_replace("to 0.5 years", "to 6 months") %>% 
      stringr::str_replace("0.5-1 years", "6-12 months")
    
    #assign levels
    levels(df$futimegroup) <- futimegroup_levels
    
    #normalized names without spaces and special characters
    futimegroup_levels_norm <- futimegroup_levels %>% 
      stringr::str_replace_all(stringr::fixed(" "), "") %>% 
      stringr::str_replace_all("-", "to") %>% 
      stringr::str_replace_all("\\.", "") %>% 
      stringr::str_replace_all("\\+", "plus") %>% 
      stringr::str_replace(stringr::regex("^[[:digit:]]"), "x")
    
    
    #create dummy variables for each level of fub
    for (lv in 1:nlevels(df$futimegroup)){
      df[, paste0(futimegroup_levels_norm[lv])] <- ifelse(as.numeric(df$futimegroup) >= lv, 1, 0)
    }
    
    
    # prepare calc_total_fu option - add total level at the end of futime_groups
    
    if(ft){
      #add to all levels
      futimegroup_levels <- c(futimegroup_levels, paste0("Total ", futime_breaks[1]," to ", futime_breaks[length(futime_breaks)], " years"))
      #normalized names without spaces and special characters
      futimegroup_levels_norm <- futimegroup_levels %>% 
        stringr::str_replace_all(stringr::fixed(" "), "") %>% 
        stringr::str_replace_all("-", "to") %>% 
        stringr::str_replace_all("\\.", "") %>% 
        stringr::str_replace_all("\\+", "plus") %>% 
        stringr::str_replace(stringr::regex("^[[:digit:]]"), "x")
      #create total dummy
      df[, paste0(futimegroup_levels_norm[length(futimegroup_levels)])] <- ifelse(!is.na(df$futimegroup), 1, 0)
    }
    
    
    #CHK - check that there are no missing values for futimegroups
    
    chk_na <- df %>% tidytable::filter.(is.na(futimegroup)) %>% nrow()
    
    if (chk_na > 0) {
      warning(
        paste0(
          "The variable for follow-up time has: ", chk_na, " missings. These will be omitted when creating the crosstabs.")
      )
    }
    
    
    
    #make symbols out of fu_time_levels
    
    fu_var_names <- futimegroup_levels_norm
    length_fu <- length(fu_var_names)
  }
  
  #make settings for fu == FALSE
  if (!fu){
    length_fu <- 1
    fu_var_names <- c("Total_FU")
    futime_breaks <- c(0,Inf)
    df <- df %>%
      tidytable::mutate.(Total_FU = 1)
  }
  
  #set-up progess bar
  
  n_iters <- (length_fu * length_yb) + 1
  
  pb <- progress::progress_bar$new(
    format = "  fun running [:bar] :percent eta: :eta",
    total = n_iters, clear = TRUE)
  
  pb$tick(0)
  Sys.sleep(3 / 100)
  
  #getting all ICD codes from refrates_df
  
  refrates_icd_all <- refrates_df %>%
    tidytable::select.(t_icdcat) %>%
    tidytable::filter.(substr(t_icdcat, 1, 5) != "Total") %>% #removing icd codes that are not based on 3 digits (e.g. Total categories)
    unique(.) %>%
    purrr::pluck(1)
  
  ### F1 Calculating Observed by group (within cohort) and PYARs
  
  #start loop for iterations of ybreak_vars [y]
  for(y in 1:length_yb){
    
    if(yb){
      syb_var <- rlang::sym(ybreak_var_names[y])
      syb_var_name <- rlang::as_string(syb_var)
    }
    if(xb){
      sxb_var <- rlang::sym(xbreak_var_names[1])
      sxb_var_name <- rlang::as_string(sxb_var)
    }
    
    #start loop for iterations of follow-up levels [f]
    fu_tot_f <- FALSE
    for(f in 1:length_fu){
      
      
      fub_var <- rlang::sym(fu_var_names[f])
      
      #set identifier fu_tot_f to see whether we are in the loop that calculates the total
      if (substr(rlang::as_string(fub_var), 1, 5)=="Total"){
        fu_tot_f <- TRUE
      }
      
      
      #F1b calculate observed
      sircalc_count <- df %>%
        tidytable::mutate.(count_var_new = tidytable::case.(
          !!fu_tot_f & ((!!count_var) == 1) & ((!!futime_var) >= !!futime_breaks[1]), 1, #for iteration fu_tot_f count_var is always 1
          ((!!count_var) == 1) & ((!!futime_var) >= !!futime_breaks[f]) & ((!!futime_var) < !!futime_breaks[f+1]), 1, #otherwise only count cases occurring between futime_breaks
          default = 0)) %>% 
        tidytable::summarize.(i_observed = sum(.SD$count_var_new, na.rm = TRUE), 
                              .by = tidyselect::all_of(c("age", "sex", "region", "year", "t_icdcat",
                                                         if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)}, 
                                                         rlang::as_string(fub_var))))
      
      sircalc_count <- sircalc_count %>% #complete groups where i_observed = 0
        tidytable::filter.(!!fub_var == 1) %>%  #remove category fub_var == 0, which does not apply #incompatiblity with fu=FALSE (unclear, how to do conditional filtering)
        {if (!yb & !xb){tidyr::complete(., age, sex, region, year, t_icdcat = refrates_icd_all)} else{.}} %>%
        {if (yb & !xb){tidyr::complete(., age, sex, region, year, t_icdcat = refrates_icd_all, !!syb_var)} else{.}} %>%
        {if (yb & xb){tidyr::complete(., age, sex, region, year, t_icdcat = refrates_icd_all, !!syb_var, !!sxb_var)} else{.}} %>%
        {if (!yb & xb){tidyr::complete(., age, sex, region, year, t_icdcat = refrates_icd_all, !!sxb_var)} else{.}} %>%
        # {if (!yb & !xb){tidytable::complete.(., .SD$age, .SD$sex, .SD$region, .SD$year, t_icdcat = !!refrates_icd_all)} else{.}} %>%
        # {if (yb & !xb){tidytable::complete.(., .SD$age, .SD$sex, .SD$region, .SD$year, t_icdcat = !!refrates_icd_all, !!syb_var)} else{.}} %>%
        # {if (yb & xb){tidytable::complete.(., .SD$age, .SD$sex, .SD$region, .SD$year, t_icdcat = !!refrates_icd_all, !!syb_var, !!sxb_var)} else{.}} %>%
        # {if (!yb & xb){tidytable::complete.(., .SD$age, .SD$sex, .SD$region, .SD$year, t_icdcat = !!refrates_icd_all, !!sxb_var)} else{.}} %>%
        tidytable::mutate.(
          i_observed = tidytable::case.(is.na(i_observed), 0,
                                        default = i_observed),
          !!fub_var := 1) %>%
        tidytable::filter.(!(t_icdcat == !!na_explicit & i_observed == 0)) # remove all NA categories as they are empty with 0 observations
      
      
      #for ybreak_var: make NAs explicit
      if(yb & !xb){
        sircalc_count <- sircalc_count %>% 
          tidytable::mutate_across.(.cols = !!syb_var, .fns = ~tidytable::replace_na.(., na_explicit)) %>%
          tidytable::filter.(!(!!syb_var == !!na_explicit & i_observed == 0)) # remove all NA categories as they are empty with 0 observations
      }
      
      #for xbreak_var: make NAs explicit
      if(!yb & xb){
        sircalc_count <- sircalc_count %>% 
          tidytable::mutate_across.(.cols = !!sxb_var, ~tidytable::replace_na.(., na_explicit)) %>%
          tidytable::filter.(!(!!sxb_var == !!na_explicit & i_observed == 0)) # remove all NA categories as they are empty with 0 observations
      }
      
      #for ybreak_vars and xbreak_var: make NAs explicit
      if(yb & xb){
        sircalc_count <- sircalc_count %>% 
          tidytable::mutate_across.(.cols = c(!!syb_var, !!sxb_var), ~tidytable::replace_na.(., na_explicit)) %>%
          tidytable::filter.(!(!!syb_var == !!na_explicit & !!sxb_var == !!na_explicit & i_observed == 0)) # remove all NA categories as they are empty with 0 observations
      }
      
      
      #F1c person-years at risk
      sircalc_fu <- df %>%
        tidytable::mutate.(
          futime_var_new = tidytable::case.(!!fu_tot_f & ((!!futime_var) >= !!futime_breaks[1]), (!!futime_var),
                                            (!!futime_var) < !!futime_breaks[f], 0,
                                            (!!futime_var) < !!futime_breaks[f+1], ((!!futime_var) - !!futime_breaks[f]),
                                            (!!futime_var) >= !!futime_breaks[f+1], (!!futime_breaks[f+1] - !!futime_breaks[f]),
                                            default = 0),
          base_n_fugroup = tidytable::case.((!!fub_var) == 1, 1,
                                            default = 0)) %>%
        tidytable::summarize.(
          i_pyar = sum(.SD$futime_var_new, na.rm = TRUE),
          n_base = sum(.SD$base_n_fugroup, na.rm = TRUE),
          .by = tidyselect::all_of(c("age", "sex", "region", "year",
                                     if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)}, 
                                     rlang::as_string(fub_var))))
      
      sircalc_fu <- sircalc_fu %>% #complete groups where i_observed = 0
        tidytable::filter.(!!fub_var == 1) %>%  #remove category fub_var == 0, which does not apply #incompatiblity with fu=FALSE (unclear, how to do conditional filtering)
        tidytable::mutate.(
          i_pyar = tidytable::case.(is.na(i_pyar), 0,
                                    default = i_pyar),
          n_base = tidytable::case.(is.na(n_base), 0,
                                    default = n_base))
      if(yb){
        sircalc_fu <- sircalc_fu %>% 
          tidytable::mutate.(
            !!syb_var := tidytable::case.(is.na(rlang::eval_tidy(!!syb_var)), "missing",
                                          default = !!syb_var))
      }
      
      if(xb){
        sircalc_fu <- sircalc_fu %>% 
          tidytable::mutate.(
            !!sxb_var := tidytable::case.(is.na(rlang::eval_tidy(!!sxb_var)), "missing",
                                          default = !!sxb_var))
      }
      
      #CHK_sircalc_n - Check that all combinations of age, sex, region, year, t_icdcat, syb_var, sxb_var are present in sircalc_count and sircalc_fu
      
      #helper function to determine number of unique values from dataframe column
      get_n_unique_values <- function(var, df){
        length(unique(df[[var]]))
      }
      
      n_t_icdcat <- length(refrates_icd_all)
      
      #required number of strata in counts is product of distinct values found in df for age, sex, region, year, syb, sxb multiplied with all t_icdcats from refrates_df
      df_f <- df %>%
        tidytable::filter.(!!fub_var == 1)
      
      n_strata_required_count <- sapply(c("age", "sex", "region", "year", 
                                          if(yb){rlang::as_string(syb_var)}, 
                                          if(xb){rlang::as_string(sxb_var)} ), 
                                        get_n_unique_values, df=df_f) %>% 
        prod(.) %>% magrittr::multiply_by(n_t_icdcat)  #determine n of unique values for all cols in df and create vector product
      
      #required number of strata in fu is distinct combinations of age, sex, region, year, syb, sxb found in df
      n_strata_required_fu <- df_f %>% 
        tidytable::distinct.(tidyselect::all_of(c("age", "sex", "region", "year", 
                                                  if(yb){rlang::as_string(syb_var)}, 
                                                  if(xb){rlang::as_string(sxb_var)}))) %>%
        nrow()
      
      
      #not found strata in sircalc_fu
      n_not_found_fu <- n_strata_required_count - (n_strata_required_fu * n_t_icdcat)
      
      
      #CHK_strata1
      
      if (n_strata_required_count != nrow(sircalc_count)) {
        rlang::warn(
          paste0(
            "The calculation of observed events was performed for: ", nrow(sircalc_count), " strata. However ", n_strata_required_count, " strata are required. Occured in: ", fub_var,",", syb_var)
        )
      }
      
      
      if (n_strata_required_fu != nrow(sircalc_fu)) {
        rlang::warn(
          paste0(
            "The calculation of observed events was performed for: ", nrow(sircalc_fu), " strata. However ", n_strata_required_fu, " strata are required. Occured in: ", fub_var,",", syb_var)
        )
      }
      
      
      #F1d merge
      
      #prepare merging
      
      #vector of matching variables in join functions
      match_vars <- c("age", "sex", "region", "year", 
                      rlang::as_string(fub_var), if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)})
      
      #check that there are no conflicting rows with regard to matching variables for join between fu and count
      n_dist_sircalc_fu <- sircalc_fu %>% 
        tidytable::distinct.(tidyselect::all_of(match_vars)) %>%
        nrow()
      
      if (n_dist_sircalc_fu != nrow(sircalc_fu)) {
        rlang::warn(
          "There are disambiguities in matching the follow-up time to the observed count strata!"
        )
      }
      
      #check that there are no unexpected missing rows in sircalc_fu
      n_exp_miss_fu <- sircalc_count %>%
        tidytable::anti_join.(sircalc_fu, by = match_vars) %>%
        nrow()
      
      if ( n_exp_miss_fu != n_not_found_fu) {
        rlang::warn(
          paste0(
            "The number of expected missing strata for fu calc are: ", n_exp_miss_fu, ". However the number of not found strata where fu cannot be matched is ", n_not_found_fu, " Check calculations! This occured in ", fub_var,",", syb_var)
        )
      }
      
      n_exp_miss_count <- sircalc_fu %>%
        tidytable::anti_join.(sircalc_count, by = match_vars) %>%
        nrow()
      
      
      
      #merge sircalc_count and sircalc_fu
      sircalc <- sircalc_count %>%
        tidytable::full_join.(sircalc_fu, by = match_vars)
      
      #some missings in t_icdcat are expected after merge for those strata where no observed case occured
      #make NAs in t_icdcat in sircalc explicit
      sircalc <- sircalc %>% 
        tidytable::mutate.(t_icdcat = tidytable::replace_na.(t_icdcat, !!na_explicit))
      
      
      #check if empty i_pyar is equal to n_not_found_fu and set i_pyar to 0
      n_i_pyar_miss <- sircalc %>% 
        tidytable::filter.(is.na(i_pyar)) %>% nrow()
      
      if(n_i_pyar_miss == n_exp_miss_fu){
        sircalc <- sircalc %>% #complete groups where i_observed = 0
          tidytable::mutate.(
            i_pyar = tidytable::case.((is.na(i_pyar) & i_observed == 0), 0,
                                      default = i_pyar),
            n_base = tidytable::case.((is.na(n_base) & i_observed == 0), 0,
                                      default = n_base))
      } else{
        rlang::warn(
          paste0(
            "The number of expected missing strata for fu calc are: ", n_exp_miss_fu, ". However the number of empty strata in sircalc is ", n_i_pyar_miss, " Check calculations!")
        )
      }
      
      #check if empty i_observed is equal to n_not_found_fu and set i_observed to 0
      n_i_observed_miss <- sircalc %>% 
        tidytable::filter.(is.na(i_observed)) %>% nrow()
      
      if(n_i_observed_miss == n_exp_miss_count){
        sircalc <- sircalc %>% #complete groups where i_observed = 0
          tidytable::mutate.(i_observed = tidytable::case.(is.na(i_observed), 0,
                                                           default = i_observed))
      } else{
        rlang::warn(
          paste0(
            "The number of expected missing strata for count i_observed are: ", n_exp_miss_count, ". However the number of empty strata in sircalc is ", n_i_observed_miss, " Check calculations!")
        )
      }
      
      
      
      #make check for unexpected discrepancies
      
      problems_not_empty <- sircalc %>%
        tidytable::filter.(i_pyar == 0 & !(i_observed == 0 & n_base < 2)) 
      
      if (nrow(problems_not_empty) > 0) {
        rlang::inform(paste0("There are disambiguities where strata with 0 follow-up time have data in observed or base",
                             paste0(utils::capture.output(problems_not_empty), collapse = "\n"), collapse = "\n"))
        problems_not_empty_attr <- c(problems_not_empty_attr, 
                                     paste0("There are disambiguities where strata with 0 follow-up time have data in observed or base",
                                            paste0(utils::capture.output(problems_not_empty), collapse = "\n"), collapse = "\n"))
      }
      
      #remove all lines with 0 information (0 observed and 0 follow-up time)
      
      sircalc <- sircalc %>%
        tidytable::filter.(!(i_pyar == 0 & i_observed == 0))
      
      
      
      ### F2-I: Merging reference rates (for refpop only) by t_icdcat, region, year, sex and age 
      
      #CHK 3: reporting used regions and whether they can be found in rates
      
      if(expcount_src == "refrates" & f == 1 & y == 1){
        
        used_strata <- sircalc %>%
          tidytable::distinct.(age, sex, region, year, t_icdcat) %>%
          tidytable::filter.(!is.na(t_icdcat))
        
        missing_ref_strata <- used_strata %>%
          tidytable::anti_join.(refrates_df, by = c("age", "sex", "region" , "year", "t_icdcat"))
        
        if(nrow(missing_ref_strata) > 0){
          rlang::inform(paste0("For the following region, years, etc no reference rates can be found:",
                               paste0(utils::capture.output(missing_ref_strata), collapse = "\n"), collapse = "\n"))
          problems_missing_ref_strata_attr <- c(problems_missing_ref_strata_attr,
                                                paste0("For the following region, years, etc no reference rates can be found:",
                                                       paste0(utils::capture.output(missing_ref_strata), collapse = "\n"), collapse = "\n"))
        }
      }
      
      
      #F2-Ia: Do merge
      if(expcount_src == "refrates"){
        
        sir_or <- sircalc %>%
          tidytable::left_join.(refrates_df, by = c("age", "sex", "region" , "year", "t_icdcat")) %>%
          tidytable::select.(-comment, -population_n_per_year)
        
      }
      
      
      ### F3-I Calculating Expected by group (from reference rates) [refrates]
      
      if(expcount_src == "refrates"){
        
        sir_basic <- sir_or %>%
          tidytable::mutate.(
            i_expected = .SD$i_pyar * .SD$incidence_crude_rate / 100000
          )
        
      }
      
      ### F3-II Calculating Expected by group (within cohort) [cohort]
      
      ### F4 Making SIR calculations 
      
      
      #SIR and Confidence intervals using calculation methods by @breslowStatisticalMethodsCancer1987
      ##F4a: calculating SIR and confidence intervals
      sir_longresult_strat_f <- sir_basic %>%
        tidytable::mutate.(
          sir = .SD$i_observed / .SD$i_expected,
          sir_lci = (stats::qchisq(p = !!alpha / 2, df = 2 * .SD$i_observed) / 2) / .SD$i_expected,
          sir_uci = (stats::qchisq(p = 1 - !!alpha / 2, df = 2 * (.SD$i_observed + 1)) / 2) / .SD$i_expected
        )
      
      
      #preparing binding
      
      if(fu){
        futimegroup_levels_f <- futimegroup_levels[f]
        
        sir_longresult_strat_f <- sir_longresult_strat_f %>%
          tidytable::mutate.(fu_time = !!futimegroup_levels_f,              #new var fu_time that indicates fu_time for calculated stratum
                             fu_time_sort = !!f) %>%          
          tidytable::select.(fu_time, tidyselect::everything()) %>%       #move fu_time col to front
          tidytable::select.(-tidyselect::starts_with(rlang::as_string(fub_var))) #remove old column named after fub_var
      } else{
        sir_longresult_strat_f <- sir_longresult_strat_f %>%
          dplyr::select(-tidyselect::starts_with(rlang::as_string(fub_var)))
      }
      
      #depending on which iteration of [f] is conducted, data should be joined (append new columns to right)
      
      if(f == 1){
        sir_longresult_strat <- sir_longresult_strat_f
      } else{
        sir_longresult_strat <- tidytable::bind_rows.(sir_longresult_strat, sir_longresult_strat_f)
      }
      
      #progress_bar
      pb$tick()
      Sys.sleep(3 / 100)
      
      #end loop [f] iterations
    }
    
    #F4b: preparing binding if needed
    if(!xb & yb){
      
      sir_longresult_strat <- sir_longresult_strat %>%
        tidytable::mutate.(yvar_name = !!syb_var_name,
                           yvar_sort = !!y) %>%
        tidytable::rename.(yvar_label = !!syb_var)  %>%
        tidytable::select.(.SD$yvar_name, .SD$yvar_label, tidyselect::everything())
    }
    
    if(xb & yb){
      
      sir_longresult_strat <- sir_longresult_strat %>%
        tidytable::mutate.(yvar_name = !!syb_var_name,
                           yvar_sort = !!y,
                           xvar_name = !!sxb_var_name,
                           xvar_sort = !!x) %>%
        tidytable::rename.(yvar_label = !!syb_var,
                           xvar_label = !!sxb_var)  %>%
        tidytable::select.(.SD$yvar_name, .SD$yvar_label, .SD$xvar_name, .SD$xvar_label, tidyselect::everything())
    }
    
    
    #F4c: binding results if needed
    if(!yb){
      sir_longresult <- sir_longresult_strat
    }
    
    if(yb){
      if(y == 1){
        sir_longresult <- sir_longresult_strat
      } else{
        sir_longresult <- tidytable::bind_rows.(sir_longresult, sir_longresult_strat)
      }
    }
    
    #end loop [y] iterations
    gc()
  }
  
  
  
  ### F5: Restructuring results
  
  #checking results 
  #CHK_R1 - PYARS should be the same for all age, gender, year, region groups
  
  problems_pyar <- sir_longresult %>% 
    tidytable::summarize.(
      min_pyar = min(.SD$i_pyar),
      max_pyar = max(.SD$i_pyar),
      .by = tidyselect::all_of(c("yvar_name", "yvar_label", "age", "sex", "region", "year",
                                 if(fu){"fu_time"}))) %>% 
    tidytable::filter.(min_pyar != max_pyar)
  
  if(nrow(problems_pyar) > 0){
    rlang::inform(paste0("There are differing pyar values for the same age, gender, year, region strata:"
                         ,paste0(utils::capture.output(problems_pyar), collapse = "\n"), collapse = "\n"))
    problems_pyar_attr <- c(problems_pyar_attr, 
                            paste0("There are differing pyar values for the same age, gender, year, region strata:", 
                                   utils::capture.output(problems_pyar), collapse = "\n")) #save information to write as attribute later
  }
  
  
  #CHK_R2 - observed cases should also occur in reference rates dataset
  
  notes_refcases <- sir_longresult %>% 
    tidytable::filter.(i_observed > incidence_cases)
  
  if(nrow(notes_refcases) > 0){
    rlang::inform(paste0("There are observed cases in the results file that do not occur in the refrates_df.",
                         "A possible explanation can be:",
                         "- DCO cases",
                         "- diagnosis of second cancer occured in different time period than first cancer",
                         "The following strata are affected:", collapse = "\n"))
    rlang::inform(paste0(utils::capture.output(notes_refcases), collapse = "\n"))
  }
  
  #final dataset should have the structure: columns
  #icdcat #yvars(1-y) #xvar1 #xvar2 #xvar3 ..#xvarx _ #n_base #observed #expected #pyar #sir #sir_uci #sir_lci
  
  
  #vi) rename vars
  
  
  sir_result_pre <- sir_longresult %>%
    tidytable::rename.(observed = i_observed,
                       expected = i_expected,
                       pyar = i_pyar,
                       ref_inc_cases = incidence_cases,
                       ref_population_pyar = population_pyar,
                       ref_inc_crude_rate = incidence_crude_rate)
  
  pb$tick()
  Sys.sleep(3 / 100)
  
  #5d rounding
  
  sir_result_pre <- sir_result_pre %>%
    tidytable::mutate_across.(.cols = c(pyar, sir, sir_lci, sir_uci), 
                              .fns = ~round(.,2))
  
  #collapse_ci option
  
  if(!is.logical(collapse_ci)){
    rlang::inform("Parameter `collapse_ci` should be logical (TRUE or FALSE). Default `collapse_ci = FALSE` will be used instead.")
    collapse_ci <- FALSE
  }
  
  if(collapse_ci == TRUE){
    
    sir_result_pre <- sir_result_pre %>%
      tidytable::unite.(col = "sir_ci", sir_lci, sir_uci, sep = " - ")
  }
  
  
  
  ### F5: labeling and returning results
  
  
  #since tidytable::arrange.() does not support tidyselect, we need to create a list of symbols to pass on
  final_sort_var_quo <- rlang::syms(c("age", "region", "sex", "year", 
                                      if(yb){c("yvar_sort", "yvar_label")}, 
                                      if(xb){c("xvar_name", "xvar_label")}, 
                                      if(fu){"fu_time_sort"}, "t_icdcat"))
  
  sir_result <- sir_result_pre %>%
    tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", 
                                            if(yb){c("yvar_name", "yvar_label")}, if(xb){c("xvar_name", "xvar_label")}, 
                                            if(fu){"fu_time"}, 
                                            "t_icdcat", "observed", "expected", "sir",
                                            if(collapse_ci == TRUE){"sir_ci"},
                                            if(collapse_ci == FALSE){c("sir_lci", "sir_uci")})),
                       tidyselect::everything()
    ) %>% 
    tidytable::arrange.(!!!final_sort_var_quo)
  
  #write attributes for error and warning messages
  if(length(problems_missing_ref_strata_attr > 0)){
    attr(sir_result, "problems_missing_ref_strata") <- problems_missing_ref_strata_attr
  }
  if(length(problems_missing_futime_attr > 0)){
    attr(sir_result, "problems_missing_futime") <- problems_missing_futime_attr
  }
  if(length(problems_not_empty_attr > 0)){
    attr(sir_result, "problems_not_empty") <- problems_not_empty_attr
  }
  if(length(problems_pyar_attr > 0)){
    attr(sir_result, "problems_pyar") <- problems_pyar_attr
  }
  
  
  sir_result <- sir_result %>%
    #create empty warning message
    tidytable::mutate.(warning = NA_character_) %>%
    #write warning of CHK_R2
    tidytable::mutate.(warning = tidytable::case.(
      .SD$observed > .SD$ref_inc_cases, paste(
        "This stratum contains observed cases in i_observed that do not occur in the refrates_df (ref_inc_cases).",
        "A possible explanation can be:",
        " * DCO cases",
        " * diagnosis of second cancer occured in different time period than first cancer"),
      default = .SD$warning
    ))
  
  #write attributes for matched strata
  attr(sir_result, "strata_var_names") <- strata_var_names
  
  pb$terminate()
  
  
  
  return(sir_result)
  
}

