#'
#' Calculate standardized incidence ratios with custom grouping variables stratified by follow-up time
#'
#' @param df dataframe in wide format
#' @param dattype can be "zfkd" or "seer" or empty. Will set default variable names from dataset.
#' @param ybreak_vars variables from df by which SIRs should be stratified in result df. Multiple variables will result in
#'                    appended rows in result df. 
#'                    Careful: do not chose any variables that are dependent on occurrence of count_var (e.g. Histology of second cancer).
#'                    If y_break_vars = "none", no stratification is performed. Default is "none".
#' @param xbreak_var One variable from df by which SIRs should be stratified as a second dimension in result df. This variable will be added as
#'                    a second stratification dimension to ybreak_vars and all variables will be calculated for subpopulations of x and y combinations. 
#'                    Careful: do not chose any variables that are dependent on occurrence of count_var (e.g. Year of second cancer).
#'                    If y_break_vars = "none", no stratification is performed. Default is "none".                
#' @param futime_breaks vector that indicates split points for follow-up time groups (in years) that will be used as xbreak_var.
#'                      Default is c(0, .5, 1, 5, 10, Inf) that will result in 5 groups (up to 6 months, 6-12 months, 1-5 years, 5-10 years, 10+ years). 
#'                      If you don't want to split by follow-up time, use futime_breaks = "none".
#' @param calc_total_row option to calculate a row of totals. Can be either FALSE for not adding such a row or TRUE for adding it at the first row. Default is TRUE.
#' @param calc_total_fu option to calculate totals for follow-up time. Can be either FALSE for not adding such a column or TRUE for adding. Default is TRUE.
#' @param count_var variable to be counted as observed case. Cases are usually the second cancers. Should be 1 for case to be counted.
#' @param refrates_df df where reference rate from general population are defined. It is assumed that refrates_df has the columns 
#'                  "region" for region, "sex" for biological sex, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "incidence_crude_rate" for incidence rate in the respective age/sex/year cohort.
#'                  refrates_df must use the same category coding of age, sex, region, year and t_site as age_var, sex_var, region_var, year_var and site_var. 
#' @param region_var variable in df that contains information on region where case was incident. Default is set if dattype is given.
#' @param age_var variable in df that contains information on age-group. Default is set if dattype is given.
#' @param sex_var variable in df that contains information on sex. Default is set if dattype is given.
#' @param year_var variable in df that contains information on year or year-period when case was incident. Default is set if dattype is given.
#' @param race_var optional argument for dattype="seer", if SIR should be calculated stratified by race. If you want to use this option, provide variable name of df that contains race information.
#' @param site_var variable in df that contains information on ICD code of case diagnosis. Cases are usually the second cancers. Default is set if dattype is given.
#' @param futime_var variable in df that contains follow-up time per person between date of first cancer and any of death, date of event (case), end of FU date (in years; whatever event comes first). Default is set if dattype is given.
#' @param alpha significance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @export
#'
#'

sir_byfutime <- function(df,
                         dattype = "zfkd",
                         ybreak_vars = "none",
                         xbreak_var = "none",
                         futime_breaks = c(0, .5, 1, 5, 10, Inf),
                         count_var,
                         refrates_df = rates,
                         calc_total_row = TRUE,
                         calc_total_fu = TRUE,
                         region_var = NULL,
                         age_var = NULL,
                         sex_var = NULL,
                         year_var = NULL,
                         race_var = NULL,    #optional when matching by race is wanted
                         site_var = NULL,
                         futime_var = NULL,
                         alpha = 0.05) {
  
  
  # ---- 0 function basics ----
  
  ## --- 0a setting default parameters
  na_explicit <- "zzz_NA_explicit" # string for explicit NAs
  
  ## --- 0b getting and setting names / preferences
  
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
  
  #race stratification option
  #check that race_var is only used with dattype = "seer"
  
  if(!is.null(race_var) & dattype != "seer"){
    rlang::inform(
      paste0("Only dattype 'seer' is compatible with stratification by race. \n",
             "You have used `dattype = \"", dattype, "\"` and `race_var = \"", race_var, "\"`\n",
             "Therefore race_var will be reset to 'none' and no stratification by race will be done.")
    )
    race_var <- NULL
  }
  
  if(!is.null(race_var) & dattype == "seer"){
    rs <- TRUE
  } else{
    rs <- FALSE
  }
  
  
  #check param calc_total
  if(!is.logical(calc_total_fu)){
    rlang::inform("\n Parameter `calc_total_fu` should be logical (TRUE or FALSE). Default `calc_total_fu = TRUE` will be used instead.")
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
    if (is.null(age_var)) {
      age_var <- rlang::sym("t_agegroup.1")
    } else{
      age_var <- rlang::ensym(age_var)
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
      site_var <- rlang::sym("t_site.2")
    } else{
      site_var <- rlang::ensym(site_var)
    }
    if (is.null(futime_var)) {
      futime_var <- rlang::sym("p_futimeyrs.1")
    } else{
      futime_var <- rlang::ensym(futime_var)
    }
    if(rs){
      race_var <- rlang::ensym(race_var)
    }
  }
  
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd") {
    if (is.null(region_var)) {
      region_var <- rlang::sym("p_region.1")
    } else{
      region_var <- rlang::ensym(region_var)
    }
    if (is.null(age_var)) {
      age_var <- rlang::sym("t_agegroupdiag.1")
    } else{
      age_var <- rlang::ensym(age_var)
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
      site_var <- rlang::sym("t_site.2")
    } else{
      site_var <- rlang::ensym(site_var)
    }
    if (is.null(futime_var)) {
      futime_var <- rlang::sym("p_futimeyrs.1")
    } else{
      futime_var <- rlang::ensym(futime_var)
    }
  }
  
  
  # create empty objects for possible warnings and errors
  
  problems_pyar_attr <- data.frame()
  problems_not_empty_attr <- data.frame()
  problems_missing_ref_strata_attr <- data.frame()
  problems_missing_futime_attr <- data.frame()
  problems_missing_count_strat_attr <- data.frame()
  problems_missing_fu_strat_attr <- data.frame()
  
  # create vector with basic matching variables age, sex, region, site_var, year
  
  strata_var_names <- c(rlang::as_string(age_var), rlang::as_string(sex_var), rlang::as_string(region_var), rlang::as_string(site_var), rlang::as_string(year_var))
  
  #add additional options for cohort calculations
  
  
  #CHK2: check whether all required variables are defined and present in dataset
  defined_vars <-
    c(
      rlang::as_string(region_var),
      rlang::as_string(age_var),
      rlang::as_string(sex_var),
      rlang::as_string(year_var),
      rlang::as_string(site_var),
      rlang::as_string(count_var),
      rlang::as_string(futime_var),
      if(rs){rlang::as_string(race_var)},
      if(yb){ybreak_var_names},
      if(xb){xbreak_var_names}
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
          "\n [INFO FU time missing] There are ", nrow(problems_missing_futime), 
          "rows in the data set for which futime_var is missing. \n", 
          "Please make sure that you have: \n", 
          " - calculated FU time for all cases where the index event occured and \n", 
          " - have removed all cases from the dataset that do not count at baseline. \n \n",
          paste0(utils::capture.output(problems_missing_futime), collapse = "\n"),
          "\n Check attribute `problems_missing_futime` of results to see what strata are affected."        )
      )
      problems_missing_futime_attr <- tidytable::bind_rows.(problems_missing_futime_attr, problems_missing_futime, fill=TRUE)
    }
    
  }
  
  #Check that count_var is in correct format
  
  if(!is.numeric(df[[rlang::as_name(count_var)]])){
    rlang::abort(  
      paste0("The column defined in `count_var` is not numeric. \n",
             "You have used `count_var = \"", rlang::as_name(count_var), "\"`\n",
             "Please make sure that the column of df defined as `count_var` is numeric and coded 1 for observed cases.")
    )
  }
  
  if(!( c(1) %in% (unique(df[[rlang::as_name(count_var)]])))){
    rlang::warn(  
      paste0("The column defined in `count_var` does not contain any rows where count_var == 1. So no observed cases are found. \n",
             "You have used `count_var = \"", rlang::as_name(count_var), "\"`\n",
             "Please make sure that the column of df defined as `count_var` is numeric and coded 1 for observed cases.")
    )
  }
  
  
  
  # ---- 1 data modifications ----
  
  ## --- 1a: prepare df
  
  # remove columns from data.frame that is not needed to safe memory
  df <- df %>%
    tidytable::select.(!!!rlang::syms(defined_vars))
  
  # change factors to character to avoid warning messages
  df <- df%>%
    tidytable::mutate_across.(.cols = where(is.factor), .fns = as.character)
  
  # remove all labels from df to avoid warning messages
  df[] <- lapply(df, function(x) { attributes(x) <- NULL; x })
  
  
  #make all important variables characters and make NAs explicit (for better matching)
  df <- df %>%
    tidytable::mutate.(
      age = as.character(!!age_var),
      sex = as.character(!!sex_var),
      region = as.character(!!region_var),
      year = as.character(!!year_var),
      t_site = as.character(!!site_var)) %>%
    tidytable::mutate_across.(.cols = c(age, sex, region, year, t_site), 
                              .fns = ~tidytable::replace_na.(., na_explicit))
  
  #prepare df for race stratification if needed
  if(rs){
    df <- df %>%
      tidytable::mutate.(
        race = as.character(!!race_var)) %>%
      tidytable::mutate_across.(.cols = c(race), 
                                .fns = ~tidytable::replace_na.(., na_explicit))
  }
  
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
  
  #get used age, sex, region, year, t_site
  
  used_age <- unique(df$age)
  used_sex <- unique(df$sex)
  used_region <- unique(df$region)
  used_year <- unique(df$year)
  used_t_site<- unique(df$t_site)
  if(rs){
    used_race <- unique(df$race)
  } else {
    used_race <- "none"
  }
  
  ## --- 1b: prepare calc_total_row option
  
  if(!is.logical(calc_total_row)){
    rlang::inform("\n Parameter `calc_total_row` should be logical (TRUE or FALSE). Default `calc_total_row = TRUE` will be used instead.")
    calc_total_row <- TRUE
  }
  
  if(calc_total_row == TRUE){
    
    #create a new variable total_var that is constant for giving a total for all rows in df when grouping
    df <- df %>%
      tidytable::mutate.(total_var = "Overall")
    
    #add total_var to ybreak_vars
    if(yb){
      length_yb <- length_yb + 1
      
      ybreak_var_names <- c("total_var", ybreak_var_names) #add total before all other ybreak_vars
      
    } else{
      yb <- TRUE
      length_yb <- 1
      ybreak_var_names <- c("total_var")
    }
  }
  
  ## --- 1c: prepare futime
  
  
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
    
    chk_na <- df %>% tidytable::filter.(is.na(rlang::eval_tidy(!!futime_var))) %>% nrow()
    
    if (chk_na > 0) {
      warning(
        paste0(
          "The variable for follow-up time has: ", chk_na, " missings. These will be omitted when creating the crosstabs.")
      )
    }
    
    chk_futimegroups <- df %>% tidytable::filter.(is.na(futimegroup)) %>% nrow()
    
    if (chk_futimegroups > 0) {
      rlang::warn(
        paste0(
          "There are: ", chk_futimegroups, " cases that do not belong to a follow-up time group. \n",
          "These cases will be omitted when calculating sir_byfutime and the totals. \n",
          "It is recommeded to either: \n",
          " - filter cases by futime_var that are out of the range of futime_breaks or \n",
          " - adjust futime_breaks so that it's range includes all available fu_times.")
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
  
  ## --- 1d: prepare refrates_df
  
  #make factor variables to character for better matching
  refrates_df <- refrates_df %>%
    tidytable::mutate_across.(.cols = where(is.factor), .fns = as.character)
  
  #remove attributes of refrates_df for better matching
  refrates_df[] <- lapply(refrates_df, function(x) { attributes(x) <- NULL; x })
  
  
  #refrates_df -> filter lines that are not needed in age, sex, region, year, but do not filter for t_site
  refrates_df <- refrates_df %>%
    tidytable::filter.(age %in% !!used_age,
                       sex %in% !!used_sex,
                       region %in% !!used_region,
                       year %in% !!used_year)
  
  #prepare for race stratification option, if rs=TRUE
  if(rs){
    ##get available race levels from refrates and compare to used in df
    available_race <- unique(refrates_df$race)
    miss_race <- used_race[!used_race %in% available_race]
    ##take precautions for missing race data in df
    if(length(miss_race > 0)){
      rlang::inform(
        paste0("\n The following values for race_var present in the data, is not availabe in refrates_df: \n \n",
               " - ", miss_race, "\n \n",
               "It is recommeded to clean race_var before running this function. \n",
               "For all missing reference levels of race, data will be matched to the category 'Total' in refrates_df.")
      )
      refrates_total <- refrates_df %>%
        tidytable::filter.(substr(race, 1, 5) == "Total")
      #replicate refrates_total for each miss_race value and rowbind to refrates_df
      for(lev in miss_race){
        refrates_df <- refrates_total %>%
          tidytable::mutate.(race = !!lev) %>%
          tidytable::bind_rows.(refrates_df)
      }
      rm(refrates_total)
    }
    ##filter refrates to used_race
    refrates_df <- refrates_df %>%
      tidytable::filter.(race %in% !!used_race)
  }
  
  
  #SEER only, if no race stratification is used, filter refrates so that only totals remain
  if(!rs & dattype == "seer"){
    refrates_df <- refrates_df %>%
      tidytable::filter.(substr(race, 1, 5) == "Total")
  }
  
  #getting all ICD codes from refrates_df
  refrates_site_all <- refrates_df %>%
    tidytable::select.(t_site) %>%
    tidytable::filter.(substr(t_site, 1, 5) != "Total") %>% #removing icd codes that are not based on 3 digits (e.g. Total categories)
    unique(.) %>%
    purrr::pluck(1)
  
  
  #add missing levels that are found in data, but missing from refrates_df
  if(length(used_t_site[!(used_t_site %in% refrates_site_all)]) > 0){
    refrates_site_all <- c(refrates_site_all, used_t_site[!(used_t_site %in% refrates_site_all)])
  }
  
  
  
  
  # ---- 2 analysis - refrates option ----
  ### F2 Calculating Observed by group (within cohort) and PYARs
  
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
      
      
      #F2b calculate observed
      sircalc_count <- df %>%
        tidytable::mutate.(count_var_new = tidytable::case.(
          !!fu_tot_f & ((!!count_var) == 1) & ((!!futime_var) >= !!futime_breaks[1]) & ((!!futime_var) < !!futime_breaks[length(!!futime_breaks)]), 1, #for iteration fu_tot_f count_var is 1 if it occurs between first and last futime_break
          ((!!count_var) == 1) & ((!!futime_var) >= !!futime_breaks[f]) & ((!!futime_var) < !!futime_breaks[f+1]), 1, #otherwise only count cases occurring between futime_breaks
          default = 0)) %>% 
        tidytable::summarize.(i_observed = sum(.SD$count_var_new, na.rm = TRUE), 
                              .by = tidyselect::all_of(c("age", "sex", "region", "year", "t_site",
                                                         if(rs){"race"},
                                                         if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)}, 
                                                         rlang::as_string(fub_var))))
      
      complete_vars_quo <- rlang::syms(c("age", "sex", "region", "year", 
                                         if(rs){"race"},
                                         if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)}))
      
      sircalc_count <- sircalc_count %>% #complete groups where i_observed = 0
        tidytable::filter.(!!fub_var == 1) %>%  #remove category fub_var == 0, which does not apply #incompatiblity with fu=FALSE (unclear, how to do conditional filtering)
        tidytable::complete.(., !!!complete_vars_quo, t_site = !!refrates_site_all) %>%
        tidytable::mutate.(
          i_observed = tidytable::case.(is.na(i_observed), 0,
                                        default = i_observed),
          !!fub_var := 1) 
      
      #for ybreak_var: make NAs explicit
      if(yb & !xb){
        sircalc_count <- sircalc_count %>% 
          tidytable::mutate_across.(.cols = !!syb_var, .fns = ~tidytable::replace_na.(., na_explicit))
      }
      
      #for xbreak_var: make NAs explicit
      if(!yb & xb){
        sircalc_count <- sircalc_count %>% 
          tidytable::mutate_across.(.cols = !!sxb_var, ~tidytable::replace_na.(., na_explicit))
      }
      
      #for ybreak_vars and xbreak_var: make NAs explicit
      if(yb & xb){
        sircalc_count <- sircalc_count %>% 
          tidytable::mutate_across.(.cols = c(!!syb_var, !!sxb_var), ~tidytable::replace_na.(., na_explicit))}
      
      
      #F2c person-years at risk
      sircalc_fu <- df %>%
        tidytable::mutate.(
          futime_var_new = tidytable::case.(!!fu_tot_f & ((!!futime_var) < !!futime_breaks[1]), 0,
                                            !!fu_tot_f & ((!!futime_var) < !!futime_breaks[length(futime_breaks)]), ((!!futime_var) - !!futime_breaks[1]),
                                            !!fu_tot_f & ((!!futime_var) >= !!futime_breaks[length(futime_breaks)]), ((!!futime_breaks[length(futime_breaks)]) - !!futime_breaks[1]),
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
                                     if(rs){"race"},
                                     if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)}, 
                                     rlang::as_string(fub_var))))
      
      sircalc_fu <- sircalc_fu %>% #only keep relevant groups and fill NA with 0
        tidytable::filter.(!!fub_var == 1) %>%  #remove category fub_var == 0, which does not apply #incompatiblity with fu=FALSE (unclear, how to do conditional filtering)
        tidytable::mutate.(
          i_pyar = tidytable::case.(is.na(i_pyar), 0,
                                    default = i_pyar),
          n_base = tidytable::case.(is.na(n_base), 0,
                                    default = n_base))
      
      #fill missing syb_var and sxb_var categories with explicit "missing"
      if(yb){
        sircalc_fu <- sircalc_fu %>% 
          tidytable::mutate.(
            !!syb_var := tidytable::case.(is.na(rlang::eval_tidy(!!syb_var)), "missing",
                                          default = !!syb_var))
        
        used_sybvar <- unique(sircalc_fu[[rlang::as_string(syb_var)]])
      } else {
        used_sybvar <- "none"
      }
      
      if(xb){
        sircalc_fu <- sircalc_fu %>% 
          tidytable::mutate.(
            !!sxb_var := tidytable::case.(is.na(rlang::eval_tidy(!!sxb_var)), "missing",
                                          default = !!sxb_var))
        
        used_sxbvar <- unique(sircalc_fu[[rlang::as_string(sxb_var)]])
      } else {
        used_sxbvar <- "none"
      }
      
      #CHK_sircalc_n - Check that all combinations of age, sex, region, year, t_site, syb_var, sxb_var are present in sircalc_count and sircalc_fu
      
      #required number of strata in counts is product of distinct values found in df for age, sex, region, year, syb, sxb, race multiplied with all t_sites from refrates_df
      n_strata_required_count <- length(unique(sircalc_count$age)) * 
        length(unique(sircalc_count$sex)) * length(unique(sircalc_count$region)) * 
        length(unique(sircalc_count$year)) * length(refrates_site_all)
      
      if(rs){
        n_strata_required_count <- n_strata_required_count * length(unique(sircalc_count$race))
      }
      
      if(xb){
        n_strata_required_count <- n_strata_required_count * length(unique(sircalc_count[[rlang::as_string(sxb_var)]]))
      }
      
      if(yb){
        n_strata_required_count <- n_strata_required_count * length(unique(sircalc_count[[rlang::as_string(syb_var)]]))
      }
      
      #required number of strata in fu is distinct combinations of age, sex, region, year, syb, sxb, race found in df
      n_strata_required_fu <- df %>%
        tidytable::filter.(!!fub_var == 1) %>%
        tidytable::distinct.(tidyselect::all_of(c("age", "sex", "region", "year", 
                                                  if(rs){"race"},
                                                  if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)}))) %>%
        nrow()
      
      
      #not found strata in sircalc_fu
      n_not_found_fu <- n_strata_required_count - (n_strata_required_fu * length(refrates_site_all))
      
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
            "The calculation of follow-up time was performed for: ", nrow(sircalc_fu), " strata. However ", n_strata_required_fu, " strata are required. Occured in: ", fub_var,",", syb_var)
        )
      }
      
      
      #F2d merge
      
      #prepare merging
      
      #vector of matching variables in join functions
      match_vars <- c("age", "sex", "region", "year", if(rs){"race"}, 
                      if(yb){rlang::as_string(syb_var)}, if(xb){rlang::as_string(sxb_var)}, rlang::as_string(fub_var))
      
      
      #check that there are no conflicting rows with regard to matching variables for join between fu and count
      n_dist_sircalc_fu <- sircalc_fu %>% 
        tidytable::distinct.(tidyselect::all_of(match_vars)) %>%
        nrow()
      
      if (n_dist_sircalc_fu != nrow(sircalc_fu)) {
        rlang::warn(
          "\nThere are disambiguities in matching the follow-up time to the observed count strata!"
        )
      }
      
      #merge sircalc_count and sircalc_fu
      sircalc <- sircalc_count %>%
        tidytable::full_join.(sircalc_fu, by = match_vars)
      
      #CHK2d check that merge worked as intended with all sircalc_count strata finding a match in sircalc_fu
      
      ##CHK2d-1 no unmatched sircacl_count strata
      if(nrow(sircalc) != nrow(sircalc_count)){
        #missing strata
        missing_count_strat <- sircalc_fu %>%
          tidytable::anti_join.(sircalc_count, by = match_vars)
        #warn
        rlang::warn(paste0("\n [WARN Merge of cases and FU mismatch] When trying to match the observed counts and follow-up times for this loop, an unexpected mismatch of strata occured. \n",
                           "The following strata are missing from intermediate result `sircalc_count`. \n",
                           paste0(utils::capture.output(missing_count_strat), collapse = "\n"), 
                           " \n",
                           "This error occured in: \n",
                           " - Time stratum: ", rlang::as_string(fub_var), "\n",
                           " - Y variable stratum: ", rlang::as_string(syb_var), "\n",
                           "\n Check attribute `missing_count_strat` of results to see what strata are affected.",
                           "It is recommended to run a debug with the same data."))
        
        missing_count_strat_attr <- tidytable::bind_rows.(missing_count_strat_attr, missing_count_strat, fill=TRUE)
      }
      
      ##CHK2d-2 All available sircalc_fu strata were matched
      n_i_pyar_miss <- sircalc %>% tidytable::select.(i_pyar) %>% tidytable::filter.(is.na(i_pyar)) %>% nrow()
      
      if(n_not_found_fu != n_i_pyar_miss){
        #missing strata
        missing_fu_strat <- sircalc_count %>%
          tidytable::anti_join.(sircalc_fu, by = match_vars)
        #warn
        rlang::warn(paste0("\n [WARN Merge of cases and FU mismatch] When trying to match the observed counts and follow-up times for this loop, an unexpected mismatch of strata occured. \n",
                           "The following strata are missing from intermediate result `sircalc_fu`. \n",
                           paste0(utils::capture.output(missing_fu_strat), collapse = "\n"), 
                           " \n",
                           "This error occured in: \n",
                           " - Time stratum: ", rlang::as_string(fub_var), "\n",
                           " - Y variable stratum: ", rlang::as_string(syb_var), "\n",
                           "\n Check attribute `problems_missing_fu_strat` of results to see what strata are affected.",
                           "It is recommended to run a debug with the same data."))
        
        missing_fu_strat_attr <- tidytable::bind_rows.(missing_fu_strat_attr, missing_fu_strat, fill=TRUE)
      }
      
      rm(sircalc_count, sircalc_fu)
      
      
      #some missings in t_site are expected after merge for those strata where no observed case occurred
      #make NAs in t_site in sircalc explicit
      sircalc <- sircalc %>% 
        tidytable::mutate_across.(.cols = c(t_site), .fns = ~tidytable::replace_na.(., na_explicit))
      
      
      #all strata that have missing i_pyar are not in df and therefore i_pyar = 0 and n_base = 0 
      
      sircalc <- sircalc %>% #complete groups where i_observed = 0
        tidytable::mutate.(
          i_pyar = tidytable::case.((is.na(i_pyar) & i_observed == 0), 0,
                                    default = i_pyar),
          n_base = tidytable::case.((is.na(n_base) & i_observed == 0), 0,
                                    default = n_base))
      
      
      #CHK2d-3: make check for unexpected discrepancies
      
      problems_not_empty <- sircalc %>%
        tidytable::filter.(i_pyar == 0 & i_observed != 0) 
      
      if (nrow(problems_not_empty) > 0) {
        rlang::inform(paste0("\n [INFO Cases with 0 PYARs] There are disambiguities where strata with 0 follow-up time have data in observed. \n",
                             paste0(utils::capture.output(problems_not_empty), collapse = "\n"), 
                             " \n",
                             "Check attribute `problems_not_empty` of results to see what strata are affected. \n",
                             "This might be caused by cases where SPC occured at the same day as first cancer. \n",
                             "You can check this by excluding all cases from wide_df, where date of first diagnosis is equal."))
        problems_not_empty_attr <- tidytable::bind_rows.(problems_not_empty_attr, problems_not_empty, fill=TRUE) #save information to write as attribute later
      }
      
      
      #filter empty rows for non-matched site from sircalc_count
      sircalc <- sircalc %>%
        tidytable::filter.(!(t_site == !!na_explicit & i_observed == 0)) %>%
        #remove all lines with 0 information (0 observed and 0 follow-up time)
        tidytable::filter.(!(i_pyar == 0 & i_observed == 0))
      
      
      ### F2e: Merging reference rates (for refpop only) by t_site, region, year, sex and age 
      
      #CHK2e-1: reporting used regions and whether they can be found in rates
      
      if(f == 1 & y == 1){
        
        used_strata <- sircalc %>%
          tidytable::distinct.(tidyselect::all_of(c("age", "sex", "region", "year", if(rs){"race"}, "t_site"))) %>%
          tidytable::filter.(!is.na(t_site))
        
        missing_ref_strata <- used_strata %>%
          tidytable::anti_join.(refrates_df, by = c("age", "sex", "region" , "year", if(rs){"race"}, "t_site"))
        
        if(nrow(missing_ref_strata) > 0){
          rlang::inform(paste0("\n [INFO Refrates missing]For the following age groups, sex, regions, years, t_sites no reference rates can be found: \n",
                               paste0(utils::capture.output(missing_ref_strata), collapse = "\n"), 
                               " \n",
                               "Check attribute `problems_missing_ref_strata` of results to see what strata are affected. \n",
                               "Solution could be to add these strata to refrates_df. \n"))
          problems_missing_ref_strata_attr <- tidytable::bind_rows.(problems_missing_ref_strata_attr, missing_ref_strata, fill = TRUE)
        }
      }
      
      
      #F2e: Do merge
      
      sir_or <- sircalc %>%
        tidytable::left_join.(refrates_df, by = c("age", "sex", "region" , "year", if(rs){"race"}, "t_site")) %>%
        tidytable::select.(-tidyselect::any_of(c("comment", "population_n_per_year")))
      
      rm(sircalc)
      
      
      
      ### F2f Calculating Expected by group (from reference rates) [refrates]
      
      sir_basic <- sir_or %>%
        tidytable::mutate.(
          i_expected = .SD$i_pyar * .SD$incidence_crude_rate / 100000
        )
      
      rm(sir_or)
      
      
      
      ### F2g Making SIR calculations 
      
      
      #SIR and Confidence intervals using calculation methods by @breslowStatisticalMethodsCancer1987
      ##F2g-1: calculating SIR and confidence intervals
      sir_longresult_strat_f <- sir_basic %>%
        tidytable::mutate.(
          sir = .SD$i_observed / .SD$i_expected,
          sir_lci = (stats::qchisq(p = !!alpha / 2, df = 2 * .SD$i_observed) / 2) / .SD$i_expected,
          sir_uci = (stats::qchisq(p = 1 - !!alpha / 2, df = 2 * (.SD$i_observed + 1)) / 2) / .SD$i_expected
        )
      
      rm(sir_basic)
      
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
      rm(sir_longresult_strat_f)
      
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
        tidytable::select.(yvar_name, yvar_label, tidyselect::everything())
    }
    
    if(xb & yb){
      
      sir_longresult_strat <- sir_longresult_strat %>%
        tidytable::mutate.(yvar_name = !!syb_var_name,
                           yvar_sort = !!y,
                           xvar_name = !!sxb_var_name,
                           xvar_sort = !!x) %>%
        tidytable::rename.(yvar_label = !!syb_var,
                           xvar_label = !!sxb_var)  %>%
        tidytable::select.(yvar_name, yvar_label, xvar_name, xvar_label, tidyselect::everything())
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
    rm(sir_longresult_strat)
    #end loop [y] iterations
    gc()
  }
  
  
  
  ### F2h: Restructuring results
  
  #checking results 
  #CHK_R1 - PYARS should be the same for all age, sex, year, region groups
  
  problems_pyar <- sir_longresult %>% 
    tidytable::summarize.(
      min_pyar = min(.SD$i_pyar),
      max_pyar = max(.SD$i_pyar),
      .by = tidyselect::all_of(c("yvar_name", "yvar_label", "age", "sex", "region", "year", if(rs){"race"},
                                 if(fu){"fu_time"}))) %>% 
    tidytable::filter.(min_pyar != max_pyar)
  
  if(nrow(problems_pyar) > 0){
    rlang::inform(paste0("\n  [INFO Multiple refrates matches] There are differing pyar values for the same age, sex, year, region strata: \n",
                         paste0(utils::capture.output(problems_pyar), collapse = "\n"), 
                         "\n Check attribute `problems_pyar_attr` of results to see what strata are affected. \n"))
    problems_pyar_attr <- tidytable::bind_rows.(problems_pyar_attr, problems_pyar, fill=TRUE) #save information to write as attribute later
  }
  
  
  #CHK_R2 - observed cases should also occur in reference rates dataset
  
  notes_refcases <- sir_longresult %>% 
    tidytable::filter.(i_observed > incidence_cases)
  
  if(nrow(notes_refcases) > 0){
    rlang::inform(paste0("\n There are observed cases in the results file that do not occur in the refrates_df. \n",
                         "A possible explanation can be: \n",
                         " - DCO cases \n",
                         " - diagnosis of second cancer occured in different time period than first cancer \n",
                         "The following strata are affected: \n",
                         paste0(utils::capture.output(notes_refcases), collapse = "\n"),
                         "\n",
                         "\n Check attribute `notes_refcases` of results to see what strata are affected. \n"))
  }
  
  #final dataset should have the structure: columns
  #t_site #yvars(1-y) #xvar1 #xvar2 #xvar3 ..#xvarx _ #n_base #observed #expected #pyar #sir #sir_uci #sir_lci
  
  
  #vi) rename vars
  
  sir_result_pre <- sir_longresult %>%
    tidytable::rename.(observed = i_observed,
                       expected = i_expected,
                       pyar = i_pyar,
                       ref_inc_cases = incidence_cases,
                       ref_population_pyar = population_pyar,
                       ref_inc_crude_rate = incidence_crude_rate)
  
  rm(sir_longresult)
  pb$tick()
  Sys.sleep(3 / 100)
  
  #5d rounding
  
  sir_result_pre <- sir_result_pre %>%
    tidytable::mutate_across.(.cols = c(pyar, sir, sir_lci, sir_uci), 
                              .fns = ~round(.,2))
  
  #collapse_ci option
  
  
  ### F2i: labeling and returning results
  
  
  #since tidytable::arrange.() does not support tidyselect, we need to create a list of symbols to pass on
  final_sort_var_quo <- rlang::syms(c("age", "region", "sex", if(rs){"race"}, "year", 
                                      if(yb){c("yvar_sort", "yvar_label")}, 
                                      if(xb){c("xvar_name", "xvar_label")}, 
                                      if(fu){"fu_time_sort"}, "t_site"))
  
  sir_result <- sir_result_pre %>%
    tidytable::select.(tidyselect::any_of(c("age", "region", "sex", if(rs){"race"}, "year", 
                                            if(yb){c("yvar_name", "yvar_label")}, if(xb){c("xvar_name", "xvar_label")}, 
                                            if(fu){"fu_time"}, 
                                            "t_site", "observed", "expected", "sir", "sir_lci", "sir_uci")),
                       tidyselect::everything()) %>% 
    tidytable::arrange.(!!!final_sort_var_quo)
  
  
  rm(sir_result_pre)
  
  #implement check for missing observed in refrates_df
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
  
  #write attributes for error and warning messages
  if(length(problems_missing_ref_strata_attr > 0)){
    attr(sir_result, "problems_missing_ref_strata") <- problems_missing_ref_strata_attr
  }
  if(length(problems_missing_futime_attr > 0)){
    attr(sir_result, "problems_missing_futime") <- problems_missing_futime_attr
  }
  if(length(problems_missing_count_strat_attr > 0)){
    attr(sir_result, "problems_missing_count_strat") <- problems_missing_count_strat_attr
  }
  if(length(problems_missing_fu_strat_attr > 0)){
    attr(sir_result, "problems_missing_fu_strat") <- problems_missing_fu_strat_attr
  }
  if(length(problems_not_empty_attr > 0)){
    attr(sir_result, "problems_not_empty") <- problems_not_empty_attr
  }
  if(length(problems_pyar_attr > 0)){
    attr(sir_result, "problems_pyar") <- problems_pyar_attr
  }
  if(length(notes_refcases > 0)){
    attr(sir_result, "notes_refcases") <- notes_refcases
  }
  
  
  pb$terminate()
  
  return(sir_result)
  
}
