#' Calculate standardized incidence ratios with costum grouping variables by follow-up time
#'
#' @param df dataframe in wide format
#' @param dattype can be "zfkd" or "seer" or empty. Will set default variable names from dataset.
#' @param expcount_src can bei either "refrates" or "cohort"
#' @param summarize_groups option to define summarizing stratified groups. Default is "none". 
#'                 If you want to define variables that should be summarized into one group, you can chose from region_var, sex_var, year_var. 
#'                 Define multiple summarize variables by summarize_groups = c("region", "sex", "year")
#' @param count_var variable to be counted as observed case. Should be 1 for case to be counted.
#' @param xbreak_vars variables from df by which rates should be stratified in columns of result df. Default is "none".
#' @param ybreak_vars variables from df by which rates should be stratified in rows of result df. Multiple variables will result in
#'                    appended rows in result df. Default is "none".
#' @param futime_breaks vector that indicates split points for follow-up time groups (in months) that will be used as xbreak_var.
#'                      Default is c(6, 12, 60, 120) that will result in 5 groups (up to 6 months, 6-12 months, 1-5 years, 5-10 years, 10+ years)
#' @param collapse_ci If TRUE upper and lower confidence interval will be collapsed into one column separated by "-". Default is FALSE.
#' @param add_total option to add a row of totals. Can bei either "no" for not adding such a row or "top" or "bottom" for adding it at the first or last row. Default is "no".
#' @param stdpop_df df where standard population is defined. It is assumed that stdpop_df has the columns "sex" for gender, "age" for age-groups,
#'                  "standard_pop" for name of standard population (e.g. "European Standard Population 2013) and "population_n" for size of standard population age-group.
#'                  stdpop_df must use the same category coding of age and sex as agegroup_var and sex_var.
#' @param refpop_df df where reference population data is defined. Only required if option futime = "refpop" is chosen. It is assumed that refpop_df has the columns 
#'                  "region" for region, "sex" for gender, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "population_pyar" for person-years at risk in the respective age/gender/year cohort.
#'                  refpop_df must use the same category coding of age, sex, region, year and icdcat as agegroup_var, sex_var, region_var, year_var and icdcat_var. 
#' @param refrates_df df where reference rate from general population are defined. It is assumed that refrates_df has the columns 
#'                  "region" for region, "sex" for gender, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "incidence_crude_rate" for incidence rate in the respective age/gender/year cohort.
#'                  refrates_df must use the same category coding of age, sex, region, year and icdcat as agegroup_var, sex_var, region_var, year_var and icdcat_var. 
#' @param region_var variable in df that contains information on region where case was incident. Default is set if dattype is given.
#' @param agegroup_var variable in df that contains information on age-group. Default is set if dattype is given.
#' @param sex_var variable in df that contains information on gender. Default is set if dattype is given.
#' @param year_var variable in df that contains information on year or year-period when case was incident. Default is set if dattype is given.
#' @param std_pop can be either "ESP2013, ESP1976, WHO1960. Only applies to expcount_src = "cohort".
#' @param icdcat_var variable in df that contains information on ICD code of case diagnosis. Default is set if dattype is given.
#' @param futime_var variable in df that contains follow-up time per person (in years) in cohort (can only be used with futime_src = "cohort"). Default is set if dattype is given.
#' @param pyar_var variable in refpop_df that contains person-years-at-risk in reference population (can only be used with futime_src = "refpop") Default is set if dattype is given.
#' @param alpha signifcance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @return df
#' @importFrom rlang .data
#' @export
#'
#'



sir_byfutime <- function(df,
                dattype = "zfkd",
                expcount_src = "refrates",
                xbreak_vars = "none",
                ybreak_vars = "none",
                summarize_groups = "none",
                futime_breaks = c(6, 12, 60, 120),
                count_var,
                refrates_df = rates,
                add_total = "no",
                collapse_ci = FALSE,
                stdpop_df = standard_population, #optional for indirect standardization
                refpop_df = population,        #optional for indirect standardization
                std_pop = "ESP2013",           #optional for indirect standardization
                region_var = NULL,
                agegroup_var = NULL,
                sex_var = NULL,
                year_var = NULL,
                icdcat_var = NULL,
                futime_var = NULL,
                pyar_var = NULL,              #optional for indirect standardization
                alpha = 0.05) {
  
  
  ###----  prepwork
  
  #CHK1: check if df exists and is dataframe
  
  if(is.data.frame(get("df"))){}
  else{
    rlang::abort(paste0("The following df you provdided is not a dataframe: ", rlang::quo_name(df)))
  }
  
  # getting and setting names / preferences
  
  count_var <- rlang::ensym(count_var)
  
  if(ybreak_vars[1] != "none"){
    yb <- TRUE
    ybreak_vars <- rlang::enquo(ybreak_vars)
    ybreak_var_names <- rlang::eval_tidy(ybreak_vars)
    length_yb <- length(ybreak_var_names)
  } else{
    yb <- FALSE
    length_yb <- 1
  }
  
  if(xbreak_vars[1] != "none"){
    xb <- TRUE
    xbreak_vars <- rlang::enquo(xbreak_vars)
    xbreak_var_names <- rlang::eval_tidy(xbreak_vars)
    length_xb <- length(xbreak_var_names)
  } else{
    xb <- FALSE
    length_xb <- 1}
  
  if(summarize_groups[1] != "none"){
    sg <- TRUE
    sg_vars <- rlang::enquo(summarize_groups)
    sg_var_names <- rlang::eval_tidy(summarize_groups)
  } else{sg <- FALSE}
  
  
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
    if (is.null(icdcat_var)) {
      icdcat_var <- rlang::sym("t_icdcat.2")
    } else{
      icdcat_var <- rlang::ensym(icdcat_var)
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
    if (is.null(icdcat_var)) {
      icdcat_var <- rlang::sym("t_icdcat.2")
    } else{
      icdcat_var <- rlang::ensym(icdcat_var)
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
    dplyr::mutate_if(is.factor, as.character)
  
  
  if(expcount_src == "refrates"){
    refrates_df <- refrates_df %>%
      dplyr::mutate_if(is.factor, as.character) 
  }
  
  if(expcount_src == "cohort"){
    stdpop_df <- stdpop_df %>%
      dplyr::mutate_if(is.factor, as.character)
  }
  
  if(expcount_src == "cohort"){
    refpop_df <- refpop_df %>%
      dplyr::mutate_if(is.factor, as.character)
  }
  
  #add additional options for cohort calulations
  
  
  #CHK2: check whether all required variables are defined and present in dataset
  defined_vars <-
    c(
      rlang::quo_name(region_var),
      rlang::quo_name(agegroup_var),
      rlang::quo_name(sex_var),
      rlang::quo_name(year_var),
      rlang::quo_name(icdcat_var),
      rlang::quo_name(count_var),
      rlang::quo_name(futime_var)
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
  
  
  
  
  #####---- doing everything for the first y
  
  #1a: prepare df
  
  df <- df %>%
    dplyr::mutate(
      age = as.character(!!agegroup_var),
      sex = as.character(!!sex_var),
      region = as.character(!!region_var),
      year = as.character(!!year_var),
      t_icdcat = as.character(!!icdcat_var))
  
  #1b: prepare add_total option
  
  if(add_total == "top" | add_total == "bottom"){
    
    df <- df %>%
      dplyr::mutate(total_var = "Overall")
    
    length_yb = length_yb + 1
    
    if(add_total == "top") {
      ybreak_var_names <- c("total_var", ybreak_var_names)
    }
    
    if(add_total == "bottom") {
      ybreak_var_names <- c(ybreak_var_names, "total_var")
    }
  }
  
  #1c: prepare futime
  
  df <- df %>%
    dplyr::mutate(futimegroup = cut(!!futime_var, breaks = futime_breaks, right = FALSE))
  
  futimegroup_lev_months <- levels(df$futimegroup)
  
  # capture and change levels of new grouped variable
  futimegroup_levels <- levels(df$futimegroup) %>% 
    stringr::str_replace("\\[0,", "to ") %>% 
    stringr::str_replace(",Inf\\)", "\\+ months") %>% 
    stringr::str_replace("\\[", "") %>% 
    stringr::str_replace(",", "-")  %>% 
    stringr::str_replace("\\)", " months") %>% 
    stringr::str_replace("12-60 months", "1-5 years") %>% 
    stringr::str_replace("60-120 months", "5-10 years") %>% 
    stringr::str_replace("120\\+ months", "10\\+ years")
  
  #assign levels
  levels(df$futimegroup) <- futimegroup_levels
  
  for (lv in 1:nlevels(dn$futimegroup)){
    df_n[paste0(futimegroup_levels[lv])] <- ifelse(as.numeric(df$futimegroup) >= lv, 1, 0)
  }

  #CHK - check that there are no missing values for futimegroups
  
  chk_na <- df_n %>% dplyr::filter(is.na(.data$futimegroup)) %>% nrow()
  
  if (chk_na > 0) {
    warning(
      paste0(
        "The variable for follow-up time has: ", chk_na, " missings. These will be omitted when creating the crosstabs.")
    )
  }

  fu_var_names <- futimegroup_levels
  fu_vars <- rlang::syms(fu_var_names)  
  fu <- TRUE
  
  ### F1 Calculating Observed by group (within cohort) and PYARs
  
  
  
  for(y in 1:length_yb){
    
    if(yb){
      syb_var <- rlang::sym(ybreak_var_names[y])
    }
    if(xb){
      sxb_var <- rlang::sym(xbreak_var_names[x])
    }
    if(fu){
      fub_var <- rlang::sym(fu_var_names[f])
    }
    
    
    #F1b observed
    sircalc_count <- df %>%
      {if (fu){dplyr::mutate(!!count_var := dplyr::case_when(.data[!!count_var] == 1 & .data[!!futime_var] > futime_breaks[f] & .data[!!futime_var] < futime_breaks[f+1] ~ 1,
                                             TRUE ~ 0))} else{.}} %>%
      {if (!xb & !yb & !fu){dplyr::group_by(., .data$age, .data$sex, .data$region, .data$year, .data$t_icdcat)} else{.}} %>%
      {if (!xb & yb & !fu){dplyr::group_by(., .data$age, .data$sex, .data$region, .data$year, .data$t_icdcat, !!syb_var)} else{.}} %>%
      {if (!xb & yb & fu){dplyr::group_by(., .data$age, .data$sex, .data$region, .data$year, .data$t_icdcat, !!syb_var, !!fub_var)} else{.}} %>%
      dplyr::summarize(i_observed = sum(!!count_var, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    sircalc_count <- sircalc_count %>% #complete groups where i_observed = 0
      {if (!xb & !yb){tidyr::complete(., .data$age, .data$sex, .data$region, .data$year, .data$t_icdcat)} else {.}} %>%
      {if (!xb & yb & !fu){tidyr::complete(., .data$age, .data$sex, .data$region, .data$year, .data$t_icdcat, !!syb_var)} else {.}} %>%
      {if (!xb & yb & fu){tidyr::complete(., .data$age, .data$sex, .data$region, .data$year, .data$t_icdcat, !!syb_var, !!fub_var)} else{.}} %>%
      dplyr::mutate(i_observed = dplyr::case_when(is.na(.data$i_observed) ~ 0,
                                                  TRUE              ~ .data$i_observed)) %>%
      dplyr::filter(!is.na(.data$t_icdcat))
    
    
    #F1c person-years at risk
    sircalc_fu <- df %>%
      {if (fu){dplyr::mutate(!!futime_var := dplyr::case_when(.data[!!futime_var] < futime_breaks[f+1] ~ (.data[!!futime_var] - futime_breaks[f]) / 12,
                                                              .data[!!futime_var] >= futime_breaks[f+1] ~ (futime_breaks[f+1] - futime_breaks[f]) / 12))} else{.}} %>%
      {if (!xb & !yb & !fu){dplyr::group_by(., .data$age, .data$sex, .data$region, .data$year)} else{.}} %>%
      {if (!xb & yb & !fu){dplyr::group_by(., .data$age, .data$sex, .data$region, .data$year, !!syb_var)} else{.}} %>%
      {if (!xb & yb & fu){dplyr::group_by(., .data$age, .data$sex, .data$region, .data$year, !!syb_var, !!fub_var)} else{.}} %>%
      dplyr::summarize(i_pyar = sum(!!futime_var, na.rm = TRUE),
                       n_base = dplyr::n()) %>%
      dplyr::ungroup()
    
    #F1d merge
    sircalc <- sircalc_count %>%
      dplyr::full_join(sircalc_fu, by = c("age", "sex", "region", "year", if(yb){rlang::expr_text(syb_var)}, if(fu){rlang::expr_text(fzb_var)}))
    
    
    
    ### F2-I: Merging reference rates (for refpop only) by t_icdcat, region, year, sex and age 
    
    #CHK 3: reporting used regions and whether they can be found in rates
    
    if(expcount_src == "refrates"){
      
      used_strata <- sircalc %>%
        dplyr::distinct(.data$age, .data$sex, .data$region, .data$year, .data$t_icdcat) %>%
        dplyr::filter(!is.na(.data$t_icdcat))
      
      missing_ref_strata <- used_strata %>%
        dplyr::anti_join(refrates_df, by = c("age", "sex", "region" , "year", "t_icdcat"))
      
      if(nrow(missing_ref_strata) > 0){
        warning("For the following region, years, etc no reference rates can be found:")
        message(paste0(utils::capture.output(missing_ref_strata), collapse = "\n"))
      }
      
      #F2-Ia: Do merge
      
      sir_or <- sircalc %>%
        dplyr::left_join(refrates_df, by = c("age", "sex", "region" , "year", "t_icdcat")) %>%
        dplyr::select(-.data$comment, -.data$population_n_per_year)
      
      
    }
    
    
    ### F3-I Calculating Expected by group (from reference rates) [refrates]
    
    if(expcount_src == "refrates"){
      
      sir_basic <- sir_or %>%
        dplyr::mutate(
          i_expected = .data$i_pyar * .data$incidence_crude_rate / 100000, 
        )
      
    }
    
    ### F3-II Calculating Expected by group (within cohort) [cohort]
    
    ### F4 Making SIR calculations 
    
    
    #SIR and Confidence intervals using calculation methods by @breslowStatisticalMethodsCancer1987
    ##F4a: calculating SIR and confidence intervals
    sir_longresult_strat <- sir_basic %>%
      dplyr::mutate(
        sir = .data$i_observed / .data$i_expected,
        sir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$i_observed) / 2) / .data$i_expected,
        sir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$i_observed + 1)) / 2) / .data$i_expected
      )
    
    
    #F4b: preparing binding if needed
    if(!xb & yb){
      sir_longresult_strat <- sir_longresult_strat %>%
        dplyr::mutate(yvar_name = rlang::quo_name(syb_var)) %>%
        dplyr::rename(yvar_label = !!syb_var)  %>%
        dplyr::select(.data$yvar_name, .data$yvar_label, dplyr::everything())
    }
    
    
    #F4c: binding results if needed
    if(!xb & !yb){
      sir_longresult <- sir_longresult_strat
    }
    
    if(!xb & yb){
      if(y == 1){
        sir_longresult <- sir_longresult_strat
      } else{
        sir_longresult <- rbind(sir_longresult, sir_longresult_strat)
      }
    }
  }
  
  
  
  ### F5: Restructuring results
  
  #final dataset should have the structure: columns
  #icdcat #yvars(1-y) #xvar1 #xvar2 #xvar3 ..#xvarx _ #n_base #observed #expected #pyar #sir #sir_uci #sir_lci
  
  
  
  #summarize option
  
  if(!sg){
    sir_result_pre <- sir_longresult
  }
  
  if(sg){
    
    #o) get used values for labels
    
    used_sex <- sir_longresult %>% dplyr::distinct(.data$sex) %>% dplyr::pull() 
    used_region <- sir_longresult %>% dplyr::distinct(.data$region) %>% dplyr::pull() 
    used_year <- sir_longresult %>% dplyr::distinct(.data$year) %>% dplyr::pull() 
    used_ages <- sir_longresult %>% dplyr::distinct(.data$age) %>% dplyr::pull() 
    used_icdcat <- sir_longresult %>% dplyr::distinct(.data$t_icdcat) %>% dplyr::pull() 
    
    min_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% min()
    max_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% max()
    
    min_age <- stringr::str_sub(used_ages, 1, 2) %>% as.numeric() %>% min()
    max_age <- stringr::str_sub(used_ages, -3) %>% as.numeric() %>% max()
    
    
    #i) CHK for missing summarize vars
    sg_not_found <- sg_var_names[!(sg_var_names %in% colnames(sir_longresult))]
    
    
    if (length(sg_not_found) > 0) {
      rlang::abort(
        paste0(
          "The following variables defined in summarize_groups are not found in the results dataframe: ",
          paste(sg_not_found, collapse = ", ")
        )
      )
    }
    
    #ii) create vector with all possible grouping vars and CHK
    all_grouping_vars <- c("age", "sex", "region", "year")
    if(yb){
      all_grouping_vars <- all_grouping_vars %>% c("yvar_name", "yvar_label", .) %>% unique()
    }
    
    sg_not_possible <- sg_var_names[!(sg_var_names %in% all_grouping_vars)]
    
    
    if (length(sg_not_possible) > 0) {
      rlang::abort(
        paste0(
          "The following variables defined in summarize_groups are not possible or meaningful to use: ",
          paste(sg_not_possible, collapse = ", ")
        )
      )
    }
    
    #iii) remove from grouping vars those who should be summarized
    grouping_vars <- all_grouping_vars[!(all_grouping_vars %in% sg_var_names)]
    
    
    
    #iv) summarize over grouping vars
    sir_result_pre_tmp <- sir_longresult %>%
      dplyr::group_by_at(dplyr::vars(grouping_vars, .data$t_icdcat)) %>%
      dplyr::summarize(
        group_observed = sum(.data$i_observed, na.rm = TRUE),
        group_pyar = sum(.data$i_pyar, na.rm = TRUE),
        group_n_base = sum(.data$n_base, na.rm = TRUE),
        group_incidence_cases = sum(.data$incidence_cases, na.rm = TRUE),
        group_population_pyar = sum(.data$population_pyar, na.rm = TRUE),
        group_incidence_crude_rate = .data$group_incidence_cases / .data$group_population_pyar * 100000,
        group_expected = sum(.data$i_expected, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      #calculate sir
      dplyr::mutate(
        sir = .data$group_observed / .data$group_expected,
        sir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$group_observed) / 2) / .data$group_expected,
        sir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$group_observed + 1)) / 2) / .data$group_expected
      )
    
    
    #v) #add grouping information for summarized variables
    
    
    if("age" %in% sg_var_names == TRUE){
      sir_result_pre_tmp <- sir_result_pre_tmp %>%
        dplyr::mutate(age = paste0("Total - All included ages: ",  min_age, " - ", max_age))
    }
    
    if("sex" %in% sg_var_names == TRUE){
      sir_result_pre_tmp <- sir_result_pre_tmp %>%
        dplyr::mutate(sex = paste0("Total - All included genders: ", paste(used_sex, collapse = ", ")))
    }
    
    if("region" %in% sg_var_names == TRUE){
      sir_result_pre_tmp <- sir_result_pre_tmp %>%
        dplyr::mutate(region = paste0("Total - All included regions: ", paste(used_region, collapse = ", ")))
    }
    
    if("year" %in% sg_var_names == TRUE){
      sir_result_pre_tmp <- sir_result_pre_tmp %>%
        dplyr::mutate(year = paste0("Total - All included years: ", min_year, " - ", max_year))
    }
    
    if("t_icdcat" %in% sg_var_names == TRUE){
      sir_result_pre_tmp <- sir_result_pre_tmp %>%
        dplyr::mutate(icdcat = paste0("Total - All included ICD categories: ", paste(used_icdcat, collapse = ", ")))
    }
    
    #vi) rename vars
    
    sir_result_pre <- sir_result_pre_tmp %>%
      dplyr::rename(i_observed = .data$group_observed,
                    i_pyar = .data$group_pyar,
                    n_base = .data$group_n_base,
                    incidence_cases = .data$group_incidence_cases,
                    population_pyar = .data$group_population_pyar,
                    incidence_crude_rate = .data$group_incidence_crude_rate,
                    i_expected = .data$group_expected)
    
    
  }
  
  #xbreak_vars arrangement
  
  #5d rounding
  
  sir_result_pre <- sir_result_pre %>%
    dplyr::mutate_at(dplyr::vars(.data$i_expected, .data$sir, .data$sir_lci, .data$sir_uci), ~ round(.,2))
  
  #collapse_ci option
  
  if(collapse_ci == TRUE){
    
    sir_result_pre <- sir_result_pre %>%
      tidyr::unite("sir_ci", .data$sir_lci, .data$sir_uci, sep = " - ")
  }
  
  ### F5: labelling and returning results
  
  sir_result <- sir_result_pre %>%
    dplyr::rename(
      observed = .data$i_observed,
      pyar = .data$i_pyar,
      expected = .data$i_expected
    ) %>%
    {if(collapse_ci == FALSE){dplyr::select(., .data$age, .data$region, .data$sex, .data$year, .data$t_icdcat, .data$observed, 
                                            .data$expected, .data$sir, .data$sir_lci, .data$sir_uci, dplyr::everything())} else{.}} %>%
    {if(collapse_ci == TRUE){dplyr::select(., .data$age, .data$region, .data$sex, .data$year, .data$t_icdcat, .data$observed, 
                                           .data$expected, .data$sir, .data$sir_ci, dplyr::everything())} else{.}} %>%
    {if(yb){dplyr::select(., .data$age, .data$region, .data$sex, .data$year, .data$yvar_name, .data$yvar_label, 
                          dplyr::everything())} else{.}} 
  
  return(sir_result)
  
}

