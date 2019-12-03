
#' Summarize detailed SIR results
#' 
#' @param sir_df dataframe with stratified sir results created using the sir or sir_byfutime functions
#' @param summarize_groups option to define summarizing stratified groups. Default is "none". 
#'                 If you want to define variables that should be summarized into one group, you can chose from region_var, sex_var, year_var. 
#'                 Define multiple summarize variables by summarize_groups = c("region", "sex", "year")
#' @param summarize_icdcat option to summarize over all ICD codes 
#' @param output Define the format of the output. Can be either "nested" for nested dataframe with fubreak_var and xbreak_var in separate sub_tables (purrr).
#'               Or "wide" for wide format where fubreak_var and xbreak_var are appended as columns.
#'               Or "long" for long format where sir_df is not reshaped, but just summarized (ybreak_var, xbreak_var and fubreak_var remain in rows).
#'               Default is "long".
#' @param add_total_row option to add a row of totals. Can bei either "no" for not adding such a row or "top" or "bottom" for adding it at the first or last row. Default is "no".
#' @param add_total_fu option to add totals for follow-up time. Can bei either "no" for not adding such a column or "left" or "right" for adding it at the first or last column. Default is "no".
#' @param collapse_ci If TRUE upper and lower confidence interval will be collapsed into one column separated by "-". Default is FALSE.
#' @param fubreak_var_name Name of variable with futime stratification. Default is "fu_time".
#' @param ybreak_var_name Name of variable with futime stratification. Default is "yvar_name".
#' @param xbreak_var_name Name of variable with futime stratification. Default is "xvar_name".
#' @param alpha signifcance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' 

summarize_sir_results <- function(sir_df,
                                  summarize_groups,
                                  summarize_icdcat,
                                  output = "long",
                                  add_total_row = "no",
                                  add_total_fu = "no",
                                  collapse_ci = FALSE,
                                  fubreak_var_name = "fu_time",
                                  ybreak_var_name = "yvar_name",
                                  xbreak_var_name = "xvar_name",
                                  alpha = 0.05
) {
  
  #---prework
  #get arguments
  
  #set defaults
  
  #prepare fubreak_var_name
  
  if(fubreak_var_name == "none"){
    fu <- FALSE
  } else{
    if(fubreak_var_name %in% colnames(sir_df)){
      fu <- TRUE
    } else{
      fubreak_var_name <- "fu_time"
      if(fubreak_var_name %in% colnames(sir_df)){
        warning("Provided fubreak_var_name does not exit in sir_df. Default column 'fu_time' does exist and is used instead.")
        fu <- TRUE
      } else{
        warning("Provided fubreak_var_name does not exit in sir_df. Results can not by summarized by fu_time.")
        fu <- FALSE
      }
    }
  }
  
  #prepare ybreak_var_name
  
  if(ybreak_var_name == "none"){
    yb <- FALSE
  } else{
    ylabel_var_name <- "yvar_label"
    if((ybreak_var_name %in% colnames(sir_df)) & (ylabel_var_name %in% colnames(sir_df))){
      yb <- TRUE
    } else{
      ybreak_var_name <- "yvar_name"
      if((ybreak_var_name %in% colnames(sir_df)) & (ylabel_var_name %in% colnames(sir_df))){
        warning("Provided ybreak_var_name does not exit in sir_df. Default column 'yvar_name' does exist and is used instead.")
        yb <- TRUE
      } else{
        warning("Provided ybreak_var_name or yvar_label column does not exit in sir_df. Results can not by summarized by ybreak_vars.")
        yb <- FALSE
      }
    }
  }
  
  #prepare xbreak_var_name
  
  if(xbreak_var_name == "none"){
    xb <- FALSE
  } else{
    xlabel_var_name <- "xvar_label"
    if((xbreak_var_name %in% colnames(sir_df)) & (xlabel_var_name %in% colnames(sir_df))){
      xb <- TRUE
    } else{
      xbreak_var_name <- "xvar_name"
      if((xbreak_var_name %in% colnames(sir_df)) & (xlabel_var_name %in% colnames(sir_df))){
        warning("Provided xbreak_var_name does not exit in sir_df. Default column 'xvar_name' does exist and is used instead.")
        xb <- TRUE
      } else{
        warning("Provided xbreak_var_name or xvar_label column does not exit in sir_df. Results can not by summarized by xbreak_vars.")
        xb <- FALSE
      }
    }
  }
  
  
  #prepare summarize_groups
  
  if(summarize_groups[1] != "none"){
    sg <- TRUE
    sg_vars <- rlang::enquo(summarize_groups)
    sg_var_names <- rlang::eval_tidy(summarize_groups)
  } else{sg <- FALSE}
  
  #prepare total_fu
  
  if(add_total_fu == "left" | add_total_fu == "right"){
    ft <- TRUE
  } else{ft <- FALSE} #dummy to show loop for Total line
  
  #prepare total_row
  
  if(add_total_row == "top" | add_total_row == "bottom"){
    
    df <- df %>%
      dplyr::mutate(total_var = "Overall")
    
    length_yb = length_yb + 1
    
    if(add_total_row == "top") {
      ybreak_var_names <- c("total_var", ybreak_var_names)
    }
    
    if(add_total_row == "bottom") {
      ybreak_var_names <- c(ybreak_var_names, "total_var")
    }
  }
  
  #prepare collapse_ci option
  
  if(collapse_ci == FALSE & !is.null(sir_df$sir_lci) & !is.null(sir_df$sir_uci)){
    sir_df_mod <- sir_df
    ci <- FALSE
  } else{
    
    if(collapse_ci == TRUE & !is.null(sir_df$sir_lci) & !is.null(sir_df$sir_uci)){
      sir_df_mod <- sir_df 
      ci <- TRUE
    } else{
      
      if(collapse_ci == TRUE & !is.null(sir_df$sir_ci)){
        sir_df_mod <- sir_df %>% 
          tidyr::separate(.data$sir_ci, into = c("sir_lci", "sir_uci"), sep = " - ")
        ci <- TRUE
      } else{
        warning("There is a problem with the collapse_ci setting. If FALSE, sir_lci and sir_uci must exist in sir_df. If TRUE, eiter sir_ci or sir_lci and sir_uci must exist in sir_df.")
      }
    }
  }
  
  #final check sir_df
  
  required_vars <- c(if(fu){fubreak_var_name}, if(yb){ybreak_var_name}, if(xb){xbreak_var_name},
                     if(ci){"sir_ci"}, if(!ci){c("sir_lci", "sir_uci")}, "age", "region", "sex", "year", "t_icdcat", "observed",
                     "expected", "sir", "pyar", "n_base", "ref_inc_cases", "ref_population_pyar")
  
  
  not_found_vars <- required_vars[!(required_vars %in% colnames(sir_df_mod))]
  
  if (length(not_found_vars) > 0) {
    rlang::abort(
      paste0(
        "The following variables defined are not found in the provided dataframe sir_df: ",
        paste(not_found_vars, collapse = ", ")
      )
    )
  }
  
  
  #---- summarize option
  
  if(!sg){
    sum_pre <-  sir_df_mod
  }
  
  if(sg){
    
    #o) get used values for labels
    
    used_sex <- sir_df_mod %>% dplyr::distinct(.data$sex) %>% dplyr::pull() 
    used_region <- sir_df_mod %>% dplyr::distinct(.data$region) %>% dplyr::pull() 
    used_year <- sir_df_mod %>% dplyr::distinct(.data$year) %>% dplyr::pull() 
    used_ages <- sir_df_mod %>% dplyr::distinct(.data$age) %>% dplyr::pull() 
    used_icdcat <- sir_df_mod %>% dplyr::distinct(.data$t_icdcat) %>% dplyr::pull() 
    
    min_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% min()
    max_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% max()
    
    min_age <- stringr::str_sub(used_ages, 1, 2) %>% as.numeric() %>% min()
    max_age <- stringr::str_sub(used_ages, -3) %>% as.numeric() %>% max()
    
    
    #i) CHK for missing summarize vars
    sg_not_found <- sg_var_names[!(sg_var_names %in% colnames(sir_df_mod))]
    
    
    if (length(sg_not_found) > 0) {
      rlang::abort(
        paste0(
          "The following variables defined in summarize_groups are not found in the results dataframe: ",
          paste(sg_not_found, collapse = ", ")
        )
      )
    }
    
    #ii) create vector with all possible grouping vars and CHK
    all_grouping_vars <- c("age", "sex", "region", "year", 
                           if(fu){fubreak_var_name},
                           if(yb){c(ybreak_var_name, ylabel_var_name)}, if(xb){c(xbreak_var_name, xlabel_var_name)})
    
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
    
    
    browser()
    
    #iv) summarize over grouping vars
    sum_pre_tmp <- sir_df_mod %>%
      dplyr::group_by_at(dplyr::vars(grouping_vars, .data$t_icdcat)) %>%
      dplyr::summarize(
        group_observed = sum(.data$observed, na.rm = TRUE),
        group_pyar = sum(.data$pyar, na.rm = TRUE),
        group_n_base = sum(.data$n_base, na.rm = TRUE),
        group_incidence_cases = sum(.data$ref_inc_cases, na.rm = TRUE),
        group_population_pyar = sum(.data$ref_population_pyar, na.rm = TRUE),
        group_min_incidence_crude_rate = min(.data$ref_inc_cases, na.rm = TRUE) / max(.data$ref_population_pyar, na.rm = TRUE) * 100000,
        group_max_incidence_crude_rate = max(.data$ref_inc_cases, na.rm = TRUE) / min(.data$ref_population_pyar, na.rm = TRUE) * 100000,
        group_mean_incidence_crude_rate = mean(.data$ref_inc_cases, na.rm = TRUE) / mean(.data$ref_population_pyar, na.rm = TRUE) * 100000,
        group_expected = sum(.data$expected, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      #calculate sir
      dplyr::mutate(
        sir = .data$group_observed / .data$group_expected,
        sir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$group_observed) / 2) / .data$group_expected,
        sir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$group_observed + 1)) / 2) / .data$group_expected
      ) %>%
      dplyr::distinct()
    
    
    #v) #add grouping information for summarized variables
    
    
    if("age" %in% sg_var_names == TRUE){
      sum_pre_tmp <- sum_pre_tmp %>%
        dplyr::mutate(age = paste0("Total - All included ages: ",  min_age, " - ", max_age))
    }
    
    if("sex" %in% sg_var_names == TRUE){
      sum_pre_tmp <- sum_pre_tmp %>%
        dplyr::mutate(sex = paste0("Total - All included genders: ", paste(used_sex, collapse = ", ")))
    }
    
    if("region" %in% sg_var_names == TRUE){
      sum_pre_tmp <- sum_pre_tmp %>%
        dplyr::mutate(region = paste0("Total - All included regions: ", paste(used_region, collapse = ", ")))
    }
    
    if("year" %in% sg_var_names == TRUE){
      sum_pre_tmp <- sum_pre_tmp %>%
        dplyr::mutate(year = paste0("Total - All included years: ", min_year, " - ", max_year))
    }
    
    if("t_icdcat" %in% sg_var_names == TRUE){
      sum_pre_tmp <- sum_pre_tmp %>%
        dplyr::mutate(icdcat = paste0("Total - All included ICD categories: ", paste(used_icdcat, collapse = ", ")))
    }
    
    #do collapse_ci
    if(ci){
      sum_pre <- sum_pre_tmp %>% 
        tidyr::unite("sir_ci", .data$sir_lci, .data$sir_uci, sep = " - ")
    } else{
      sum_pre <- sum_pre_tmp
    }
    
  }
  
  #reshaping according to output option
  
  #output = "long"
  if(output == "long"){
    sum_results <- sum_pre
  }
  
  #output = "wide"
  
  #output = "nested"
  
  
  return(sum_results) 
  
}
