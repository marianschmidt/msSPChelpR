
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
#' @param output_information option to define information to be presented in final output table. Default is "full" information, i.e. all variables from
#'                           from sir_df. "reduced" is observed, expected, sir, sir_ci / sir_lci+sir_uci, pyar, n_base. "minimal" is observed, expected, sir, sir_ci. 
#'                           Default is "full".
#' @param add_total_row option to add a row of totals. Can be either "no" for not adding such a row or "start" or "end" for adding it at the first or last row. Default is "no".
#' @param add_total_fu option to add totals for follow-up time. Can be either "no" for not adding such a column or "start" or "end" for adding it at the first or last column. Default is "no".
#' @param collapse_ci If TRUE upper and lower confidence interval will be collapsed into one column separated by "-". Default is FALSE.
#' @param shorten_total_cols Shorten text in all results columns that start with "Total". Default == FALSE.
#' @param fubreak_var_name Name of variable with futime stratification. Default is "fu_time".
#' @param ybreak_var_name Name of variable with futime stratification. Default is "yvar_name".
#' @param xbreak_var_name Name of variable with futime stratification. Default is "xvar_name".
#' @param alpha significance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @export

summarize_sir_results <- function(sir_df,
                                  summarize_groups,
                                  summarize_icdcat,
                                  output = "long",
                                  output_information = "full",
                                  add_total_row = "no",
                                  add_total_fu = "no",
                                  collapse_ci = FALSE,
                                  shorten_total_cols = FALSE,
                                  fubreak_var_name = "fu_time",
                                  ybreak_var_name = "yvar_name",
                                  xbreak_var_name = "none",
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
  
  #prepare sorting
  if(yb & !("yvar_sort" %in% colnames(sir_df))){
    sir_df <- sir_df %>%
      dplyr::mutate(yvar_sort = as.numeric(as.factor(.data[[!!ybreak_var_name]]))) 
  }
  
  if(yb & !("yvar_sort_levels" %in% colnames(sir_df))){
    sir_df <- sir_df %>%
      dplyr::mutate(yvar_sort_levels = as.numeric(as.factor(.data[[!!ylabel_var_name]]))) 
  }
  
  if(fu & !("fu_time_sort" %in% colnames(sir_df))){
    sir_df <- sir_df %>%
      dplyr::mutate(fu_time_sort = as.numeric(as.factor(.data[[!!fubreak_var_name]]))) 
  }
  
  
  #prepare summarize_groups
  
  if(summarize_groups[1] != "none"){
    sg <- TRUE
    sg_var_names <- rlang::eval_tidy(summarize_groups)
  } else{sg <- FALSE}
  
  #enforce summarize icdcat
  
  if(summarize_icdcat == TRUE){
    sg <- TRUE
    sg_var_names <- rlang::eval_tidy(c(summarize_groups, "t_icdcat"))
  }
  
  #prepare total_fu
  
  if(add_total_fu == "start" | add_total_fu == "end"){
    ft <- TRUE
  } else{ft <- FALSE} #dummy to show loop for Total line
  
  #prepare total_row
  
  if(add_total_row == "start" | add_total_row == "end"){
    
    #BUG: do something here
    
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
    all_grouping_vars <- c("age", "sex", "region", "year", "t_icdcat",
                           if(fu){c(fubreak_var_name, "fu_time_sort")},
                           if(yb){c(ybreak_var_name, ylabel_var_name, "yvar_sort", "yvar_sort_levels")}, if(xb){c(xbreak_var_name, xlabel_var_name)})
    
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
    sum_pre_tmp <- sir_df_mod %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::all_of(grouping_vars))) %>%
      dplyr::summarize(
        group_observed = sum(.data$observed, na.rm = TRUE),
        group_pyar = sum(.data$pyar, na.rm = TRUE),
        group_n_base = sum(.data$n_base, na.rm = TRUE),
        group_incidence_cases = sum(.data$ref_inc_cases, na.rm = TRUE),
        group_population_pyar = sum(.data$ref_population_pyar, na.rm = TRUE),
        group_expected = sum(.data$expected, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      #calculate sir
      dplyr::mutate(
        sir = .data$group_observed / .data$group_expected,
        sir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$group_observed) / 2) / .data$group_expected,
        sir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$group_observed + 1)) / 2) / .data$group_expected,
        group_incidence_crude_rate = .data$group_incidence_cases / .data$group_population_pyar * 100000
      ) %>%
      dplyr::distinct()
    
    
    #v) #add grouping information for summarized variables
    
    
    if("age" %in% sg_var_names == TRUE){
      if(shorten_total_cols==FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(age = paste0("Total - All included ages: ",  min_age, " - ", max_age))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(age = "Total")
      }
    }
    
    if("sex" %in% sg_var_names == TRUE){
      if(shorten_total_cols==FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(sex = paste0("Total - All included genders: ", paste(used_sex, collapse = ", ")))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(sex = "Total")
      }
    }
    
    if("region" %in% sg_var_names == TRUE){
      if(shorten_total_cols==FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(region = paste0("Total - All included regions: ", paste(used_region, collapse = ", ")))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(region = "Total")
      }
    }
    
    if("year" %in% sg_var_names == TRUE){
      if(shorten_total_cols==FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(year = paste0("Total - All included years: ", min_year, " - ", max_year))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(year = "Total")
      }
    }
    
    if("t_icdcat" %in% sg_var_names == TRUE){
      if(shorten_total_cols==FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(t_icdcat = paste0("Total - All included ICD categories: ", paste(used_icdcat, collapse = ", ")))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          dplyr::mutate(t_icdcat = "Total")
      }
    }
    
    #rounding
    
    sum_pre_tmp <- sum_pre_tmp %>%
      dplyr::mutate_at(dplyr::vars(.data$group_pyar, .data$sir, .data$sir_lci, .data$sir_uci), ~ round(.,2))
    
    #do collapse_ci
    if(ci){
      sum_pre_tmp <- sum_pre_tmp %>% 
        tidyr::unite("sir_ci", .data$sir_lci, .data$sir_uci, sep = " - ")
    } 
    
    #label and sort
    
    sum_pre <- sum_pre_tmp %>%
      dplyr::rename(observed = .data$group_observed,
                    expected = .data$group_expected,
                    pyar = .data$group_pyar,
                    ref_inc_cases = .data$group_incidence_cases,
                    ref_population_pyar = .data$group_population_pyar,
                    ref_inc_crude_rate = .data$group_incidence_crude_rate) %>%
      dplyr::select(dplyr::one_of(c("age", "region", "sex", "year", 
                                    if(yb){c("yvar_name", "yvar_label")}, if(xb){c("xvar_name", "xvar_label")}, 
                                    if(fu){"fu_time"}, 
                                    "t_icdcat", "observed", "expected", "sir",
                                    if(collapse_ci == TRUE){"sir_ci"},
                                    if(collapse_ci == FALSE){c("sir_lci", "sir_uci")})),
                    dplyr::everything()
      ) %>% 
      dplyr::arrange_at(dplyr::vars(dplyr::one_of(c("age", "region", "sex", "year", 
                                                    if(yb){c("yvar_sort", "yvar_sort_levels")}, if(xb){c("xvar_name", "xvar_label")}, 
                                                    if(fu){"fu_time_sort"}
      ))))
    
    
  }
  
  
  
  #change output information required
  
  
  #full
  if(output_information == "full"){
    sum_pre2 <- sum_pre
  }
  #reduced
  if(output_information == "reduced"){
    sum_pre2 <- sum_pre %>%
      dplyr::select(dplyr::one_of(c("age", "region", "sex", "year", 
                                    if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, 
                                    if(xb){c("xvar_name", "xvar_label")}, 
                                    if(fu){c("fu_time", "fu_time_sort")}, 
                                    "t_icdcat", "observed", "expected", "sir",
                                    if(collapse_ci == TRUE){"sir_ci"},
                                    if(collapse_ci == FALSE){c("sir_lci", "sir_uci")},
                                    "pyar", "n_base")))
  }
  #minimal
  if(output_information == "minimal"){
    if(!ci){
      sum_pre <- sum_pre %>%
        #make sure CIs are minimal
        tidyr::unite("sir_ci", .data$sir_lci, .data$sir_uci, sep = " - ")
      
      ci <- TRUE
    }
    
    #select only required vars
    sum_pre2 <- sum_pre %>%
      dplyr::select(dplyr::one_of(c("age", "region", "sex", "year", 
                                    if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, 
                                    if(xb){c("xvar_name", "xvar_label")}, 
                                    if(fu){c("fu_time", "fu_time_sort")}, 
                                    "t_icdcat", "observed", "expected", "sir", "sir_ci")))
  }
  
  #reshaping according to output option
  
  #output == "long"
  if(output == "long"){
    sum_results <- sum_pre2
  }
  
  #output = "wide"
  if(output == "wide"){
    #wide - only FU-times need to be transposed
    
    trans_vars <- names(sum_pre2)[!names(sum_pre2) %in% c("age", "region", "sex", "year", "t_icdcat", 
                                                          if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, 
                                                          if(xb){c("xvar_name", "xvar_label")}, 
                                                          if(fu){c("fu_time", "fu_time_sort")})]
    
    #creating pivoting specs
    if(fu | xb){
      specs <- sum_pre2 %>% 
        tidyr::build_wider_spec(names_from = c(if(fu){c("fu_time_sort", "fu_time")}, if(xb){c("xvar_name", "xvar_label")}), 
                                values_from = tidyselect::all_of(trans_vars),
                                names_sep = ".")
      
      yb_off <- FALSE
      
    }
    
    if(fu & !xb){
      specs <- specs %>%
        dplyr::mutate(.name = paste(paste(.data[["fu_time_sort"]], .data[["fu_time"]], sep = "**"), .data[[".value"]], sep = "__"))
    }
    
    if(!fu & xb){
      specs <- specs %>%
        dplyr::mutate(.name = paste(.data[["xvar_label"]], .data[[".value"]], sep = "__"))
    }
    
    if(fu & xb){
      specs <- specs %>%
        dplyr::mutate(.name = paste(paste(paste(.data[["fu_time_sort"]], .data[["fu_time"]], sep = "**"), .data[["xvar_label"]], sep = "<<"), .data[[".value"]], sep = "__"))
    }
    
    if(!fu & !xb & yb){
      
      specs <- sum_pre2 %>% 
        tidyr::build_wider_spec(names_from = c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels"), 
                                values_from = tidyselect::all_of(trans_vars),
                                names_sep = ".")
      
      specs <- specs %>%
        dplyr::mutate(.name = paste(.data[["yvar_label"]], .data[[".value"]], sep = "__"))
      
      yb_off <- TRUE
      
    }
    
    if(!fu & !xb & !yb){
      warning("No break variables (futime, xvar, yvar) provided. Nothing to reshape. Returning long results.")
      sum_results <- sum_pre2
      
      return(sum_results)
    }
    
    
    sum_results_pre <- sum_pre2 %>% 
      tidyr::pivot_wider_spec(specs)
    
    #do sorting
    
    #change sort order for fu_time dependent variable names
    sort <- names(sum_results_pre) %>% stringr::str_subset("__") %>% #detect unique separator
      stringr::str_replace_all("_exp", "_2exp") %>% stringr::str_replace_all("_obs", "_1obs") %>% #add numer to facilitate sorting
      stringr::str_replace_all("_sir_uci", "_5sir_uci") %>% stringr::str_replace_all("_sir_lci", "_4sir_lci") %>%
      stringr::str_replace_all("_sir_ci", "_4sir_ci") %>% stringr::str_replace_all("_sir", "_3sir") %>% 
      stringr::str_replace_all("_pyar", "_6pyar") %>% stringr::str_replace_all("_group_n_base", "_7group_n_base") %>%
      stringr::str_sort() %>%
      stringr::str_replace_all(stringr::regex("\\_[:digit:]"), "_") 
    
    
    #sort dataframe
    sum_results <- sum_results_pre %>%
      dplyr::select(dplyr::one_of(c("age", "region", "sex", "year", 
                                    if(yb & !yb_off){c("yvar_name", "yvar_label")}, 
                                    "t_icdcat", sort)),
                    dplyr::everything())
    
    names(sum_results) <- names(sum_results) %>% 
      stringr::str_replace_all(stringr::regex("[:digit:]\\*\\*"), "")
    
  }
  
  #output = "nested"
  if(output == "nested"){
    
    sum_results <- sum_pre %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::all_of(grouping_vars))) %>%
      {if (xb){dplyr::group_by(., .data$xvar_name, .add = TRUE)} else{.}} %>% # add x grouping variable if present
      {if (fu){dplyr::group_by(., .data$fu_time, .add = TRUE)} else{.}} %>% # add fub grouping variable
      tidyr::nest()
  }
  
  return(sum_results) 
  
}

