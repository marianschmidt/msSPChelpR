#'
#' Summarize detailed SIR results
#' 
#' @param sir_df dataframe with stratified sir results created using the sir or sir_byfutime functions
#' @param summarize_groups option to define summarizing stratified groups. Default is "none". 
#'                 If you want to define variables that should be summarized into one group, you can chose from age, sex, region, year. 
#'                 Define multiple summarize variables e.g. by summarize_groups = c("region", "sex", "year")
#' @param summarize_site If TRUE results will be summarized over all t_site categories. Default is FALSE. 
#' @param output Define the format of the output. 
#'               Can be either "nested" for nested dataframe with fubreak_var and xbreak_var in separate sub_tables (purrr).
#'               Or "wide" for wide format where fubreak_var and xbreak_var are appended as columns.
#'               Or "long" for long format where sir_df is not reshaped, but just summarized (ybreak_var, xbreak_var and fubreak_var remain in rows).
#'               Default is "long".
#' @param output_information option to define information to be presented in final output table. Default is "full" information, i.e. all variables from
#'                           from sir_df. "reduced" is observed, expected, sir, sir_ci / sir_lci+sir_uci, pyar, n_base. "minimal" is observed, expected, sir, sir_ci. 
#'                           Default is "full".
#' @param add_total_row option to add a row of totals. Can be either "no" for not adding such a row or "start" or "end" for adding it at the first or last row 
#'                      or "only" for only showing totals and no yvar. Default is "no".
#' @param add_total_fu option to add totals for follow-up time. Can be either "no" for not adding such a column or "start" or "end" for adding it at the first or last column
#'                      or "only" for only showing follow-up time totals. Default is "no".
#' @param collapse_ci If TRUE upper and lower confidence interval will be collapsed into one column separated by "-". Default is FALSE.
#' @param shorten_total_cols Shorten text in all results columns that start with "Total". Default == FALSE.
#' @param site_var_name Name of variable with site stratification. Default is "t_site".
#' @param fubreak_var_name Name of variable with futime stratification. Default is "fu_time".
#' @param ybreak_var_name Name of variable with futime stratification. Default is "yvar_name".
#' @param xbreak_var_name Name of variable with futime stratification. Default is "xvar_name".
#' @param alpha significance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @export
#' @examples 
#' #There are various preparation steps required, before you can run this function.
#' #Please refer to the Introduction vignette to see how to prepare your data
#' \dontrun{
#' summarize_sir_results(.,
#'     summarize_groups = c("region", "age", "year", "race"),
#'     summarize_site = TRUE,
#'     output = "long",  output_information = "minimal",
#'     add_total_row = "only",  add_total_fu = "no",
#'     collapse_ci = FALSE,  shorten_total_cols = TRUE,
#'     fubreak_var_name = "fu_time", ybreak_var_name = "yvar_name",
#'     xbreak_var_name = "none", site_var_name = "t_site",
#'     alpha = 0.05
#'     )
#'     }
#'     

summarize_sir_results <- function(sir_df,
                                  summarize_groups,
                                  summarize_site = FALSE,
                                  output = "long",
                                  output_information = "full",
                                  add_total_row = "no",
                                  add_total_fu = "no",
                                  collapse_ci = FALSE,
                                  shorten_total_cols = FALSE,
                                  fubreak_var_name = "fu_time",
                                  ybreak_var_name = "yvar_name",
                                  xbreak_var_name = "none",
                                  site_var_name = "t_site",
                                  alpha = 0.05
) {
  
  #---prepwork
  #get arguments
  
  #check that provided arguments are in correct format
  
  if(!is.character(summarize_groups)){
    rlang::abort("Parameter `summarize_groups` must be character vector.")
  }
  
  if(!is.logical(summarize_site)){
    rlang::warn(c(
      "i" = "Parameter `summarize_site` must be logical (TRUE or FALSE). Default `summarize_site = FALSE` will be used instead."
    ))
    summarize_site <- FALSE
  }
  
  if(!is.character(output)){
    rlang::warn(c(
      "i" = "Parameter `output` must be character vector. Default `output = \"long\"` will be used instead."
    ))
    output <- "long"
  }
  if(!(output %in% c("nested", "wide", "long"))){
    rlang::warn(c(
      "i" = "Parameter `output` must be \"wide\", \"long\" or \"nested\".", 
      paste0("Default `output = \"long\"` will be used instead of: ", output)
    ))
    output <- "long"
  }
  
  
  if(!is.character(output_information)){
    rlang::warn(c(
      "i" = "Parameter `output_information` must be character vector. Default `output = \"full\"` will be used instead."
    ))
    output_information <- "full"
  }
  if(!(output_information %in% c("full", "reduced", "minimal"))){
    rlang::warn(c(
      "i" = "Parameter `output_information` must be \"full\", \"reduced\" or \"minimal\". \n", 
      paste0("Default `output_information = \"full\"` will be used instead of: ", output_information)
    ))
    output_information <- "full"
  }
  
  
  if(!is.character(add_total_row)){
    rlang::warn(c(
      "i" = "Parameter `add_total_row` must be character vector. Default `output = \"no\"` will be used instead."
    ))
    add_total_row <- "no"
  }
  if(!(add_total_row %in% c("no", "start", "end", "only"))){
    rlang::warn(c(
      "i" = "Parameter `add_total_row` must be \"start\", \"end\", \"only\", or \"no\".", 
      paste0("Default `add_total_row = \"no\"` will be used instead of: ", add_total_row)
    ))
    add_total_row <- "full"
  }
  
  if(!is.character(add_total_fu)){
    rlang::warn(c(
      "Parameter `add_total_fu` must be character vector. Default `output = \"no\"` will be used instead."
    ))
    add_total_fu <- "no"
  }
  if(!(add_total_fu %in% c("no", "start", "end", "only"))){
    rlang::warn(c(
      "i" = "Parameter `add_total_fu` must be \"start\", \"end\", \"only\", or \"no\".", 
      paste0("Default `add_total_fu = \"no\"` will be used instead of: ", add_total_fu)
    ))
    add_total_fu <- "full"
  }
  
  if(!is.logical(collapse_ci)){
    rlang::warn(c(
      "i" = "Parameter `collapse_ci` must be logical (TRUE or FALSE). Default `collapse_ci = FALSE` will be used instead."
    ))
    collapse_ci <- FALSE
  }
  
  #prepare site_var_name
  
  site_grouped <- FALSE
  if(!is.character(site_var_name)){
    rlang::warn(c(
      "i" = "Parameter `site_var_name` must be character vector. Default `site_var_name = \"t_site\"` will be used instead."
    ))
    site_var_name <- "t_site"
  }else{
    if(site_var_name == "t_site"){
      cs <- FALSE
    }else{
      if((site_var_name %in% colnames(sir_df))){
        cs <- TRUE
        #add additional check if a different variable than t_site is used
        if("t_site" %in% colnames(sir_df)){
          rlang::warn(c(
            "[WARN Site Variable] Unexpected site variable",
            "i" = paste0("The provided `site_var_name == `", site_var_name,  " is different from default `site_var_name = \"t_site\"`."), 
            "We assume, you want to group results by a different site variable than used when using the `sir_by_futime()` function.",
            "We try to adjust calculation of Observed, PYARs and SIR accordingly,", 
            "!" = "Check results for correctness!"
          ))
          site_grouped <- TRUE
        }
      }else{
        rlang::warn(c(
          "i" = "Provided `site_var_name` does not exit in sir_df. Default `site_var_name = \"t_site\"` will be used instead."
        ))
        site_var_name <- "t_site"
        cs <- FALSE
      }
    }
  }
  
  
  #prepare fubreak_var_name
  
  if(fubreak_var_name == "none"){
    fu <- FALSE
  } else{
    if(fubreak_var_name %in% colnames(sir_df)){
      fu <- TRUE
    } else{
      fubreak_var_name <- "fu_time"
      if(fubreak_var_name %in% colnames(sir_df)){
        rlang::warn(c(
          "i" = "Provided `fubreak_var_name` does not exit in sir_df. Default column 'fu_time' does exist and is used instead."
        ))
        fu <- TRUE
      } else{
        rlang::warn(c(
          "Provided `fubreak_var_name` does not exit in sir_df.",
          "x" = "Results can not by summarized by fu_time."
        ))
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
        rlang::warn(c(
          "i" = "Provided `ybreak_var_name` does not exist in sir_df. Default column 'yvar_name' does exist and is used instead."
        ))
        yb <- TRUE
      } else{
        rlang::warn(c(
          "i" = "Provided `ybreak_var_name` or `yvar_label` column does not exit in sir_df.",
          "x" = "Results can not by summarized by ybreak_vars."
        ))
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
        rlang::warn(c(
          "i" = "Provided `xbreak_var_name` does not exit in sir_df. Default column 'xvar_name' does exist and is used instead."
        ))
        xb <- TRUE
      } else{
        rlang::warn(c(
          "i" = "Provided `xbreak_var_name` or xvar_label column does not exit in sir_df.",
          "x" = "Results can not by summarized by xbreak_vars."
        ))
        xb <- FALSE
      }
    }
  }
  
  #prepare site_var_name
  #in case t_site var need to be changed
  if(cs){
    if(site_grouped == FALSE){
      sir_df <- sir_df %>%
        tidytable::select.(-t_site) %>%
        tidytable::rename.(t_site = !!rlang::sym(site_var_name))
    }else{
      sir_df <- sir_df %>%
        tidytable::rename.(t_site_orig = t_site,
                           t_site = !!rlang::sym(site_var_name))
    }
  }
  
  
  #rename variables futime_var, xbreak_var and ybreak_var for normalized output
  if(fu){
    sir_df <- sir_df %>%
      tidytable::rename.(fu_time = !!rlang::sym(fubreak_var_name))
  }
  
  if(xb){
    sir_df <- sir_df %>%
      tidytable::rename.(xvar_name = !!rlang::sym(xbreak_var_name))
  }
  
  if(yb){
    sir_df <- sir_df %>%
      tidytable::rename.(yvar_name = !!rlang::sym(ybreak_var_name))
  }
  
  #prepare sorting
  if(yb & !("yvar_sort" %in% colnames(sir_df))){
    sir_df <- sir_df %>%
      tidytable::mutate.(yvar_sort = as.numeric(as.factor(.data$yvar_name))) 
  }
  
  if(yb & !("yvar_sort_levels" %in% colnames(sir_df))){
    sir_df <- sir_df %>%
      tidytable::mutate.(yvar_sort_levels = as.numeric(as.factor(.data$yvar_label))) 
  }
  
  if(fu & !("fu_time_sort" %in% colnames(sir_df))){
    sir_df <- sir_df %>%
      tidytable::mutate.(fu_time_sort = as.numeric(as.factor(.data$fu_time))) 
  }
  
  
  #prepare summarize_groups
  
  if(summarize_groups[1] != "none"){
    sg <- TRUE
    sg_var_names <- rlang::eval_tidy(summarize_groups)
  } else{sg <- FALSE}
  
  #check if race is in dataset
  if("race" %in% colnames(sir_df)){
    rs <- TRUE
  } else(rs <- FALSE)
  
  
  #enforce summarize site
  
  if(summarize_site == TRUE){
    sg <- TRUE
    sg_var_names <- rlang::eval_tidy(c(summarize_groups, "t_site"))
  }
  
  # if(summarize_site == TRUE){
  #   if(site_grouped == FALSE){
  #   sg <- TRUE
  #   sg_var_names <- rlang::eval_tidy(c(summarize_groups, "t_site"))
  #   } else{
  #     #FIX WIP: check whether this safeguard here is really needed.
  #     rlang::warn(paste0(
  #     "Parameter `summarize_site == TRUE` provided, but function detected that you are not using the original `site_var_name == t_site`. \n",
  #     "We therefore assume you are using a grouped site_var for which summarize_site makes no sense. \n",
  #     "Default `summarize_site = FALSE` will be used instead."))
  #     summarize_site <- FALSE
  #     sg <- FALSE
  #   }
  # }
  
  #prepare total_fu
  
  if(add_total_fu == "start" | add_total_fu == "end" | add_total_fu == "only"){
    ft <- TRUE
  } else{ft <- FALSE}
  
  #prepare total_row
  
  if(add_total_row == "start" | add_total_row == "end" | add_total_row == "only"){
    rt <- TRUE
    if(rt & !yb){
      rlang::warn(c(
        "i" = "You try to use `add_total_row` option with `ybreak_var_name = \"none\"`.",
        "Please provide `ybreak_var_name`.",
        paste0("Default `add_total_row = \"no\"` will be used instead of: ", add_total_row)
      ))
      rt <- FALSE
    }
  } else{rt <- FALSE}
  
  #prepare collapse_ci option
  
  ci <- collapse_ci
  
  #final check sir_df
  
  required_vars <- c(if(fu){fubreak_var_name}, if(yb){ybreak_var_name}, if(xb){xbreak_var_name},
                     "sir_lci", "sir_uci", "age", "region", "sex", "year", "t_site", "observed",
                     "expected", "sir", "pyar", "n_base", "ref_inc_cases", "ref_population_pyar",
                     if(rs){"race"})
  
  
  not_found_vars <- required_vars[!(required_vars %in% colnames(sir_df))]
  
  if (length(not_found_vars) > 0) {
    rlang::abort(c(
      "The following variables required are not found in the provided dataframe `sir_df`:",
      paste(not_found_vars, collapse = ", "),
      "!" = "Make sure that `sir_df` provided is a results file from `msSPChelpR::sir_byfutime()`"
    ))
  }
  
  
  #check that FU totals are present, if ft == TRUE
  
  if(ft){
    if(any(stringr::str_detect(unique(sir_df$fu_time), "Total")) == FALSE) {
      rlang::warn(c(
        paste0("There is are no follow-up time totals found in `sir_df` in variable ", fubreak_var_name, "."),
        "Default `add_total_fu = no` will be used instead.",
        "!" = "Make sure that when you run the function `msSPChelpR::sir_byfutime()` the option `calc_total_fu = TRUE` is used."
      ))
      ft <- FALSE
      add_total_fu <- "no"
    }
  }
  
  #check that row totals are present, if rt == TRUE
  
  if(rt){
    if(any(stringr::str_detect(unique(sir_df$yvar_name), "total_var")) == FALSE) {
      rlang::warn(c(
        paste0("There is are no row totals found in `sir_df` in variable ", ybreak_var_name, "."),
        "Default `add_total_row = no` will be used instead.",
        "!" = "Make sure that when you run the function `msSPChelpR::sir_byfutime()` the option `calc_total_row = TRUE` is used."
      ))
      rt <- FALSE
      add_total_row <- "no"
    }
  }
  
  
  #---- summarize option
  
  if(!sg){
    #if no summarize option is chosen, then just rename variables for easier handling later
    if(fu){
      sum_pre <-  sir_df
    }
  }
  
  #else do the summarizing
  if(sg){
    
    #o) get used values for labels
    
    used_sex <- unique(sir_df$sex) 
    used_region <- unique(sir_df$region)  
    used_year <- unique(sir_df$year)  
    used_age <- unique(sir_df$age)  
    if(rs){used_race <- unique(sir_df$race)}
    used_t_site <- unique(sir_df$t_site)  
    
    min_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% min()
    max_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% max()
    
    min_age <- stringr::str_sub(used_age, 1, 2) %>% as.numeric() %>% min()
    max_age <- stringr::str_sub(used_age, -3) %>% as.numeric() %>% max()
    
    
    #i) CHK for missing summarize vars
    sg_not_found <- sg_var_names[!(sg_var_names %in% colnames(sir_df))]
    
    
    if (length(sg_not_found) > 0) {
      rlang::abort(c(
        "The following variables defined in `summarize_groups` are not found in the results dataframe:",
        paste(sg_not_found, collapse = ", ")
      ))
    }
    
    #ii) create vector with all possible grouping vars and CHK
    all_grouping_vars <- c("age", "sex", "region", "year", if(rs){"race"}, "t_site",
                           if(fu){c("fu_time", "fu_time_sort")},
                           if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, if(xb){c("xvar_name", "xvar_label")})
    
    sg_not_possible <- sg_var_names[!(sg_var_names %in% all_grouping_vars)]
    
    
    if (length(sg_not_possible) > 0) {
      rlang::abort(c(
        "The following variables defined in `summarize_groups` are not possible or meaningful to use: ",
        paste(sg_not_possible, collapse = ", ")
      ))
    }
    
    #iii) remove from grouping vars those who should be summarized
    grouping_vars <- all_grouping_vars[!(all_grouping_vars %in% sg_var_names)]
    if(site_grouped == TRUE){
      grouping_vars_with_site <- c(grouping_vars, "t_site_orig")
    } else{
      grouping_vars_with_site <- c(grouping_vars, "t_site")
    }
    
    #iv) summarize over grouping vars
    
    #first all variables that are varying across t_site
    sum_pre_tmp_a <- sir_df %>%
      tidytable::summarize.(tidytable::across.(
        .cols = c(observed, expected, ref_inc_cases),
        .fns =  ~ sum(.x, na.rm = TRUE),
        .names = "group_{.col}") ,
        .by = !!grouping_vars) %>%
      #calculate sir
      tidytable::mutate.(
        sir = .data$group_observed / .data$group_expected,
        sir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$group_observed) / 2) / .data$group_expected,
        sir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$group_observed + 1)) / 2) / .data$group_expected,
      ) %>%
      tidytable::distinct.()
    
    #second, variables that are constant over t_site
    sum_pre_tmp_b <- sir_df %>%
      tidytable::summarize.(tidytable::across.(
        .cols = c(pyar, n_base, ref_population_pyar),
        .fns =  ~ round(sum(.x, na.rm = TRUE), 2),
        .names = "group_{.col}"),
        .by = !!grouping_vars_with_site) %>% 
      tidytable::select.(-tidyselect::any_of("t_site_orig")) %>%
      tidytable::distinct.(tidyselect::all_of(c(grouping_vars, "group_pyar", "group_n_base", "group_ref_population_pyar")), .keep_all = TRUE)
    
    #in some instances, there will be ambiguous results for n_base and ref_population_pyar
    #created by the fact that sir_results is filtered to exclude non informative strata 
    #(i.e. no case occured in the reference population and no FU time contributed in the dataset).
    #This occurs for strata where cases are observed that don't occur in refrates_df. --> see sir_df$warning)
    #therefore we include a check
    
    if(nrow(sum_pre_tmp_b) != nrow(sum_pre_tmp_a)){
      #give warning
      rlang::warn(c(
        "The results file `sir_df` contains observed cases in i_observed that do not occur in the refrates_df (ref_inc_cases).",
        "Therefore calculation of the variables n_base and ref_population_pyar is ambiguous.",
        "We take the first value of each variable. Expect small inconsistencies in the calculation of n_base, ref_population_pyar and ref_inc_crude_rate across strata.",
        "!" = "If you want to know more, please check the `warnings` column of `sir_df`."
      ))
      #take first results only
      sum_pre_tmp_b <- sum_pre_tmp_b %>%
        tidytable::distinct.(tidyselect::all_of(c(grouping_vars, "group_pyar")), .keep_all = TRUE)
    }
    
    #check that merge will work
    if(nrow(sum_pre_tmp_b) != nrow(sum_pre_tmp_a)){
      #give warning
      rlang::abort(c(
        "Merge error.",
        "x" = "sum_pre_tmp_a and sum_pre_tmp_b have unequal numbers of rows."
      ))
    }
    
    #now merge a and b
    sum_pre_tmp <- sum_pre_tmp_a %>%
      tidytable::left_join.(sum_pre_tmp_b, by = grouping_vars) %>%
      #calculate crude rate
      tidytable::mutate.(
        group_incidence_crude_rate = .data$group_ref_inc_cases / .data$group_ref_population_pyar * 100000
      ) %>%
      #ensure same sorting as before
      tidytable::select.(tidyselect::all_of(
        c(grouping_vars, 
          "group_observed", "group_expected", "sir", "sir_lci", "sir_uci",
          "group_pyar", "group_n_base", "group_ref_inc_cases", "group_ref_population_pyar", "group_incidence_crude_rate")
      ))
    
    rm(sum_pre_tmp_a, sum_pre_tmp_b)
    
    
    
    #v) #add grouping information for summarized variables
    
    
    if("age" %in% sg_var_names == TRUE){
      if(shorten_total_cols == FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(age = paste0("Total - All included ages: ",  min_age, " - ", max_age))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(age = "Total")
      }
    }
    
    if("sex" %in% sg_var_names == TRUE){
      if(shorten_total_cols == FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(sex = paste0("Total - All included sexes: ", paste(used_sex, collapse = ", ")))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(sex = "Total")
      }
    }
    
    if("region" %in% sg_var_names == TRUE){
      if(shorten_total_cols == FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(region = paste0("Total - All included regions: ", paste(used_region, collapse = ", ")))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(region = "Total")
      }
    }
    
    if("year" %in% sg_var_names == TRUE){
      if(shorten_total_cols == FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(year = paste0("Total - All included years: ", min_year, " - ", max_year))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(year = "Total")
      }
    }
    
    if("race" %in% sg_var_names == TRUE){
      if(shorten_total_cols == FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(race = paste0("Total - All included races: ", paste(used_race, collapse = ", ")))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(race = "Total")
      }
    }
    
    if("t_site" %in% sg_var_names == TRUE){
      if(shorten_total_cols == FALSE){
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(t_site = paste0("Total - All included Tumor sites: ", paste(used_t_site, collapse = ", ")))
      } else{
        sum_pre_tmp <- sum_pre_tmp %>%
          tidytable::mutate.(t_site = "Total")
      }
    }
    
    #rounding
    
    sum_pre_tmp <- sum_pre_tmp %>%
      tidytable::mutate.(tidytable::across.(.cols = c(group_pyar, sir, sir_lci, sir_uci), 
                                            .fns = ~ round(.x, 2)))
    
    #do collapse_ci
    if(ci){
      sum_pre_tmp <- sum_pre_tmp %>% 
        tidytable::unite.("sir_ci", sir_lci, sir_uci, sep = " - ")
    } 
    
    
    #label
    
    sum_pre <- sum_pre_tmp %>%
      tidytable::rename.(observed = group_observed,
                         expected = group_expected,
                         pyar = group_pyar,
                         n_base = group_n_base,
                         ref_inc_cases = group_ref_inc_cases,
                         ref_population_pyar = group_ref_population_pyar,
                         ref_inc_crude_rate = group_incidence_crude_rate) %>%
      tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", if(rs){"race"},
                                              if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, 
                                              if(xb){c("xvar_name", "xvar_label")}, 
                                              if(fu){c("fu_time", "fu_time_sort")}, 
                                              "t_site", "observed", "expected", "sir",
                                              if(collapse_ci == TRUE){"sir_ci"},
                                              if(collapse_ci == FALSE){c("sir_lci", "sir_uci")})),
                         tidyselect::everything()
      ) 
    
    
  }
  
  
  
  
  #change output information required
  
  ##--- enforce add_total_fu
  
  if(add_total_fu == "only"){
    sum_pre <- sum_pre %>%
      tidytable::filter.(substr(fu_time, 1, 5) == "Total")
  }
  
  if(add_total_fu == "start"){
    sum_pre <- sum_pre %>%
      #set sorting value for Totals to 0, so it appears first
      tidytable::mutate.(fu_time_sort = tidytable::case.(substr(.data$fu_time, 1, 5) == "Total", 0,
                                                         default = .data$fu_time_sort))
  }
  
  if(add_total_fu == "end"){
    sum_pre <- sum_pre %>%
      #set sorting value for Totals to 999, so it appears last
      tidytable::mutate.(fu_time_sort = tidytable::case.(substr(.data$fu_time, 1, 5) == "Total", 999,
                                                         default = .data$fu_time_sort))
  }
  
  ##--- sort
  
  #since tidytable::arrange.() does not support tidyselect, we need to create a list of symbols to pass on
  arrange_vars <- rlang::syms(c("age", "region", "sex", "year", if(rs){"race"},
                                if(yb){c("yvar_sort", "yvar_sort_levels")}, if(xb){c("xvar_name", "xvar_label")}, 
                                if(fu){"fu_time_sort"}, "t_site"))
  
  sum_pre <- sum_pre %>% 
    tidytable::arrange.(!!!arrange_vars)
  
  ##--- enforce add_total_row
  if(rt){
    
    #calculate totals
    totals <- sum_pre %>%
      tidytable::filter.(yvar_name == "total_var") %>%
      tidytable::summarize.(
        yvar_name = tidytable::first.(.data$yvar_name),
        group_observed = sum(.data$observed, na.rm = TRUE),
        group_pyar = tidytable::first.(.data$pyar),
        group_n_base = tidytable::first.(.data$n_base),
        group_ref_inc_cases = sum(.data$ref_inc_cases),
        group_ref_population_pyar = tidytable::first.(.data$ref_population_pyar),
        group_expected = sum(.data$expected, na.rm = TRUE),
        .by = tidyselect::any_of(c("yvar_label", "fu_time", "fu_time_sort")))%>%
      #calculate sir
      tidytable::mutate.(
        sir = .data$group_observed / .data$group_expected,
        sir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$group_observed) / 2) / .data$group_expected,
        sir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$group_observed + 1)) / 2) / .data$group_expected,
        group_incidence_crude_rate = .data$group_ref_inc_cases / .data$group_ref_population_pyar * 100000
      ) %>%
      #rename and select required vars
      tidytable::rename.(observed = group_observed,
                         expected = group_expected,
                         pyar = group_pyar,
                         n_base = group_n_base,
                         ref_inc_cases = group_ref_inc_cases,
                         ref_population_pyar = group_ref_population_pyar,
                         ref_inc_crude_rate = group_incidence_crude_rate) %>%
      tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", if(rs){"race"},
                                              if(yb){c("yvar_name", "yvar_label")}, if(xb){c("xvar_name", "xvar_label")}, 
                                              if(fu){c("fu_time", "fu_time_sort")}, 
                                              "t_site", "observed", "expected", "sir",
                                              if(collapse_ci == TRUE){"sir_ci"},
                                              if(collapse_ci == FALSE){c("sir_lci", "sir_uci")})),
                         tidyselect::everything()
      ) %>%
      tidytable::mutate.(age = paste0("Total - All included ages: ",  min_age, " - ", max_age),
                         region = paste0("Total - All included regions: ", paste(used_region, collapse = ", ")),
                         sex = paste0("Total - All included sexes: ", paste(used_sex, collapse = ", ")),
                         year = paste0("Total - All included years: ", min_year, " - ", max_year),
                         t_site = paste0("Total - All included Tumor sites: ", paste(used_t_site, collapse = ", "))
      ) %>%
      tidytable::mutate.(tidytable::across.(.cols = c(pyar, sir, sir_lci, sir_uci), 
                                            .fns = ~ round(.x, 2)))
    
    
    if(shorten_total_cols == TRUE){
      totals <- totals %>%
        tidytable::mutate.(age = "Total",
                           region = "Total",
                           sex = "Total",
                           year = "Total",
                           t_site = "Total")
    }
    
    if(rs & shorten_total_cols == FALSE){
      totals <- totals %>%
        tidytable::mutate.(race = paste0("Total - All included races: ", paste(used_race, collapse = ", ")))
      
    } else{
      if(rs & shorten_total_cols == TRUE){
        totals <- totals %>%
          tidytable::mutate.(race = "Total")
      }
    }
    
    if(fu){
      totals <- totals  %>%
        tidytable::arrange.(fu_time_sort)
    }
    
    # "only" --> only keep totals
    if(add_total_row == "only"){
      sum_pre <- totals
    }
    
    # "start" --> bind totals to start
    if(add_total_row == "start"){
      sum_pre <- tidytable::bind_rows.(totals, sum_pre) %>%
        tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", if(rs){"race"},
                                                if(yb){c("yvar_name", "yvar_label")}, if(xb){c("xvar_name", "xvar_label")}, 
                                                if(fu){c("fu_time", "fu_time_sort")},
                                                "t_site", "observed", "expected", "sir",
                                                if(collapse_ci == TRUE){"sir_ci"},
                                                if(collapse_ci == FALSE){c("sir_lci", "sir_uci")})),
                           tidyselect::everything())
    }
    
    # "end" --> bind totals to end
    if(add_total_row == "end"){
      sum_pre <- tidytable::bind_rows.(sum_pre, totals)
    }
    
  }
  
  ##--- enforce option output_information
  #full
  if(output_information == "full"){
    
    sum_pre2 <- sum_pre %>%
      tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", if(rs){"race"}, 
                                              if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, 
                                              if(xb){c("xvar_name", "xvar_label")}, 
                                              if(fu){c("fu_time", "fu_time_sort")}, 
                                              "t_site", "observed", "expected", "sir",
                                              if(collapse_ci == TRUE){"sir_ci"},
                                              if(collapse_ci == FALSE){c("sir_lci", "sir_uci")},
                                              "pyar", "n_base")
      ),
      tidyselect::everything())
  }
  #reduced
  if(output_information == "reduced"){
    sum_pre2 <- sum_pre %>%
      tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", if(rs){"race"}, 
                                              if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, 
                                              if(xb){c("xvar_name", "xvar_label")}, 
                                              if(fu){c("fu_time", "fu_time_sort")}, 
                                              "t_site", "observed", "expected", "sir",
                                              if(collapse_ci == TRUE){"sir_ci"},
                                              if(collapse_ci == FALSE){c("sir_lci", "sir_uci")},
                                              "pyar", "n_base")))
  }
  #minimal
  if(output_information == "minimal"){
    if(!ci){
      sum_pre <- sum_pre %>%
        #make sure CIs are minimal
        tidytable::unite.("sir_ci", sir_lci, sir_uci, sep = " - ")
      
      ci <- TRUE
    }
    
    #select only required vars
    sum_pre2 <- sum_pre %>%
      tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", if(rs){"race"}, 
                                              if(yb){c("yvar_name", "yvar_label", "yvar_sort", "yvar_sort_levels")}, 
                                              if(xb){c("xvar_name", "xvar_label")}, 
                                              if(fu){c("fu_time", "fu_time_sort")}, 
                                              "t_site", "observed", "expected", "sir", "sir_ci")))
  }
  
  ##--- enforce option output
  #reshaping according to output option
  
  #output == "long"
  if(output == "long"){
    sum_results <- sum_pre2
  }
  
  #output = "wide"
  if(output == "wide"){
    #wide - only FU-times need to be transposed
    
    trans_vars <- names(sum_pre2)[!names(sum_pre2) %in% c("age", "region", "sex", "year", if(rs){"race"}, "t_site", 
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
      rlang::warn("No break variables (futime, xvar, yvar) provided. Nothing to reshape. Returning long results.")
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
      stringr::str_replace_all("_pyar", "_6pyar") %>% stringr::str_replace_all("_n_base", "_7n_base") %>%
      stringr::str_sort() %>%
      stringr::str_replace_all(stringr::regex("\\_[:digit:]"), "_") 
    
    
    #sort dataframe
    sum_results <- sum_results_pre %>%
      #sort columns
      tidytable::select.(tidyselect::any_of(c("age", "region", "sex", "year", if(rs){"race"}, 
                                              if(yb & !yb_off){c("yvar_name", "yvar_label")}, 
                                              "t_site", sort)),
                         tidyselect::everything())
    
    names(sum_results) <- names(sum_results) %>% 
      stringr::str_replace_all(stringr::regex("[:digit:]\\*\\*"), "")
    
  }
  
  #output = "nested"
  if(output == "nested"){
    
    # determine nesting variables
    nesting_vars <- c("")
    
    if(sg){
      nesting_vars <- sg_var_names
    }
    if(fu){
      nesting_vars <- c(nesting_vars, "fu_time", "fu_time_sort")
    }
    if(yb){
      nesting_vars <- c(nesting_vars, "yvar_name", "yvar_sort", "yvar_label", "yvar_label_sort")
    }
    if(xb){
      nesting_vars <- c(nesting_vars, "xvar_name", "xvar_sort")
    }
    
    #only do nesting when nesting_vars present
    if(length(nesting_vars) > 0){
      sum_results <- sum_pre %>%
        tidytable::nest_by.(tidyselect::any_of(nesting_vars))
    }else{
      rlang::warn("No nesting variables found. Nothing to reshape. Returning long results.")
      sum_results <- sum_pre
    }
  }
  
  return(sum_results) 
  
}

