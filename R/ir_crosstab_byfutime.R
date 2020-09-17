
#' Calculate crude incidence rates and cross-tabulate results by break variables; cumulative FU-times as are used as xbreak_var
#'
#' @param df dataframe in wide format
#' @param dattype can be "zfkd" or "seer" or empty. Will set default variable names from dataset.
#' @param count_var variable to be counted as observed case. Should be 1 for case to be counted.
#' @param futime_breaks vector that indicates split points for follow-up time groups (in years) that will be used as xbreak_var.
#'                      Default is c(0, .5, 1, 5, 10, Inf) that will result in 5 groups (up to 6 months, 6-12 months, 1-5 years, 5-10 years, 10+ years). 
#' @param ybreak_vars variables from df by which rates should be stratified in rows of result df. Multiple variables will result in
#'                    appended rows in result df. y_break_vars is required.
#' @param collapse_ci If TRUE upper and lower confidence interval will be collapsed into one column separated by "-". Default is FALSE.
#' @param add_total option to add a row of totals. Can bei either "no" for not adding such a row or "top" or "bottom" for adding it at the first or last row. Default is "no".
#' @param futime_var variable in df that contains follow-up time per person (in years). Default is set if dattype is given.
#' @param alpha signifcance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @return df
#' @importFrom rlang .data
#' @export 
#'


ir_crosstab_byfutime <- function(df,
                                 dattype = "zfkd",
                                 count_var,
                                 futime_breaks = c(0, .5, 1, 5, 10, Inf),
                                 ybreak_vars,
                                 collapse_ci = FALSE,
                                 add_total = "no",
                                 futime_var = NULL,
                                 alpha = 0.05) {
  
  ###----  prepwork
  
  #setting default parameters
  options_dplyr_old <- options(dplyr.summarise.inform = TRUE) # save old setting for showing dplyr messages
  on.exit(options(options_dplyr_old), add = TRUE) #make sure old options are used when exiting function
  options(dplyr.summarise.inform = FALSE) #set new setting for not showing dplyr messages to avoid outbut by summarize()
  
  ### remove all labels from dfs to avoid warning messages
  df <- sjlabelled::remove_all_labels(df)
  
  ### get variable names
  
  futime_breaks_quo <- rlang::enquo(futime_breaks)
  
  xb <- TRUE
  
  ybreak_vars <- rlang::enquo(ybreak_vars)
  ybreak_var_names <- rlang::eval_tidy(ybreak_vars)
  
  count_var <- rlang::ensym(count_var)
  
  
  ### setting default var names and values for SEER data --> still need to update to final names!
  if (dattype == "seer") {
    if (is.null(futime_var)) {
      futime_var <- rlang::sym("p_futime.1")
    } else{
      futime_var <- rlang::ensym(futime_var)
    }
  }
  
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd") {
    if (is.null(futime_var)) {
      futime_var <- rlang::sym("p_futimeyrs.1")
    } else{
      futime_var <- rlang::ensym(futime_var)
    }
  }
  
  
  #CHK1: check whether all required variables are defined and present in dataset
  defined_vars <-
    c(
      rlang::as_name(count_var),
      ybreak_var_names, 
      rlang::as_name(futime_var)
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
  
  #DM1 - change factor variables to character for ybreak_vars
  
  df_n <- df %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(ybreak_var_names), .fns = as.character))
  
  y <- 1
  
  #DM1b calculate futimegroup
  
  
  df_n <- df_n %>%
    dplyr::mutate(futimegroup = cut(!!futime_var, breaks = futime_breaks, right = FALSE))
  
  # capture and change futime levels to make grouping readable 
  futimegroup_levels <- levels(df_n$futimegroup) %>% 
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
  levels(df_n$futimegroup) <- futimegroup_levels
  
  #normalized names without spaces and special characters
  futimegroup_levels_norm <- futimegroup_levels %>% 
    stringr::str_replace_all(stringr::fixed(" "), "") %>% 
    stringr::str_replace_all("-", "to") %>% 
    stringr::str_replace_all("\\.", "") %>% 
    stringr::str_replace_all("\\+", "plus") %>% 
    stringr::str_replace(stringr::regex("^[[:digit:]]"), "x")
  
  
  #create dummy variables for each level of fub
  for (lv in 1:nlevels(df_n$futimegroup)){
    df_n[, paste0(futimegroup_levels[lv])] <- ifelse(as.numeric(df_n$futimegroup) >= lv, 1, 0)
  }
  
  xbreak_var_names <- futimegroup_levels
  xbreak_vars <- rlang::syms(xbreak_var_names)
  
  #CHK - check that there are no missing values for futimegroups
  
  chk_na <- df_n %>% dplyr::filter(is.na(.data$futimegroup)) %>% nrow()
  
  if (chk_na > 0) {
    rlang::warn(
      paste0(
        "The variable for follow-up time has: ", chk_na, " missings. These will be omitted when creating the crosstabs.")
    )
  }
  
  #AN1 - calculating rates for first ybreak_var
  
  y <- 1
  x <- 1
  
  single_ybreak_var <- rlang::sym(ybreak_var_names[y]) #getting y object in list of quosures to be used as ybreak_var in this loop
  single_xbreak_var <- rlang::sym(xbreak_var_names[x])
  
  
  ratecount_result <- df_n %>%
    dplyr::mutate(count = dplyr::case_when(((!!count_var) == 1) & ((!!futime_var) >= futime_breaks[x]) & ((!!futime_var) < futime_breaks[x+1]) ~ 1,
                                           TRUE ~ 0),
                  futime = dplyr::case_when((!!futime_var) < futime_breaks[x] ~ 0,
                                            (!!futime_var) < futime_breaks[x+1] ~ ((!!futime_var) - futime_breaks[x]),
                                            (!!futime_var) >= futime_breaks[x+1] ~ (futime_breaks[x+1] - futime_breaks[x]),
                                            TRUE ~ 0),
                  base_n_fugroup = dplyr::case_when((!!single_xbreak_var) == 1 ~ 1,
                                                    TRUE           ~ 0)) %>%
    dplyr::group_by(., !!single_ybreak_var, !!single_xbreak_var) %>% 
    dplyr::summarize(
      n_base = sum(.data$base_n_fugroup, na.rm = TRUE),
      observed = sum(.data$count, na.rm = TRUE),
      pyar = sum(.data$futime, na.rm = TRUE),
      abs_ir = .data$observed / .data$pyar * 100000,
      abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
      abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
    ) %>%
    #rounding of values
    dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
    dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
    dplyr::ungroup() %>%
    #complete missing categories and clean up      tidyr::complete(., !!single_ybreak_var, !!single_xbreak_var) %>%
    dplyr::arrange(!!single_ybreak_var) %>%
    dplyr::filter(!!single_xbreak_var == 1) %>%
    dplyr::select(-!!single_xbreak_var)
  
  #collapse upper and lower CI limit if option is set
  if(collapse_ci == TRUE){
    ratecount_result <- ratecount_result %>%
      tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
  }
  
  rescolnames <-
    ratecount_result %>% colnames() %>% .[2:ncol(ratecount_result)] %>% paste0(rlang::as_name(single_xbreak_var), "_", .) %>% c("yvar_label", .)
  
  colnames(ratecount_result) <- rescolnames
  
  
  for (x in 2:length(xbreak_var_names)){
    
    single_xbreak_var <- rlang::sym(xbreak_var_names[x])
    
    ratecount_result_tmp <- df_n %>%
      dplyr::mutate(count = dplyr::case_when(((!!count_var) == 1) & ((!!futime_var) >= futime_breaks[x]) & ((!!futime_var) < futime_breaks[x+1]) ~ 1,
                                             TRUE ~ 0),
                    futime = dplyr::case_when((!!futime_var) < futime_breaks[x] ~ 0,
                                              (!!futime_var) < futime_breaks[x+1] ~ ((!!futime_var) - futime_breaks[x]),
                                              (!!futime_var) >= futime_breaks[x+1] ~ (futime_breaks[x+1] - futime_breaks[x]),
                                              TRUE ~ 0),
                    base_n_fugroup = dplyr::case_when((!!single_xbreak_var) == 1 ~ 1,
                                                      TRUE           ~ 0)) %>%
      dplyr::group_by(., !!single_ybreak_var, !!single_xbreak_var) %>% 
      dplyr::summarize(
        n_base = sum(.data$base_n_fugroup, na.rm = TRUE),
        observed = sum(.data$count, na.rm = TRUE),
        pyar = sum(.data$futime, na.rm = TRUE),
        abs_ir = .data$observed / .data$pyar * 100000,
        abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
        abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
      ) %>%
      #rounding of values
      dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
      dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
      dplyr::ungroup() %>%
      #complete missing cateories and clean up
      tidyr::complete(., !!single_ybreak_var, !!single_xbreak_var) %>%
      dplyr::arrange(!!single_ybreak_var) %>%
      dplyr::filter(!!single_xbreak_var == 1) %>%
      dplyr::select(-!!single_ybreak_var, -!!single_xbreak_var)
    
    #collapse upper and lower CI limit if option is set
    if(collapse_ci == TRUE){
      ratecount_result_tmp <- ratecount_result_tmp %>%
        tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
    }
    
    rescolnames <-
      ratecount_result_tmp %>% colnames() %>% paste0(rlang::as_name(single_xbreak_var), "_", .)
    
    colnames(ratecount_result_tmp) <- rescolnames
    
    ratecount_result <-
      cbind(ratecount_result, ratecount_result_tmp)
    
  }
  
  ratecount_result <- ratecount_result %>%
    dplyr::mutate(yvar_name = rlang::as_name(single_ybreak_var)) %>%
    dplyr::select(.data$yvar_name, dplyr::everything())
  
  
  #AN2 - calculating rates for all other ybreak_vars
  
  if (length(ybreak_var_names) > 1) {
    
    for (y in 2:length(ybreak_var_names)) {
      
      x <- 1
      
      single_ybreak_var <- rlang::sym(ybreak_var_names[y]) #getting y object in list of quosures to be used as ybreak_var in this loop
      single_xbreak_var <- rlang::sym(xbreak_var_names[x])
      
      
      ratecount_result_y <- df_n %>%
        dplyr::mutate(count = dplyr::case_when(((!!count_var) == 1) & ((!!futime_var) >= futime_breaks[x]) & ((!!futime_var) < futime_breaks[x+1]) ~ 1,
                                               TRUE ~ 0),
                      futime = dplyr::case_when((!!futime_var) < futime_breaks[x] ~ 0,
                                                (!!futime_var) < futime_breaks[x+1] ~ ((!!futime_var) - futime_breaks[x]),
                                                (!!futime_var) >= futime_breaks[x+1] ~ (futime_breaks[x+1] - futime_breaks[x]),
                                                TRUE ~ 0),
                      base_n_fugroup = dplyr::case_when((!!single_xbreak_var) == 1 ~ 1,
                                                        TRUE           ~ 0)) %>%
        dplyr::group_by(., !!single_ybreak_var, !!single_xbreak_var) %>% 
        dplyr::summarize(
          n_base = sum(.data$base_n_fugroup, na.rm = TRUE),
          observed = sum(.data$count, na.rm = TRUE),
          pyar = sum(.data$futime, na.rm = TRUE),
          abs_ir = .data$observed / .data$pyar * 100000,
          abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
          abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
        ) %>%
        #rounding of values
        dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
        dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
        dplyr::ungroup() %>%
        #complete missing cateories and clean up
        tidyr::complete(., !!single_ybreak_var, !!single_xbreak_var) %>%
        dplyr::arrange(!!single_ybreak_var) %>%
        dplyr::filter(!!single_xbreak_var == 1) %>%
        dplyr::select(-!!single_xbreak_var)
      
      #collapse upper and lower CI limit if option is set
      if(collapse_ci == TRUE){
        ratecount_result_y <- ratecount_result_y %>%
          tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
      }
      
      rescolnames <-
        ratecount_result_y %>% colnames() %>% .[2:ncol(ratecount_result_y)] %>% paste0(rlang::as_name(single_xbreak_var), "_", .) %>% c("yvar_label", .)
      
      colnames(ratecount_result_y) <- rescolnames
      
      
      for (x in 2:length(xbreak_var_names)){
        
        single_xbreak_var <- rlang::sym(xbreak_var_names[x])
        
        ratecount_result_ytmp <- df_n %>%
          dplyr::mutate(count = dplyr::case_when(((!!count_var) == 1) & ((!!futime_var) >= futime_breaks[x]) & ((!!futime_var) < futime_breaks[x+1]) ~ 1,
                                                 TRUE ~ 0),
                        futime = dplyr::case_when((!!futime_var) < futime_breaks[x] ~ 0,
                                                  (!!futime_var) < futime_breaks[x+1] ~ ((!!futime_var) - futime_breaks[x]),
                                                  (!!futime_var) >= futime_breaks[x+1] ~ (futime_breaks[x+1] - futime_breaks[x]),
                                                  TRUE ~ 0),
                        base_n_fugroup = dplyr::case_when((!!single_xbreak_var) == 1 ~ 1,
                                                          TRUE           ~ 0)) %>%
          dplyr::group_by(., !!single_ybreak_var, !!single_xbreak_var) %>% 
          dplyr::summarize(
            n_base = sum(.data$base_n_fugroup, na.rm = TRUE),
            observed = sum(.data$count, na.rm = TRUE),
            pyar = sum(.data$futime, na.rm = TRUE),
            abs_ir = .data$observed / .data$pyar * 100000,
            abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
            abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
          ) %>%
          #rounding of values
          dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
          dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
          dplyr::ungroup() %>%
          #complete missing categories and clean up
          tidyr::complete(., !!single_ybreak_var, !!single_xbreak_var) %>%
          dplyr::arrange(!!single_ybreak_var) %>%
          dplyr::filter(!!single_xbreak_var == 1) %>%
          dplyr::select(-!!single_ybreak_var, -!!single_xbreak_var)
        
        #collapse upper and lower CI limit if option is set
        if(collapse_ci == TRUE){
          ratecount_result_ytmp <- ratecount_result_ytmp %>%
            tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
        }
        
        rescolnames <-
          ratecount_result_ytmp %>% colnames() %>% paste0(rlang::as_name(single_xbreak_var), "_", .)
        
        colnames(ratecount_result_ytmp) <- rescolnames
        
        ratecount_result_y <-
          cbind(ratecount_result_y, ratecount_result_ytmp)
        
      }
      
      
      ratecount_result_y <- ratecount_result_y %>%
        dplyr::mutate(yvar_name = rlang::as_name(single_ybreak_var)) %>%
        dplyr::select(.data$yvar_name, dplyr::everything())
      
      
      ratecount_result <- rbind(ratecount_result, ratecount_result_y)
      
      
    }
    
    ### add option for total row
    
    if(add_total == "top" | add_total == "bottom"){
      
      
      df_n <- df_n %>%
        dplyr::mutate(total_var = "Total")
      
      x <- 1
      
      single_ybreak_var <- rlang::sym("total_var")  #setting the constant "total_var" as break_var
      single_xbreak_var <- rlang::sym(xbreak_var_names[x])
      
      
      
      ratecount_result_y <- df_n %>%
        dplyr::mutate(count = dplyr::case_when(((!!count_var) == 1) & ((!!futime_var) >= futime_breaks[x]) & ((!!futime_var) < futime_breaks[x+1]) ~ 1,
                                               TRUE ~ 0),
                      futime = dplyr::case_when((!!futime_var) < futime_breaks[x] ~ 0,
                                                (!!futime_var) < futime_breaks[x+1] ~ ((!!futime_var) - futime_breaks[x]),
                                                (!!futime_var) >= futime_breaks[x+1] ~ (futime_breaks[x+1] - futime_breaks[x]),
                                                TRUE ~ 0),
                      base_n_fugroup = dplyr::case_when((!!single_xbreak_var) == 1 ~ 1,
                                                        TRUE           ~ 0)) %>%
        dplyr::group_by(., !!single_ybreak_var, !!single_xbreak_var) %>% 
        dplyr::summarize(
          n_base = sum(.data$base_n_fugroup, na.rm = TRUE),
          observed = sum(.data$count, na.rm = TRUE),
          pyar = sum(.data$futime, na.rm = TRUE),
          abs_ir = .data$observed / .data$pyar * 100000,
          abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
          abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
        ) %>%
        #rounding of values
        dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
        dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
        dplyr::ungroup() %>%
        #complete missing cateories and clean up
        tidyr::complete(., !!single_ybreak_var, !!single_xbreak_var) %>%
        dplyr::arrange(!!single_ybreak_var) %>%
        dplyr::filter(!!single_xbreak_var == 1) %>%
        dplyr::select(-!!single_xbreak_var)
      
      #collapse upper and lower CI limit if option is set
      if(collapse_ci == TRUE){
        ratecount_result_y <- ratecount_result_y %>%
          tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
      }
      
      rescolnames <-
        ratecount_result_y %>% colnames() %>% .[2:ncol(ratecount_result_y)] %>% paste0(rlang::as_name(single_xbreak_var), "_", .) %>% c("yvar_label", .)
      
      colnames(ratecount_result_y) <- rescolnames
      
      
      
      for (x in 2:length(xbreak_var_names)){
        
        single_xbreak_var <- rlang::sym(xbreak_var_names[x])
        
        ratecount_result_ytmp <- df_n %>%
          dplyr::mutate(count = dplyr::case_when(((!!count_var) == 1) & ((!!futime_var) >= futime_breaks[x]) & ((!!futime_var) < futime_breaks[x+1]) ~ 1,
                                                 TRUE ~ 0),
                        futime = dplyr::case_when((!!futime_var) < futime_breaks[x] ~ 0,
                                                  (!!futime_var) < futime_breaks[x+1] ~ ((!!futime_var) - futime_breaks[x]),
                                                  (!!futime_var) >= futime_breaks[x+1] ~ (futime_breaks[x+1] - futime_breaks[x]),
                                                  TRUE ~ 0),
                        base_n_fugroup = dplyr::case_when((!!single_xbreak_var) == 1 ~ 1,
                                                          TRUE           ~ 0)) %>%
          dplyr::group_by(., !!single_ybreak_var, !!single_xbreak_var) %>% 
          dplyr::summarize(
            n_base = sum(.data$base_n_fugroup, na.rm = TRUE),
            observed = sum(.data$count, na.rm = TRUE),
            pyar = sum(.data$futime, na.rm = TRUE),
            abs_ir = .data$observed / .data$pyar * 100000,
            abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
            abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
          ) %>%
          #rounding of values
          dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
          dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
          dplyr::ungroup() %>%
          #complete missing cateories and clean up
          tidyr::complete(., !!single_ybreak_var, !!single_xbreak_var) %>%
          dplyr::arrange(!!single_ybreak_var) %>%
          dplyr::filter(!!single_xbreak_var == 1) %>%
          dplyr::select(-!!single_ybreak_var, -!!single_xbreak_var)
        
        #collapse upper and lower CI limit if option is set
        if(collapse_ci == TRUE){
          ratecount_result_ytmp <- ratecount_result_ytmp %>%
            tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
        }
        
        rescolnames <-
          ratecount_result_ytmp %>% colnames() %>% paste0(rlang::as_name(single_xbreak_var), "_", .)
        
        colnames(ratecount_result_ytmp) <- rescolnames
        
        ratecount_result_y <-
          cbind(ratecount_result_y, ratecount_result_ytmp)
        
      }
      
      
      ratecount_result_y <- ratecount_result_y %>%
        dplyr::mutate(yvar_name = rlang::as_name(single_ybreak_var)) %>%
        dplyr::select(.data$yvar_name, dplyr::everything())
      
      if(add_total == "top"){
        ratecount_result <- rbind(ratecount_result_y, ratecount_result)
      } else{
        ratecount_result <- rbind(ratecount_result, ratecount_result_y)
      }
    }
    
  }
  
  return(ratecount_result)
  
}

