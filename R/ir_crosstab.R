
#' Calculate crude incidence rates and crosstabulate results by break variables
#'
#' @param df dataframe in wide format
#' @param dattype can be "zfkd" or "seer" or empty. Will set default variable names from dataset.
#' @param count_var variable to be counted as observed case. Should be 1 for case to be counted.
#' @param xbreak_var variable from df by which rates should be stratified in columns of result df. Default is "none".
#' @param ybreak_vars variables from df by which rates should be stratified in rows of result df. Multiple variables will result in
#'                    appended rows in result df. y_break_vars is required.
#' @param collapse_ci If TRUE upper and lower confidence interval will be collapsed into one column separated by "-". Default is FALSE.
#' @param add_total option to add a row of totals. Can be either "no" for not adding such a row or "top" or "bottom" for adding it at the first or last row. Default is "no".
#' @param add_n_percentages option to add a column of percentages for n_base in its respective yvar_group. Can only be used when xbreak_var = "none". Default is FALSE.
#' @param futime_var variable in df that contains follow-up time per person (in years). Default is set if dattype is given.
#' @param alpha significance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @return df
#' @importFrom rlang .data
#' @export 
#' @examples 
#' #load sample data
#' data("us_second_cancer")
#' 
#' #prep step - make wide data as this is the required format
#' usdata_wide <- us_second_cancer %>%
#'                     msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
#'                     time_id_var = "SEQ_NUM", timevar_max = 10)
#'                     
#' #prep step - calculate p_spc variable
#' usdata_wide <- usdata_wide %>%
#'                  dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
#'                                                        !is.na(t_site_icd.2)   ~ "SPC developed",
#'                                                        TRUE ~ NA_character_)) %>%
#'                  dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
#'                                                               TRUE ~ 0))
#'                                                               
#' #prep step - create patient status variable
#' usdata_wide <- usdata_wide %>%
#'                   msSPChelpR::pat_status(., fu_end = "2017-12-31", dattype = "seer",
#'                                          status_var = "p_status", life_var = "p_alive.1",
#'                                          birthdat_var = "datebirth.1", lifedat_var = "datedeath.1")
#'  
#' #now we can run the function
#' usdata_wide <- usdata_wide %>%
#'                  msSPChelpR::calc_futime(., 
#'                         futime_var_new = "p_futimeyrs", 
#'                         fu_end = "2017-12-31",
#'                         dattype = "seer", 
#'                         time_unit = "years",
#'                         status_var = "p_status",
#'                         lifedat_var = "datedeath.1", 
#'                         fcdat_var = "t_datediag.1", 
#'                         spcdat_var = "t_datediag.2")
#'                     
#' #for example, you can calculate incidence and summarize by sex and registry
#' msSPChelpR::ir_crosstab(usdata_wide,
#'       dattype = "seer",
#'       count_var = "count_spc",
#'       xbreak_var = "none",
#'       ybreak_vars = c("sex.1", "registry.1"),
#'       collapse_ci = FALSE,
#'       add_total = "no",
#'       add_n_percentages = FALSE,
#'       futime_var = "p_futimeyrs",
#'       alpha = 0.05)
#' 
#'



ir_crosstab <-
  function(df,
           dattype = "zfkd",
           count_var,
           xbreak_var = "none",
           ybreak_vars,
           collapse_ci = FALSE,
           add_total = "no",
           add_n_percentages = FALSE,
           futime_var = NULL,
           alpha = 0.05) {
    
    ###----  prepwork
    
    #setting default parameters
    options_dplyr_old <- options(dplyr.summarise.inform = TRUE) # save old setting for showing dplyr messages
    on.exit(options(options_dplyr_old), add = TRUE) #make sure old options are used when exiting function
    options(dplyr.summarise.inform = FALSE) #set new setting for not showing dplyr messages to avoid outbut by summarize()
    
    
    ### check if df exists and is dataframes
    
    if (exists("df") && is.data.frame(get("df"))){}
    else{
      rlang::abort(paste0("The following df for for providing the observed cases does not exist or is not a dataframe: ",
                          rlang::as_name(df)))
    }
    
    ### remove all labels from dfs to avoid warning messages
    
    df <- sjlabelled::remove_all_labels(df)
    
    ### get variable names
    
    count_var <- rlang::ensym(count_var)
    if(xbreak_var != "none"){
      xbreak_var <- rlang::ensym(xbreak_var)
      xb <- TRUE # indicator that xbreak_var is used
    } else{
      xb <- FALSE
    }
    
    ybreak_vars <- rlang::enquo(ybreak_vars)
    ybreak_var_names <- rlang::eval_tidy(ybreak_vars)
    
    #set option for percentatges if add_n_percentages = TRUE, but only works with no xbreak_variable
    
    perc <- add_n_percentages
    if(xb == TRUE & perc == TRUE){
      warning("Option add_n_percentages cannot be combinded with xbreak_var. No percentages will be calculated.")
      perc <- FALSE
    }
    
    
    ### setting default var names and values for SEER data --> still need to update to final names!
    if (dattype == "seer") {
      if (is.null(futime_var)) {
        futime_var <- rlang::sym("p_futimeyrs.1")
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
        if(xbreak_var != "none"){rlang::as_name(xbreak_var)}
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
    
    #DM1 - make change factor variables to character for ybreak_vars
    
    df_n <- df %>%
      dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(ybreak_var_names), .fns = as.character))
    
    y <- 1
    
    #AN1 - calculating rates for first ybreak_var
    
    single_ybreak_var <- rlang::sym(ybreak_var_names[y]) #getting y object in list of quosures to be used as ybreak_var in this loop
    
    ratecount <- df_n %>%
      {if(xb){dplyr::group_by(., !!single_ybreak_var, !!xbreak_var)} else {dplyr::group_by(., !!single_ybreak_var)}} %>% 
      dplyr::summarize(
        n_base = dplyr::n(),
        observed = sum(!!count_var, na.rm = TRUE),
        pyar = sum(!!futime_var, na.rm = TRUE),
        abs_ir = .data$observed / .data$pyar * 100000,
        abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
        abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
      ) %>%
      #rounding of values
      dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
      dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
      #if add_n_percentages option is true, add calculation
      {if(perc){dplyr::mutate(., n_perc = round(.data$n_base / sum(.data$n_base), 2))} else {.}}  %>% 
      dplyr::ungroup() %>%
      {if(xb){tidyr::complete(., !!single_ybreak_var, !!xbreak_var)} else {tidyr::complete(., !!single_ybreak_var)}} #create NA lines for all combinations of ybreak_var and xbreak_var not present in the dataset (needed later for transposing)
    
    #collapse upper and lower CI limit if option is set
    if(collapse_ci == TRUE){
      ratecount <- ratecount %>%
        tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
    }
    
    if(xb == FALSE){
      ratecount_result <- ratecount %>%
        dplyr::rename(yvar_label = !!single_ybreak_var)     #creating ratecount_result later needed and changing variable name
    }
    
    else{
      ratecount_nest <- ratecount %>%
        dplyr::group_by(!!xbreak_var) %>%
        tidyr::nest()
      
      rescolnames <-
        ratecount_nest$data[[1]] %>% colnames() %>% .[1:ncol(ratecount_nest$data[[1]])] %>% paste0(ratecount_nest[[1]][[1]], "_", .)
      
      rescolnames[1] <- "yvar_label"
      
      ratecount_result <-
        stats::setNames(ratecount_nest$data[[1]], rescolnames)
      
      for (i in 2:length(ratecount_nest[[1]])) {
        rescolnames <-
          ratecount_nest$data[[i]] %>% # create vector of new variable names that combines level of xbreak_var with name of calculated metric (e.g. to6months_n_base)
          colnames() %>%
          .[2:ncol(ratecount_nest$data[[i]])] %>%          # skip the name of the first column, because this is replicated in each df of the list
          paste0(ratecount_nest[[1]][[i]], "_", .)
        
        ratecount_result <-
          cbind(ratecount_result,
                stats::setNames(ratecount_nest$data[[i]][2:ncol(ratecount_nest$data[[i]])], rescolnames))
      }
    }
    
    
    yvar_name_str <- df %>% 
      dplyr::select(!!single_ybreak_var) %>% colnames()
    
    ratecount_result <- ratecount_result %>%
      dplyr::mutate(yvar_name = !!yvar_name_str) %>%
      dplyr::select(.data$yvar_name, dplyr::everything())
    
    #AN2 - calculating rates for all other ybreak_vars
    
    if (length(ybreak_var_names) > 1) {
      
      for (y in 2:length(ybreak_var_names)) {
        
        single_ybreak_var <- rlang::sym(ybreak_var_names[y])  #getting y object in list of quosures to be used as ybreak_var in this loop
        
        ratecount <- df_n %>%
          {if(xb){dplyr::group_by(., !!single_ybreak_var, !!xbreak_var)} else {dplyr::group_by(., !!single_ybreak_var)}} %>% 
          dplyr::summarize(
            n_base = dplyr::n(),
            observed = sum(!!count_var, na.rm = TRUE),
            pyar = sum(!!futime_var, na.rm = TRUE),
            abs_ir = .data$observed / .data$pyar * 100000,
            abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
            abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
          ) %>%
          #rounding of values
          dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
          dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
          #if add_n_percentages option is true, add calculation
          {if(perc){dplyr::mutate(., n_perc = round(.data$n_base / sum(.data$n_base), 2))} else {.}}  %>% 
          dplyr::ungroup() %>%
          {if(xb){tidyr::complete(., !!single_ybreak_var, !!xbreak_var)} else {tidyr::complete(., !!single_ybreak_var)}} #create NA lines for all combinations of ybreak_var and xbreak_var not present in the dataset (needed later for transposing)
        
        #collapse upper and lower CI limit if option is set
        if(collapse_ci == TRUE){
          ratecount <- ratecount %>%
            tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
        }
        
        if(xb == FALSE){
          ratecount_result_tmp <- ratecount %>%
            dplyr::rename(yvar_label = !!single_ybreak_var)
        }
        
        else{
          ratecount_nest <- ratecount %>%
            dplyr::group_by(!!xbreak_var) %>%
            tidyr::nest()
          
          rescolnames <-
            ratecount_nest$data[[1]] %>% colnames() %>% .[1:ncol(ratecount_nest$data[[1]])] %>% paste0(ratecount_nest[[1]][[1]], "_", .)
          rescolnames[1] <- "yvar_label"
          
          ratecount_result_tmp <-
            cbind(stats::setNames(ratecount_nest$data[[1]], rescolnames))
          
          for (i in 2:length(ratecount_nest[[1]])) {
            rescolnames <-
              ratecount_nest$data[[i]] %>% # create vector of new variable names that combines level of xbreak_var with name of calculated metric (e.g. to6months_n_base)
              colnames() %>%
              .[2:ncol(ratecount_nest$data[[i]])] %>%          # skip the name of the first column, because this is replicated in each df of the list
              paste0(ratecount_nest[[1]][[i]], "_", .)
            
            ratecount_result_tmp <-
              cbind(ratecount_result_tmp,
                    stats::setNames(ratecount_nest$data[[i]][2:ncol(ratecount_nest$data[[i]])], rescolnames))
          }
        }
        
        
        yvar_name_str <- df %>% 
          dplyr::select(!!single_ybreak_var) %>% colnames()
        
        ratecount_result_tmp <- ratecount_result_tmp %>%
          dplyr::mutate(yvar_name = yvar_name_str) %>%
          dplyr::select(.data$yvar_name, dplyr::everything())
        
        ratecount_result <- rbind(ratecount_result, ratecount_result_tmp)
        
        
      }
      
      ### add option for total row
      
      if(add_total == "top" | add_total == "bottom"){
        
        df_n <- df_n %>%
          dplyr::mutate(total_var = "Total")
        
        single_ybreak_var <- rlang::sym("total_var")  #setting the constant "total_var" as break_var
        
        ratecount <- df_n %>%
          {if(xb){dplyr::group_by(., !!single_ybreak_var, !!xbreak_var)} else {dplyr::group_by(., !!single_ybreak_var)}} %>% 
          dplyr::summarize(
            n_base = dplyr::n(),
            observed = sum(!!count_var, na.rm = TRUE),
            pyar = sum(!!futime_var, na.rm = TRUE),
            abs_ir = .data$observed / .data$pyar * 100000,
            abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$observed) / 2) / .data$pyar * 100000,
            abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$observed + 1)) / 2) / .data$pyar * 100000
          ) %>%
          #rounding of values
          dplyr::mutate(dplyr::across(.cols = c(.data$pyar), .fns = ~round(., 0))) %>%
          dplyr::mutate(dplyr::across(.cols = c(.data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci), .fns = ~round(., 2))) %>%
          #if add_n_percentages option is true, add calculation
          {if(perc){dplyr::mutate(., n_perc = round(.data$n_base / sum(.data$n_base), 2))} else {.}}  %>% 
          dplyr::ungroup() %>%
          {if(xb){tidyr::complete(., !!single_ybreak_var, !!xbreak_var)} else {tidyr::complete(., !!single_ybreak_var)}} #create NA lines for all combinations of ybreak_var and xbreak_var not present in the dataset (needed later for transposing)
        
        #collapse upper and lower CI limit if option is set
        if(collapse_ci == TRUE){
          ratecount <- ratecount %>%
            tidyr::unite("abs_ir_ci", .data$abs_ir_lci, .data$abs_ir_uci, sep = " - ")
        }
        
        if(xb == FALSE){
          ratecount_result_tmp <- ratecount %>%
            dplyr::rename(yvar_label = !!single_ybreak_var)
        }
        
        else{
          ratecount_nest <- ratecount %>%
            dplyr::group_by(!!xbreak_var) %>%
            tidyr::nest()
          
          rescolnames <-
            ratecount_nest$data[[1]] %>% colnames() %>% .[1:ncol(ratecount_nest$data[[1]])] %>% paste0(ratecount_nest[[1]][[1]], "_", .)
          rescolnames[1] <- "yvar_label"
          
          ratecount_result_tmp <-
            cbind(stats::setNames(ratecount_nest$data[[1]], rescolnames))
          
          for (i in 2:length(ratecount_nest[[1]])) {
            rescolnames <-
              ratecount_nest$data[[i]] %>% # create vector of new variable names that combines level of xbreak_var with name of calculated metric (e.g. to6months_n_base)
              colnames() %>%
              .[2:ncol(ratecount_nest$data[[i]])] %>%          # skip the name of the first column, because this is replicated in each df of the list
              paste0(ratecount_nest[[1]][[i]], "_", .)
            
            ratecount_result_tmp <-
              cbind(ratecount_result_tmp,
                    stats::setNames(ratecount_nest$data[[i]][2:ncol(ratecount_nest$data[[i]])], rescolnames))
          }
        }
        
        yvar_name_str <- df_n %>% 
          dplyr::select(!!single_ybreak_var) %>% colnames()
        
        ratecount_result_tmp <- ratecount_result_tmp %>%
          dplyr::mutate(yvar_name = yvar_name_str) %>%
          dplyr::select(.data$yvar_name, dplyr::everything())
        
        if(add_total == "top"){
          ratecount_result <- rbind(ratecount_result_tmp, ratecount_result)
        } else{
          ratecount_result <- rbind(ratecount_result, ratecount_result_tmp)
        }
      }
      
      return(ratecount_result)
      
      
    }
  }

