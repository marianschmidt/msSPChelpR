
#' Calculate follow-up time per case until end of follow-up depending on pat_status - tidytable version
#'
#' @param wide_df dataframe or data.table in wide format
#' @param futime_var_new Name of the newly calculated variable for follow-up time. Default is p_futimeyrs.
#' @param fu_end end of follow-up in time format YYYY-MM-DD.
#' @param dattype Type of cancer registry data. Can be "seer" or "zfkd". Default is "zfkd".
#' @param check Check newly calculated variable p_status by printing frequency table. Default is TRUE.
#' @param time_unit Unit of follow-up time (can be "days", "weeks", "months", "years"). Default is "years".
#' @param status_var Name of the patient status variable that was previously created. Default is p_status.
#' @param lifedat_var Name of variable containing Date of Death. Will override dattype preset.
#' @param fcdat_var Name of variable containing Date of Primary Cancer diagnosis. Will override dattype preset.
#' @param spcdat_var Name of variable containing Date of SPC diagnosis Will override dattype preset.
#' @return wide_df
#' @export
#'
#'
#'

calc_futime_tt <- function(wide_df,
                           futime_var_new = "p_futimeyrs",
                           fu_end,
                           dattype = "zfkd",
                           check = TRUE,
                           time_unit = "years",
                           status_var = "p_status",
                           lifedat_var = NULL,
                           fcdat_var = NULL,
                           spcdat_var = NULL){
  
  #fetch variable names provided in function call
  futime_var_new <- rlang::ensym(futime_var_new)
  status_var <- rlang::ensym(status_var)
  time_unit <- rlang::enquo(time_unit)
  
  #setting default var names and values for SEER data
  
  if (dattype == "seer"){
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::sym("p_datedeath.1")
    } else{
      lifedat_var <- rlang::ensym(lifedat_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::sym("t_datediag.1")
    } else{
      fcdat_var <- rlang::ensym(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::sym("t_datediag.2")
    } else{
      spcdat_var <- rlang::ensym(spcdat_var)
    }
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::sym("SDIMP.1")
    } else{
      lifedat_var <- rlang::ensym(lifedat_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::sym("DDIMP.1")
    } else{
      fcdat_var <- rlang::ensym(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::sym("DDIMP.2")
    } else{
      spcdat_var <- rlang::ensym(spcdat_var)
    }
  }
  
  #---- Checks start
  
  #check whether p_dodmin information can be used
  if("p_dobmin" %in% names(wide_df)) {
    dmin <- TRUE
  } else{
    dmin <- FALSE
  }
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::as_name(status_var), rlang::as_name(lifedat_var), rlang::as_name(fcdat_var), rlang::as_name(spcdat_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  #get used FU date from function parameter and from label of status_var
  fu_end_param <- as.Date(rlang::as_name(fu_end), date.format = "%y-%m-%d")
  fu_end_quo <- rlang::enquo(fu_end_param)
  fu_end_status <- attr(wide_df[[rlang::as_name(status_var)]], "label", exact = T) %>% stringr::str_sub(-10) %>% as.Date(.,  date.format = "%y-%m-%d")
  
  
  #check whether date is provided in correct format
  
  if(lubridate::is.Date(fu_end_param) == FALSE) {
    rlang::abort("You have not provided a correct Follow-up date in the format YYYY-MM-DD")
  }
  
  #check whether time_unit is provided in correct format
  if((rlang::as_name(time_unit) %in% c("years", "months", "days")) == FALSE) {
    rlang::abort("You can only use 'years', 'months' or 'days' as time_unit")
  }
  
  #check whether FU date provided was the same as for pat_status function
  
  if(fu_end_status != fu_end_param) {
    rlang::abort(paste0("You are using a different date of FU for calculating FU time (this function) than you have for calculating patient status (pat_status function).",
                        "\nEnd of Follow-up provide: ", fu_end_param,
                        "\nEnd of FU used in p_status is: ", fu_end_status))
  }
  
  
  #check whether FU date provided might be too late
  if(fu_end_param > max(wide_df[[rlang::as_name(fcdat_var)]], na.rm = TRUE) & fu_end_param > max(wide_df[[rlang::as_name(spcdat_var)]], na.rm = TRUE)) {
    rlang::abort(paste0("You have provided an end of Follow-up date that might be out of range of the collected data.",
                        "Thus events such as SPCs or deaths might not have been recorded and FU-time is overestimated.",
                        "\nEnd of Follow-up provided: ", fu_end_param,
                        "\nLatest recorded First Cancer: ",  max(wide_df[[rlang::as_name(fcdat_var)]], na.rm = TRUE),
                        "\nLatest recorded Second Cancer: ",  max(wide_df[[rlang::as_name(spcdat_var)]], na.rm = TRUE)
    ))
  }
  
  #check if new and old futime_var are the same --> message that id was overwritten
  if(rlang::as_name(futime_var_new) %in% names(wide_df)){
    rlang::warn(paste0(rlang::as_name(futime_var_new)," is already present in dataset. Variable has been overwritten with new values."))
  }
  
  #---- Calculate
  
  #revert status_var to numeric if previously labelled
  if(is.factor(wide_df[[rlang::as_name(status_var)]])){
    changed_status_var <- TRUE
    wide_df <- wide_df %>%
      tidytable::mutate.(
        #copy old status var
        status_var_orig = !!status_var,
        #make status_var numeric
        !!status_var := sjlabelled::as_numeric(!!status_var,
                                               keep.labels=FALSE, use.labels = TRUE))
  } else{
    changed_status_var <- FALSE
  }
  
  #new variable label
  futime_var_new_label <- paste0("Follow-up time of patient from diagnosis of first cancer until SPC or Death or End of FU [years]. End of FU is ", fu_end_param)
  
  
  #not working from here
  #calculate new follow_up time p_futimeyrs
  wide_df <- wide_df %>%
    tidytable::mutate.(!!futime_var_new := tidytable::case.(
      #patient alive, after FC
      !!status_var == 1, lubridate::time_length(difftime(!!fu_end_quo, rlang::eval_tidy(!!fcdat_var)), !!time_unit),
      #patient alive, after SPC
      !!status_var == 2, lubridate::time_length(difftime(rlang::eval_tidy(!!spcdat_var), rlang::eval_tidy(!!fcdat_var)), !!time_unit),
      #patient dead, after FC
      !!status_var == 3, lubridate::time_length(difftime(rlang::eval_tidy(!!lifedat_var), rlang::eval_tidy(!!fcdat_var)), !!time_unit),
      #patient dead, after SPC
      !!status_var == 4, lubridate::time_length(difftime(rlang::eval_tidy(!!spcdat_var), rlang::eval_tidy(!!fcdat_var)), !!time_unit),
      # NA 97 - not born
      !!status_var == 97, NA_real_,
      # NA 98 - no FC
      !!status_var == 98, NA_real_,
      # NA 99 - DOD missing
      !!status_var == 99, NA_real_,
      default = NA_real_)) %>%
    #label new variable
    sjlabelled::var_labels(!!futime_var_new := !!futime_var_new_label)
  
  #if status_var was changed from factor to numeric, revert
  if(changed_status_var == TRUE){
    wide_df <- wide_df %>%
      tidytable::mutate.(
        #replace temporary lifedat_var values with values from old lifedat_var
        !!status_var := status_var_orig
      ) %>%
      #remove status_var_orig
      tidytable::select.(-status_var_orig)
  }
  
  #---- Checks end
  
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- wide_df %>%
      tidytable::summarise.(mean_futime = mean(!!futime_var_new, na.rm = TRUE),
                            min_futime = min(!!futime_var_new, na.rm = TRUE),
                            max_futime = max(!!futime_var_new, na.rm = TRUE),
                            median_futime = stats::median(!!futime_var_new, na.rm = TRUE),
                            .by = !!status_var)
    
    print(check_tab)
    
    
  }
  
  return(wide_df)
  
}

