#' Calculate follow-up time per case until end of follow-up depending on pat_status
#'
#' @param wide_df dataframe in wide format
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

calc_futime <- function(wide_df, 
                        futime_var_new = "p_futimeyrs",
                        fu_end,
                        dattype = "zfkd",
                        check = TRUE, 
                        time_unit = "years", 
                        status_var = "p_status", 
                        lifedat_var = NULL,
                        fcdat_var = NULL,
                        spcdat_var = NULL){
  
  #check if wide_df is data.frame
  if(!is.data.frame(wide_df) | data.table::is.data.table(wide_df)){
    message("You are using a dplyr based function on a raw data.table; the data.table has been converted to a data.frame to let this function run more efficiently.
            You can also run the data.table version of this function calc_futime_dt instead.")
    wide_df <- as.data.frame(wide_df)
  }
  
  futime_var_new <- rlang::enquo(futime_var_new)
  status_var <- rlang::enquo(status_var)
  
  #setting default var names and values for SEER data
  
  if (dattype == "seer"){
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::quo("p_dodeath")
    } else{
      lifedat_var <- rlang::enquo(lifedat_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::quo("p_dofirst")
    } else{
      fcdat_var <- rlang::enquo(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::quo("p_dospc")
    } else{
      spcdat_var <- rlang::enquo(spcdat_var)
    }
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::quo("SDIMP.1")
    } else{
      lifedat_var <- rlang::enquo(lifedat_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::quo("DDIMP.1")
    } else{
      fcdat_var <- rlang::enquo(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::quo("DDIMP.2")
    } else{
      spcdat_var <- rlang::enquo(spcdat_var)
    }
  }
  
  
  #check whether p_dodmin information can be used
  if("p_dobmin" %in% names(wide_df)) {
    dmin <- TRUE
  } else{
    dmin <- FALSE
  }
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::quo_name(status_var), rlang::quo_name(lifedat_var), rlang::quo_name(fcdat_var), rlang::quo_name(spcdat_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  #get used FU date from function parameter and from label of status_var
  fu_end <- rlang::enquo(fu_end)
  fu_end_param <- as.Date(rlang::eval_tidy(fu_end), date.format = "%y-%m-%d")
  fu_end_status <- attr(wide_df[[rlang::eval_tidy(status_var)]], "label", exact = T) %>% stringr::str_sub(-10) %>% as.Date(.,  date.format = "%y-%m-%d")
  
  
  #check whether date was provided in correct format
  
  if(!lubridate::is.Date(fu_end_param)) {
    rlang::abort("You have not provided a correct Follow-up date in the format YYYY-MM-DD")
  }
  
  
  #check whether FU date provided was the same as for pat_status function
  
  if(fu_end_status != fu_end_param) {
    rlang::abort(paste0("You are using a different date of FU for calculating FU time (this function) than you have for calculating patient status (pat_status function).",
                        "\nEnd of Follow-up provide: ", fu_end_param, 
                        "\nEnd of FU used in p_status is: ", fu_end_status))
  }
  
  
  #check whether FU date provided might be too late
  if(fu_end_param > max(wide_df[[rlang::quo_name(fcdat_var)]], na.rm = TRUE) & fu_end_param > max(wide_df[[rlang::quo_name(spcdat_var)]], na.rm = TRUE)) {
    rlang::abort(paste0("You have provided an end of Follow-up date that might be out of range of the collected data.",
                        "Thus events such as SPCs or deaths might not have been recorded and FU-time is overestimated.", 
                        "\nEnd of Follow-up provided: ", fu_end_param, 
                        "\nLatest recorded First Cancer: ",  max(wide_df[[rlang::quo_name(fcdat_var)]], na.rm = TRUE),
                        "\nLatest recorded Second Cancer: ",  max(wide_df[[rlang::quo_name(spcdat_var)]], na.rm = TRUE)
    ))
  }
  
  #CHK2: if new and old futime_var are the same --> message that id was overwritten
  
  if(rlang::quo_name(futime_var_new) %in% names(wide_df)){
    warning(paste0(rlang::quo_name(futime_var_new),"is already present in dataset. Variable has been overwritten with new FU time values"))
  }
  
  
  
  #revert status_var to numeric if previously labeled
  if(is.factor(wide_df[[rlang::eval_tidy(status_var)]])){
    wide_df <- wide_df %>%
      dplyr::mutate_at(dplyr::vars(!!status_var), sjlabelled::as_numeric, keep.labels=FALSE, use.labels = TRUE)
  }
  
  #new variable label
  futime_var_new_label <- paste0("Follow-up time of patient from diagnosis of first cancer until SPC or Death or End of FU [years]. End of FU is ", fu_end_param)
  
  #calculate new follow_up time p_futimeyrs
  wide_df <- wide_df %>%
    dplyr::mutate(!!futime_var_new := dplyr::case_when(
      #patient alive, after FC
      .data[[!!status_var]] == 1 ~ lubridate::time_length(difftime(fu_end_param, .data[[!!fcdat_var]]), "years"),
      #patient alive, after SPC
      .data[[!!status_var]] == 2 ~ lubridate::time_length(difftime(.data[[!!spcdat_var]], .data[[!!fcdat_var]]), "years"),
      #patient dead, after FC
      .data[[!!status_var]] == 3 ~ lubridate::time_length(difftime(.data[[!!lifedat_var]], .data[[!!fcdat_var]]), "years"),
      #patient dead, after SPC
      .data[[!!status_var]] == 4 ~ lubridate::time_length(difftime(.data[[!!spcdat_var]], .data[[!!fcdat_var]]), "years"),
      # NA 97 - not born
      .data[[!!status_var]] == 97 ~ NA_real_,
      # NA 98 - no FC
      .data[[!!status_var]] == 98 ~ NA_real_,
      # NA 99 - DOD missing
      .data[[!!status_var]] == 99 ~ NA_real_,
      TRUE ~NA_real_)) %>%
    #label new variable
    sjlabelled::var_labels(!!futime_var_new := !!futime_var_new_label)
  
  ##-- continue here
  
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- wide_df %>%
      dplyr::group_by(!!status_var)%>%
      dplyr::summarise(mean_futime = mean(.data[[!!futime_var_new]], na.rm = TRUE), 
                       min_futime = min(.data[[!!futime_var_new]], na.rm = TRUE),
                       max_futime = max(.data[[!!futime_var_new]], na.rm = TRUE),
                       median_futime = stats::median(.data[[!!futime_var_new]], na.rm = TRUE))
    
    print(check_tab)
    
    
  }
  
  return(wide_df)
  
}


