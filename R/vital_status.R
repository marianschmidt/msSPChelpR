#' Calculate vital status at end of follow-up depending on pat_status
#'
#' @param wide_df dataframe in wide format
#' @param status_var Name of the patient status variable that was previously created. Default is p_status.  
#' @param life_var_new Name of the newly calculated variable for patient vital status. Default is p_alive.  
#' @param check Check newly calculated variable p_status by printing frequency table. Default is TRUE.   
#' @param as_labelled_factor If true, output life_var_new as labelled factor variable. Default is FALSE.
#' @return wide_df
#' @export
#'

vital_status <- function(wide_df, status_var = "p_status", life_var_new = "p_alive", check = TRUE, as_labelled_factor = FALSE){
  
  #check if wide_df is data.frame
  if(!is.data.frame(wide_df) | data.table::is.data.table(wide_df)){
    message("You are using a dplyr based function on a raw data.table; the data.table has been converted to a data.frame to let this function run more efficiently.")
    wide_df <- as.data.frame(wide_df)
  }
  
  status_var <- rlang::enquo(status_var)
  life_var_new <- rlang::enquo(life_var_new)
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::quo_name(status_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  #make label for new variable (use FU date from label of status_var)
  
  lifevar_label <- paste("Patient Vital Status at end of follow-up", attr(wide_df[[rlang::eval_tidy(status_var)]], "label", exact = T) %>%
                           stringr::str_sub(-10))
  
  #calculate new status_var variable and label it
  #todo: implement check on date of spc_diagnosis and date of birth and introduce new status.
  
  #revert status_var to numeric if previously labelled
  if(is.factor(wide_df[[rlang::eval_tidy(status_var)]])){
    wide_df <- wide_df %>%
      dplyr::mutate_at(dplyr::vars(!!status_var), sjlabelled::as_numeric, keep.labels=FALSE, use.labels = TRUE)
  }
  
  #create new life_var
  wide_df <- wide_df %>%
    dplyr::mutate(!!life_var_new := dplyr::case_when(
      #patient alive
      .data[[!!status_var]] == 1 ~ 10,
      .data[[!!status_var]] == 2 ~ 10,
      #patient dead
      .data[[!!status_var]] == 3 ~ 11,
      .data[[!!status_var]] == 4 ~ 11,
      #copy NA codes
      TRUE ~ .data[[!!status_var]])) %>%
    #label new variable 
    sjlabelled::var_labels(!!life_var_new := lifevar_label) %>%
    sjlabelled::val_labels(!!life_var_new := c("patient alive" = 10,
                                                    "patient dead" = 11,
                                                    "NA - patient not born before end of FU" = 97,
                                                    "NA - patient did not develop cancer before end of FU" = 98,
                                                    "NA - patient date of death is missing" = 99)) 
  
  #enforce option as_labelled_factor = TRUE
  if(as_labelled_factor == TRUE){
    wide_df <- wide_df %>%
      dplyr::mutate_at(dplyr::vars(!!life_var_new), sjlabelled::as_label, keep.labels=TRUE) 
  }
  
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- wide_df %>%
      dplyr::count(.data[[!!status_var]], .data[[!!life_var_new]])
    
    print(check_tab)
    
  }
  
  return(wide_df)
  
}
