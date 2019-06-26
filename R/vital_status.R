#' Calculate vital status at end of follow-up depending on pat_status
#'
#' @param df dataframe in wide format
#' @param status_var Name of the patient status variable that was previously created. Default is p_status.  
#' @param life_var_new Name of the newly calculated variable for patient vital status. Default is p_alive.  
#' @param check Check newly calculated variable p_status by printing frequency table. Default is TRUE.   
#' @return df
#' @export
#'

vital_status <- function(df, status_var = "p_status", life_var_new = "p_alive", check = TRUE){
  
  status_var <- rlang::enquo(status_var)
  life_var_new <- rlang::enquo(life_var_new)
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::quo_name(status_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  #make label for new variable (use FU date from label of status_var)
  statvar_label <- paste("Patient Vital Status at end of follow-up", sjlabelled::get_label(.data[[!!status_var]]) %>% stringr::str_sub(-10))
  
  #calculate new status_var variable and label it
  #todo: implement check on date of spc_diagnosis and date of birth and introduce new status.
  df <- df %>%
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
    sjlabelled::var_labels(!!life_var_new := !!statvar_label) %>%
    sjlabelled::set_labels(!!life_var_new, labels = c("patient alive" = 1,
                                                    "patient dead" = 2,
                                                    "NA - patient not born before end of FU" = 97,
                                                    "NA - patient did not develop cancer before end of FU" = 98,
                                                    "NA - patient date of death is missing" = 99)) #%>%
  #dplyr::mutate_at(dplyr::vars(!!life_var_new), sjlabelled::as_label, keep.labels=TRUE) 
  
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- df %>%
      dplyr::count(.data[[!!status_var]], .data[[!!life_var_new]])
    
    print(check_tab)
    
  }
  
  return(df)
  
}
