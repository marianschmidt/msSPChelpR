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

vital_status_dt <- function(wide_df, status_var = "p_status", life_var_new = "p_alive", check = TRUE, as_labelled_factor = FALSE){
  
  #setDT
  data.table::setDT(wide_df)
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(status_var)
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  #make label for new variable (use FU date from label of status_var)
  
  lifevar_label <- paste("Patient Vital Status at end of follow-up", attr(wide_df[[status_var]], "label", exact = T) %>%
                           stringr::str_sub(-10))
  
  #calculate new status_var variable and label it
  #todo: implement check on date of spc_diagnosis and date of birth and introduce new status.
  
  #revert status_var to numeric if previously labelled
  if(is.factor(wide_df[, get(status_var)])){
    wide_df %>%
      .[, (status_var) := sjlabelled::as_numeric(.[, get(status_var)], keep.labels=FALSE, use.labels = TRUE)]
  }
  
  #create new life_var
  wide_df <- wide_df %>%
    .[, (status_var) := data.table::fcase(
      #patient alive
      get(status_var) == 1, 10L,
      get(status_var) == 2, 10L,
      #patient dead
      get(status_var) == 3, 11L,
      get(status_var) == 4, 11L,
      #copy NA codes
      default = get(status_var)
    )] 
  
  #label new variable 
  sjlabelled::set_label(wide_df[[life_var_new]]) <- lifevar_label
  
  #label new variable values
  wide_df[[life_var_new]] <- sjlabelled::set_labels(wide_df[[life_var_new]], labels =  c("patient alive" = 10,
                                                                                     "patient dead" = 11,
                                                                                     "NA - patient not born before end of FU" = 97,
                                                                                     "NA - patient did not develop cancer before end of FU" = 98,
                                                                                     "NA - patient date of death is missing" = 99),
                                                  force.labels = TRUE)

  
  #enforce option as_labelled_factor = TRUE
  if(as_labelled_factor == TRUE){
    wide_df <- wide_df %>%
      .[, (life_var_new) := sjlabelled::as_label(.[, get(life_var_new)], keep.labels=TRUE)]
  }
 
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- wide_df %>%
      .[, .N, by = c(status_var, life_var_new)]
    
    print(check_tab)
    
  }
  
  return(wide_df)
  
}
