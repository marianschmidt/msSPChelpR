
#' Calculate vital status at end of follow-up depending on pat_status - tidytable version
#'
#' @param wide_df dataframe or data.table in wide format
#' @param status_var Name of the patient status variable that was previously created. Default is p_status.  
#' @param life_var_new Name of the newly calculated variable for patient vital status. Default is p_alive.  
#' @param check Check newly calculated variable life_var_new by printing frequency table. Default is TRUE.   
#' @param as_labelled_factor If true, output life_var_new as labelled factor variable. Default is FALSE.
#' @return wide_df
#' @export
#'

vital_status_tt <- function(wide_df, status_var = "p_status", life_var_new = "p_alive", check = TRUE, 
                            as_labelled_factor = FALSE){
  
  #fetch variable names provided in function call
  status_var <- rlang::ensym(status_var)
  life_var_new <- rlang::ensym(life_var_new)
  
  #----- Checks
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::as_name(status_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  
  #make label for new variable (use FU date from label of status_var)
  
  lifevar_label <- paste("Patient Vital Status at end of follow-up", attr(wide_df[[rlang::as_name(status_var)]], "label", exact = T) %>%
                           stringr::str_sub(-10))
  
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
  
  #create new life_var
  wide_df <- wide_df %>%
    tidytable::mutate.(!!life_var_new := tidytable::case.(
      #patient alive
      !!status_var == 1, 10,
      !!status_var == 2, 10,
      #patient dead
      !!status_var == 3, 11,
      !!status_var == 4, 11,
      #copy NA codes
      #status_var needs to be converted to double to avoid error message (status_var is in some cases changed to integer during function)
      default = as.numeric(rlang::eval_tidy(!!status_var))))
  
  
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
  
  wide_df <- wide_df%>%
    #label new variable 
    sjlabelled::var_labels(!!life_var_new := !!lifevar_label) %>%
    sjlabelled::val_labels(!!life_var_new := c("Patient alive" = 10,
                                               "Patient dead" = 11,
                                               "NA - patient not born before end of FU" = 97,
                                               "NA - patient did not develop cancer before end of FU" = 98,
                                               "NA - patient date of death is missing" = 99),
                           force.labels = TRUE) 
  
  #enforce option as_labelled_factor = TRUE
  if(as_labelled_factor == TRUE){
    wide_df <- wide_df %>%
      tidytable::mutate.(!!life_var_new := sjlabelled::as_label(!!life_var_new, keep.labels=TRUE))
  }
  
  #---- Checks end
  
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- wide_df %>%
      dplyr::count(.data[[!!status_var]], .data[[!!life_var_new]])
    
    print(check_tab)
    
  }
  
  return(wide_df)
  
}

