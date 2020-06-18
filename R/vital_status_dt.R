
#' Calculate vital status at end of follow-up depending on pat_status - data.table version
#'
#' @param wide_dt wide data.table (data.frame can also be provided, but will be converted to data.table)
#' @param status_var Name of the patient status variable that was previously created. Default is p_status.  
#' @param life_var_new Name of the newly calculated variable for patient vital status. Default is p_alive.  
#' @param check Check newly calculated variable life_var_new by printing frequency table. Default is TRUE.   
#' @param as_labelled_factor If true, output life_var_new as labelled factor variable. Default is FALSE.
#' @return wide_dt
#' @export
#'

vital_status_dt <- function(wide_dt, status_var = "p_status", life_var_new = "p_alive", 
                            check = TRUE, as_labelled_factor = FALSE){
  
  #check if wide_dt is data.table
  if(!data.table::is.data.table(wide_dt)){
    rlang::inform("You are using a data.table based function on a different data object; data has been converted to a data.table.")
    wide_dt <- data.table::as.data.table(wide_dt)
  }
  
  #------ Checks start
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(status_var)
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_dt))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  #make label for new variable (use FU date from label of status_var)
  
  lifevar_label <- paste("Patient Vital Status at end of follow-up", attr(wide_dt[[status_var]], "label", exact = T) %>%
                           stringr::str_sub(-10))
  
  #---- Calculate
  
  #revert status_var to numeric if previously labelled
  if(is.factor(wide_dt[[status_var]])){
    changed_status_var <- TRUE
    wide_dt <- wide_dt %>%
      #copy old status_var
      .[, c("status_var_orig") := get(status_var)]  %>%
      #make status_var numeric
      .[, (status_var) :=  sjlabelled::as_numeric(wide_dt[[status_var]], keep.labels=FALSE, use.labels = TRUE)]
    
  } else{
    changed_status_var <- FALSE
  }
  
  
  #create new life_var
  wide_dt <- data.table::set(wide_dt, i=NULL, j=life_var_new, value = data.table::fcase(
    #patient alive
    wide_dt[[status_var]] == 1, 10,
    wide_dt[[status_var]] == 2, 10,
    #patient dead
    wide_dt[[status_var]] == 3, 11,
    wide_dt[[status_var]] == 4, 11,
    #copy NA codes - currently "default" in data.table::fcase does not support vectors; check back if this might be supported
    wide_dt[[status_var]] == 97, 97,
    wide_dt[[status_var]] == 98, 98,
    wide_dt[[status_var]] == 99, 99,
    default = NA_real_
  ))
  
  
  #if status_var was changed from factor to numeric, revert
  if(changed_status_var == TRUE){
    wide_dt <- wide_dt %>%
      #replace temporary status_var values with values from old lifedat_var
      .[, (status_var) := status_var_orig] %>%
      #remove status_var_orig
      .[, c("status_var_orig") := NULL]
    
  }
  
  
  #label new variable 
  sjlabelled::set_label(wide_dt[[life_var_new]]) <- lifevar_label
  
  #label new variable values
  wide_dt[[life_var_new]] <- sjlabelled::set_labels(wide_dt[[life_var_new]], labels =  c("patient alive" = 10,
                                                                                         "patient dead" = 11,
                                                                                         "NA - patient not born before end of FU" = 97,
                                                                                         "NA - patient did not develop cancer before end of FU" = 98,
                                                                                         "NA - patient date of death is missing" = 99),
                                                    force.labels = TRUE)
  
  
  #enforce option as_labelled_factor = TRUE
  if(as_labelled_factor == TRUE){
    wide_dt <- wide_dt %>%
      .[, (life_var_new) := sjlabelled::as_label(wide_dt[[life_var_new]], keep.labels=TRUE)]
  }
  
  #---- Checks end
  
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- wide_dt %>%
      .[, .N, by = c(status_var, life_var_new)]
    
    print(check_tab)
    
  }
  
  return(wide_dt)
  
}

