
#' Calculate vital status at end of follow-up depending on pat_status - tidyverse version
#'
#' @param wide_df dataframe in wide format
#' @param status_var Name of the patient status variable that was previously created. Default is p_status.  
#' @param life_var_new Name of the newly calculated variable for patient vital status. Default is p_alive.  
#' @param check Check newly calculated variable life_var_new by printing frequency table. Default is TRUE.   
#' @param as_labelled_factor If true, output life_var_new as labelled factor variable. Default is FALSE.
#' @return wide_df
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
#' msSPChelpR::vital_status(usdata_wide, 
#'                         status_var = "p_status",
#'                         life_var_new = "p_alive_new", 
#'                         check = TRUE, 
#'                         as_labelled_factor = FALSE)
#'


vital_status <- function(wide_df, status_var = "p_status", life_var_new = "p_alive", check = TRUE, 
                         as_labelled_factor = FALSE){
  
  #check if wide_df is data.frame
  if(!is.data.frame(wide_df) & data.table::is.data.table(wide_df)){
    rlang::inform("You are using a dplyr based function on a raw data.table; the data.table has been converted to a data.frame to let this function run more efficiently.")
    wide_df <- as.data.frame(wide_df)
  }
  
  #fetch variable names provided in function call
  status_var <- rlang::enquo(status_var)
  life_var_new <- rlang::enquo(life_var_new)
  
  #----- Checks
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::as_name(status_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found, ". Please run pat_status function beforehand."))
  }
  
  
  #make label for new variable (use FU date from label of status_var)
  
  lifevar_label <- paste("Patient Vital Status at end of follow-up", attr(wide_df[[rlang::eval_tidy(status_var)]], "label", exact = T) %>%
                           stringr::str_sub(-10))
  
  #---- Calculate
  
  #revert status_var to numeric if previously labelled
  if(is.factor(wide_df[[rlang::eval_tidy(status_var)]])){
    changed_status_var <- TRUE
    wide_df <- wide_df %>%
      dplyr::mutate(
        #copy old status var
        status_var_orig = .data[[!!status_var]],
        #make status_var numeric
        !!status_var := sjlabelled::as_numeric(.data[[!!status_var]], 
                                               keep.labels=FALSE, use.labels = TRUE))
    
  } else{
    changed_status_var <- FALSE
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
      #status_var needs to be converted to double to avoid error message (status_var is in some cases changed to integer during function)
      TRUE ~ as.numeric(.data[[!!status_var]])))
  
  #if status_var was changed from factor to numeric, revert
  if(changed_status_var == TRUE){
    wide_df <- wide_df %>%
      dplyr::mutate(
        #replace temporary lifedat_var values with values from old lifedat_var
        !!status_var := .data$status_var_orig
      ) %>%
      #remove status_var_orig
      dplyr::select(-status_var_orig)
  }
  
  wide_df <- wide_df %>%
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
      dplyr::mutate(!!life_var_new := sjlabelled::as_label(.data[[!!life_var_new]], keep.labels=TRUE))
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

