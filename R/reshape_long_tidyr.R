
#' Reshape dataset to wide format - tidyr version
#'
#' @param wide_df dataframe
#' @param case_id_var String with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param datsize Number of rows to be taken from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @return long_df
#' @export
#' @examples 
#' 
#' data(us_second_cancer)
#' 
#' #prep step - reshape wide
#' usdata_wide <- msSPChelpR::reshape_wide(us_second_cancer,
#'                          case_id_var = "fake_id", 
#'                          time_id_var = "SEQ_NUM", 
#'                          timevar_max = 2)
#'
#' #now we can reshape long again
#' msSPChelpR::reshape_long_tidyr(usdata_wide,
#'                          case_id_var = "fake_id", 
#'                          time_id_var = "SEQ_NUM")
#' 
#'

reshape_long_tidyr <- function(wide_df, case_id_var, time_id_var, datsize = Inf){
  
  case_id_var <- rlang::ensym(case_id_var)
  time_id_var <- rlang::ensym(time_id_var)
  # restrict size of data.frame to datsize number of rows
  if(nrow(wide_df) > datsize){
    wide_df <- wide_df[c(1:datsize), ]
  }
  
  #number of patient IDs at start of function
  n_start <- nrow(wide_df)
  
  #get data type of case_id_var
  class_case_id_start <- class(wide_df[[rlang::quo_text(case_id_var)]])
  
  
  #in list of variable names find variables that have a dot separator followed by digits in the end or NA in the end
  varying_vars <- colnames(wide_df) %>% stringr::str_subset(.,
                                                            paste0("\\.", "(?=[:digit:]$|(?=[:digit:](?=[:digit:]$))|(?=N(?=A$)))"))
  
  constant_vars <- colnames(wide_df)[!colnames(wide_df) %in% c(varying_vars)]
  
  ### perform tidyr::pivot_longer
  long_df <- wide_df %>% tidyr::pivot_longer(
    -c(tidyselect::all_of(constant_vars)),
    names_to = c(".value", rlang::quo_text(time_id_var)),
    names_pattern = "(.*)\\.(.*)",
    values_drop_na = TRUE
  ) %>%  
    #make time_id_var numeric
    dplyr::mutate(!!time_id_var := as.numeric(rlang::eval_tidy(!!time_id_var))) %>%
    #sort by case_id_var and time_id_var
    dplyr::arrange(as.numeric(.data[[!!case_id_var]]), .data[[!!time_id_var]])
  
  
  #---- Checks 
  
  #check whether final number of patient IDs matches number at start.
  n_end <- long_df %>% dplyr::select(!!case_id_var) %>% dplyr::n_distinct()
  
  if(n_end != n_start){
    rlang::abort('Unique n in long and wide dataset do not match. There may have been an error!')
  }
  
  #check whether final data type of case_id_var is the same as at start
  
  class_case_id_end <- class(long_df[[rlang::quo_text(case_id_var)]])
  
  if(class_case_id_end != class_case_id_start){
    rlang::inform(paste0("Data type of case_id_var has been changed to: ", class_case_id_end, 
                         ". Was ", class_case_id_start, " before."))
  }
  
  #---- Return results
  
  return(long_df)
  
}

