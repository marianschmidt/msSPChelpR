
#' Reshape dataset to wide format - tidytable version
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
#' #prep step - reshape wide a sample of 10000 rows from us_second_cancer
#' usdata_wide_sample <- msSPChelpR::reshape_wide(us_second_cancer,
#'                          case_id_var = "fake_id", 
#'                          time_id_var = "SEQ_NUM", 
#'                          timevar_max = 2,
#'                          datsize = 10000)
#'
#' #now we can reshape long again
#' msSPChelpR::reshape_long_tt(usdata_wide_sample,
#'                          case_id_var = "fake_id", 
#'                          time_id_var = "SEQ_NUM")
#' 
#'


reshape_long_tt <- function(wide_df, case_id_var, time_id_var, datsize = Inf){
  
  case_id_var <- rlang::ensym(case_id_var)
  time_id_var <- rlang::ensym(time_id_var)
  
  # restrict size of data.frame to datsize number of rows
  if(nrow(wide_df) > datsize){
    wide_df <- wide_df[c(1:datsize), ]
  }
  
  #number of patient IDs at start of function
  n_start <- nrow(wide_df)
  
  #in list of variable names find variables that have a dot separator followed by digits in the end or NA in the end
  varying_vars <- colnames(wide_df) %>% stringr::str_subset(.,
                                                            paste0("\\.", "(?=[:digit:]$|(?=[:digit:](?=[:digit:]$))|(?=N(?=A$)))"))
  
  constant_vars <- colnames(wide_df)[!colnames(wide_df) %in% c(varying_vars)]
  
  
  ### perform tidytable::pivot_longer
  wide_df %>% tidytable::pivot_longer.(
    -c(tidyselect::all_of(constant_vars)),
    names_to = c(".value", rlang::as_name(time_id_var)),
    names_pattern = "(.*)\\.(.*)",
    values_drop_na = TRUE
  ) %>%  
    #make time_id_var numeric
    tidytable::mutate.(!!time_id_var := as.numeric(rlang::eval_tidy(!!time_id_var))) %>%
    #sort by case_id_var
    tidytable::arrange.(as.numeric(rlang::eval_tidy(!!case_id_var)), !!time_id_var)
  
}
