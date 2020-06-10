
#' Reshape dataset to wide format - tidyr version
#'
#' @param df dataframe
#' @param case_id_var String with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param timevar_max Numeric; default 6. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted before reshaping.              
#' @param datsize Number of rows to be taken from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @return df
#' @export
#'

reshape_wide_tidyr <- function(df, case_id_var, time_id_var, timevar_max = 6, datsize = Inf){
  
  case_id_var <- rlang::ensym(case_id_var)
  time_id_var <- rlang::ensym(time_id_var)
  
  ### restrict size of data.frame to datsize number of rows
  if(nrow(df) > datsize){
    df <- df[c(1:datsize), ]
  }
  
  ### get names from df to provide to pivot function
  trans_vars <- names(df)[!names(df) %in% c(rlang::quo_text(case_id_var), rlang::quo_text(time_id_var))]
  
  
  ### determine maximum number of cases per patient and deleting all cases > timevar_max
  max_time <- df %>%
    dplyr::select(tidyselect::all_of(time_id_var)) %>%
    unlist %>%
    as.numeric %>%
    max(na.rm = TRUE)
  
  
  if(max_time > timevar_max){
    rlang::inform(paste("Long dataset had too many cases per patient. Wide dataset is limited to ", timevar_max," cases per id as defined in timevar_max option."))    
    
    df <- df %>%
      #sort by case_id and time_id_var
      dplyr::arrange(!!case_id_var, !!time_id_var) %>%
      #group by case_id_var
      dplyr::group_by(!!case_id_var) %>%
      #calculate new renumbered variable
      dplyr::mutate(counter = as.integer(dplyr::row_number())) %>%
      #ungroup
      dplyr::ungroup() %>%
      #delete all rows where counter > timevar_max
      dplyr::filter(counter <= timevar_max) %>%
      dplyr::select(-counter)
    
    
  }
  
  ### perform tidyr::pivot_wider
  df %>% tidyr::pivot_wider(
    names_from = {{time_id_var}}, 
    values_from = tidyselect::all_of(trans_vars),
    names_sep = "."
  ) %>%  
    #sort by case_id_var
    dplyr::arrange(as.numeric(rlang::eval_tidy(rlang::ensym(case_id_var))))
  
  
}

