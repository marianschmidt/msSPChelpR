
#' Reshape dataset to wide format
#'
#' @param df dataframe
#' @param case_id_var String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param timevar_max Numeric; default 6. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted before reshaping.    
#' @param datsize Number of rows to be  take from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @return df
#' @export
#'

reshape_wide_tidyr <- function(df, case_id_var, time_id_var, timevar_max = 6, datsize = Inf){
  
  ### restrict size of data.frame to datsize number of rows
  if(nrow(df) > datsize){
    df <- df[c(1:datsize), ]
  }
  
  ### get names from df to provide to pivot function
  trans_vars <- names(df)[!names(df) %in% c({{case_id_var}}, {{time_id_var}})]
  
  
  ### determine maximum number of cases per patient and deleting all cases > timevar_max
  max_time <- df %>%
    dplyr::select(tidyselect::all_of(time_id_var)) %>%
    unlist %>%
    as.numeric %>%
    max(na.rm = TRUE)
  
  
  if(max_time > timevar_max){
    warning(glue::glue('Long dataset had too many cases per patient. Wide dataset is limited to {timevar_max} cases per id.'))
    
    df <- df %>%
      #sort by case_id and time_id_var
      dplyr::arrange(!!rlang::sym(case_id_var),!!rlang::sym(time_id_var))%>%
      #group by case_id_var
      dplyr::group_by(!!rlang::sym(case_id_var))%>%
      #calculate new renumbered variable
      dplyr::mutate(counter = dplyr::row_number()) %>%
      #filter based on new renumbered variable
      dplyr::filter(counter <= timevar_max) %>%
      dplyr::select(-counter) %>%
      #ungroup
      dplyr::ungroup()
    
  }
  
  ### perform tidyr::pivot_wider
  df %>% tidyr::pivot_wider(
    names_from = {{time_id_var}}, 
    values_from = tidyselect::all_of(trans_vars),
    names_sep = "."
  )
  
}