
#' Reshape dataset to wide format
#'
#' @param df dataframe
#' @param case_id_var String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param timevar_max Numeric; default 6. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted before reshaping.    
#' @param var_selection Vector of variables to keep. Default is "_all".          
#' @return df
#'

reshape_wide_tidyr <- function(df, case_id_var, time_id_var, timevar_max = 6, var_selection = c("_all")){
  
  ### get names from df to provide to pivot function
  trans_vars <- names(df)[!names(df) %in% c({{case_id_var}}, {{time_id_var}})]
  
  ### limit variables on option var_selection
  if(all(var_selection != "_all")) {
    
    #give warning in case variables defined in var_selection do not exist
    not_found <- var_selection[!(var_selection %in% trans_vars)]
    
    if(length(not_found) > 0) {
      warning("The following variables defined in var_selection are not found in the formats file: ", not_found)
    }
    
    trans_vars <- trans_vars[trans_vars %in% var_selection]
    
  }
  
  ### determine maximum number of cases per patient and deleting all cases > timevar_max
  max_time <- df %>%
    dplyr::select(time_id_var) %>%
    unlist %>%
    as.numeric %>%
    max(na.rm = TRUE)
  
  if(max_time > timevar_max){
    warning(glue::glue('Long dataset had too many cases per patient. Wide dataset is limited to {timevar_max} cases per id.'))
    
    df <- df %>%
      dplyr::mutate(counter = as.numeric(.data[[!!time_id_var]])) %>%
      dplyr::filter(counter <= timevar_max) %>%
      dplyr::select(-counter)
    
  }
  
  ### perform tidyr::pivot_wider
  df %>% tidyr::pivot_wider(
    names_from = {{time_id_var}}, 
    values_from = trans_vars,
    names_sep = "."
  )

}
