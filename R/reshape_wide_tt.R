
#' Reshape dataset to wide format - tidytable version
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

reshape_wide_tt <- function(df, case_id_var, time_id_var, timevar_max = 6, datsize = Inf){
  
  timevar_max <- rlang::enquo(timevar_max)
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
    tidytable::select.(tidyselect::all_of(time_id_var)) %>%
    unlist %>%
    as.numeric %>%
    max(na.rm = TRUE)
  
  if(max_time > rlang::eval_tidy(timevar_max)){
    rlang::inform(paste("Long dataset had too many cases per patient. Wide dataset is limited to ", rlang::eval_tidy(timevar_max)," cases per id as defined in timevar_max option."))
    
    df <- df %>%
      #sort by case_id and time_id_var
      tidytable::arrange.(!!case_id_var, !!time_id_var) %>%
      #calculate new renumbered variable group by case_id_var
      tidytable::mutate.(counter = as.integer(tidytable::row_number.()), .by = !!case_id_var) %>%
      #filter based on new renumbered variable
      tidytable::filter.(counter <= !!timevar_max) %>% 
      tidytable::select.(-counter)
    
  }
  
  ### perform pivot_wider
  df %>% tidytable::pivot_wider.(
    names_from = {{time_id_var}}, 
    values_from = tidyselect::all_of(trans_vars),
    names_sep = "."
  ) %>%
    #sort by case_id_var
    tidytable::arrange.(as.numeric(rlang::eval_tidy(!!case_id_var)))
  
}

