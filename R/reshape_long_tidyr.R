
#' Reshape dataset to long format
#'
#' @param df dataframe
#' @param case_id_var String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param var_selection Vector of variables to keep. Default is "_all".
#' @param drop_na Logical to indicate whether empty rows after pivoting should be dropped (activates option values_drop_na in tidyr::pivot_longer).
#'                Default is drop_na = FALSE.
#' @return df
#' @export
#' 

reshape_long_tidyr <- function(df, case_id_var, time_id_var, drop_na = FALSE, var_selection = c("_all")){
  
  df %>%
    tidyr::pivot_longer(
      -c({{case_id_var}}),
      names_to = c(".value", {{time_id_var}}),
      names_pattern = "(.*)\\.(.*)",
      values_drop_na = drop_na
    ) %>%
    #enable variable selection
    {if (all(var_selection != "_all")){dplyr::select(case_id_var, time_id_var, var_selection)} else {.}}
  
  }

