
#' Reshape dataset to long format
#'
#' @param df dataframe
#' @param case_id_var String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param time_id_num Logical that indicates whether time_id_var should be transformed to numeric or not.
#'                Default is \code{time_id_num=TRUE}.
#' @param constant_vars Vector of variables that are constant for all times per case_id, so these variables will just be filled during pivot_longer
#'                      Default is NULL.
#' @param drop_na Logical to indicate whether empty rows after pivoting should be dropped (activates option values_drop_na in tidyr::pivot_longer).
#'                Default is drop_na = FALSE.
#' @param var_selection Vector of variables to keep after pivoting. Default is "_all".
#' @param names_pattern A regular expression containing matching groups (()). Default is names_pattern = "(.*)\\.(.*)" where variable name and time_id are separated by a dot ".".
#' @return df
#' @export
#' 

reshape_long_tidyr <- function(df, case_id_var, time_id_var, time_id_num = TRUE, constant_vars = NULL, drop_na = FALSE, var_selection = c("_all"), names_pattern = "(.*)\\.(.*)"){
  
  df %>%
    tidyr::pivot_longer(
      -c({{case_id_var}}, {{constant_vars}}),
      names_to = c(".value", {{time_id_var}}),
      names_pattern = names_pattern,
      values_drop_na = drop_na
    ) %>%
    #enable variable selection
    {if (all(var_selection != "_all")){dplyr::select(case_id_var, time_id_var, var_selection)} else {.}} %>% 
    #make time_id_var numeric if 
    {if (time_id_num == TRUE){dplyr::mutate_at({{time_id_var}}, as.numeric)} else {.}}
  
  }

