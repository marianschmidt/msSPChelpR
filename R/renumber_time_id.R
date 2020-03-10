
#' Renumber the time ID per case (i.e. Tumor sequence)
#'
#' @param df dataframe
#' @param new_time_id_var Name of the newly calculated variable for time_id. Required. 
#' @param dattype Type of cancer registry data. Can be "seer" or "zfkd". Default is "zfkd".
#' @param case_id_var String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param timevar_max Numeric; default Inf. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted before renumbering  
#' @return df
#' @export
#'

renumber_time_id <- function(df, new_time_id_var, dattype = "zfkd", 
                             case_id_var = NULL, time_id_var = NULL, timevar_max = Inf){
  
  #----- Setting basic parameters
  
  
  new_time_id_var <- rlang::enquo(new_time_id_var)
  
  #setting default var names and values for SEER data
  
  if (dattype == "seer"){
    if(is.null(case_id_var)){
      case_id_var <- rlang::sym("PUBCSNUM")
    } else{
      case_id_var <- rlang::ensym(case_id_var)
    }
    if(is.null(time_id_var)){
      time_id_var <- rlang::sym("SEQ_NUM")
    } else{
      time_id_var <- rlang::ensym(time_id_var)
    }
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(case_id_var)){
      case_id_var <- rlang::sym("PSEUDOPATID")
    } else{
      case_id_var <- rlang::ensym(case_id_var)
    }
    if(is.null(time_id_var)){
      time_id_var <- rlang::sym("TUMID3")
    } else{
      time_id_var <- rlang::ensym(time_id_var)
    }
  }
  
  #------ Checks 
  
  #CHK1: check whether all required variables are defined and present in dataset 
  #(check for time_id_var is also check for long dataset format)
  defined_vars <- c(rlang::quo_name(case_id_var), rlang::quo_name(time_id_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", paste(not_found, collapse=", ")))
  }
  
  #CHK2: ifs new and old id_var the same --> message that id was overwritten
  
  if(rlang::quo_name(time_id_var) == rlang::quo_name(new_time_id_var)){
    warning(paste0("Original time_id_var: ", rlang::quo_name(time_id_var)," has been overwritten with new renumbered values"))
  }
  
  #----- DM 
  
  df %>%
    #sort by case_id and time_id_var
    dplyr::arrange(!!case_id_var, !!time_id_var)%>%
    #group by case_id_var
    dplyr::group_by(!!case_id_var)%>%
    #calculate new renumbered variable
    dplyr::mutate(!!new_time_id_var := dplyr::row_number()) %>%
    #ungroup
    dplyr::ungroup() %>%
    #delete all rows where new_time_id_var > timevar_max
    dplyr::filter(.data[[!!new_time_id_var]] <= timevar_max)
  
}
