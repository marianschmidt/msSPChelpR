
#' Renumber the time ID per case (i.e. Tumor sequence) - Tidytable variant
#'
#' @param df dataframe
#' @param new_time_id_var Name of the newly calculated variable for time_id. Required. 
#' @param dattype Type of cancer registry data. Can be "seer" or "zfkd". Default is "zfkd".
#' @param case_id_var String with name of ID variable indicating same patient.
#'                E.g. \code{case_id_var="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{time_id_var="SEQ_NUM"} for SEER data.
#' @param diagdat_var String with name of variable that indicates date of diagnosis per event. 
#'                E.g. \code{diagdat_var="t_datediag"} for SEER data.
#' @param timevar_max Numeric; default Inf. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted. 
#' @return df
#' @export
#'

renumber_time_id_tt <- function(df, new_time_id_var, dattype = "zfkd", 
                                case_id_var = NULL, time_id_var = NULL, diagdat_var = NULL, timevar_max = Inf){
  
  #----- Setting basic parameters
  
  
  new_time_id_var <- rlang::ensym(new_time_id_var)
  timevar_max <- rlang::enquo(timevar_max)
  
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
    if(is.null(diagdat_var)){
      diagdat_var <- rlang::sym("t_datediag")
    } else{
      diagdat_var <- rlang::ensym(diagdat_var)
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
    if(is.null(diagdat_var)){
      diagdat_var <- rlang::sym("DDIMP")
    } else{
      diagdat_var <- rlang::ensym(diagdat_var)
    }
  }
  
  #------ Checks 
  
  #CHK1: check whether all required variables are defined and present in dataset 
  #(check for time_id_var is also check for long dataset format)
  defined_vars <- c(rlang::quo_name(case_id_var), rlang::quo_name(time_id_var), rlang::quo_name(diagdat_var))
  
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
    #sort by case_id, diagdat_var and time_id_var
    tidytable::arrange.({{case_id_var}}, {{diagdat_var}}, {{time_id_var}})%>%
    #calculate new renumbered variable #group by case_id_var
    tidytable::mutate.({{new_time_id_var}} := as.integer(tidytable::row_number.()), by = {{case_id_var}}) %>%
    #delete all rows where new_time_id_var > timevar_max
    tidytable::filter.({{new_time_id_var}} <= {{timevar_max}}) %>%
    #sort by case_id_var and new_time_id_var
    tidytable::arrange.(as.numeric({{case_id_var}}), {{new_time_id_var}})
  
}

