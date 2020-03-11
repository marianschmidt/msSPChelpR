
#' Renumber the time ID per case (i.e. Tumor sequence) using data.table
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
#'                    All tumors > timevar_max will be deleted before renumbering  
#' @return df
#' @export
#'

renumber_time_id_dt <- function(df, new_time_id_var, dattype = "zfkd", 
                             case_id_var = NULL, time_id_var = NULL, diagdat_var = NULL, timevar_max = Inf){
  
  #----- Setting basic parameters
  
  #setting default var names and values for SEER data
  
  if (dattype == "seer"){
    if(is.null(case_id_var)){
      case_id_var <- "PUBCSNUM"
    }
    if(is.null(time_id_var)){
      time_id_var <- "SEQ_NUM"
    }
    if(is.null(diagdat_var)){
      diagdat_var <- "t_datediag"
    }
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(case_id_var)){
      case_id_var <- "PSEUDOPATID"
    }
    if(is.null(time_id_var)){
      time_id_var <- "TUMID3"
    }
    if(is.null(diagdat_var)){
      diagdat_var <- "DDIMP"
    }
  }
  
  #------ Checks 
  
  #CHK1: check whether all required variables are defined and present in dataset 
  #(check for time_id_var is also check for long dataset format)
  defined_vars <- c(case_id_var, time_id_var, diagdat_var)
  
  not_found <- defined_vars[!(defined_vars %in% colnames(df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", paste(not_found, collapse=", ")))
  }
  
  #CHK2: ifs new and old id_var the same --> message that id was overwritten
  
  if(time_id_var == new_time_id_var){
    warning(paste0("Original time_id_var: ", time_id_var," has been overwritten with new renumbered values"))
  }
  
  #----- DM 
  
  df %>%
    data.table::setDT(.) %>%
    #sort by case_id and time_id_var
    .[base::order(nchar(.[, get(case_id_var)]), .[, get(case_id_var)], .[, get(diagdat_var)], .[, get(time_id_var)], method = "radix")] %>%
    #calculate new renumbered variable
    .[, (new_time_id_var) := seq_len(.N), by=get(case_id_var)] %>%
    #filter all time_ids per case that exceed timevar_max
    .[get(new_time_id_var) <= timevar_max, ]
  
}
