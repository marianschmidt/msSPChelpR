
#' Reshape dataset to long format - data.table version
#'
#' @param wide_dt wide data.table (data.frame can also be provided, but will be converted to data.table)
#' @param case_id_var String with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param datsize Number of rows to be taken from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @param chunks Numeric; default 0. Technical parameter how the data is split during reshaping.
#' @return long data.table
#' @export
#'


reshape_long_dt <- function(wide_dt, case_id_var, time_id_var, datsize = Inf, chunks = 0){
  
  #make df a data.table
  if(is.infinite(datsize) | nrow(wide_dt) <= datsize){
    wide_df <- wide_dt %>%
      data.table::as.data.table(.)
  } else{
    wide_df <- wide_dt %>%
      data.table::as.data.table(.) %>%
      #select first rows 1:datsize
      .[1:datsize, ]
  }
  
  #number of patient IDs at start of function
  n_start <- wide_df %>% nrow()
  
  #get data type of case_id_var
  class_case_id_start <- class(wide_df[[rlang::as_name(case_id_var)]])
  
  
  #in list of variable names find variables that have a dot separator followed by digits in the end or NA in the end
  varying_vars <- colnames(wide_df) %>% stringr::str_subset(.,
                                                            paste0("\\.", "(?=[:digit:]$|(?=[:digit:](?=[:digit:]$))|(?=N(?=A$)))"))
  
  long_df <- wide_df %>%
    #create new variable "group_ids" that takes the last n(chunks) digits from ID
    data.table::set(., j = "group_ids", value = as.numeric(.[, get(case_id_var)]) %% 10^chunks) %>%
    split(., by = c("group_ids")) %>%
    #perform reshape on all single data.tables
    lapply(., stats::reshape, timevar=time_id_var, idvar=case_id_var, direction = "long", varying = varying_vars, sep=".") %>%
    #bind all data.tables together
    data.table::rbindlist(., fill = TRUE) %>%
    #remove group_ids variable
    data.table::set(., j = "group_ids", value = NULL) %>%
    #results are sorted by case_id_var
    .[base::order(nchar(.[, get(case_id_var)]), .[, get(case_id_var)], .[, get(time_id_var)], method = "radix")] %>%
    #remove rows in long_df that are all NA for all variables
    .[rowSums(!is.na(.[, .SD, .SDcols = colnames(.)[!colnames(.) %in% (c(case_id_var, time_id_var, "group_ids"))]])) > 0,]
  
  
  
  #---- Checks 
  
  #check whether final number of patient IDs matches number at start.
  n_end <- long_df %>%
    .[,get(case_id_var)] %>%
    unique() %>%
    length()
  
  if(n_end != n_start){
    rlang::abort('Unique n in long and wide dataset do not match. There may have been an error!')
  }
  
  #check whether final data type of case_id_var is the same as at start
  
  class_case_id_end <- class(long_df[[rlang::as_name(case_id_var)]])
  
  if(class_case_id_end != class_case_id_start){
    rlang::inform(paste0("Data type of case_id_var has been changed to: ", class_case_id_end, 
                         ". Was ", class_case_id_start, " before."))
  }
  
  #---- Return results
  
  return(long_df)
  
}

