
#' Reshape dataset to long format
#'
#' @param wide_df wide dataframe
#' @param case_id_var String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param chunks Number of last digits taken from case_id_var as grouping variable. Increasing chunks will increase computational time,
#'                    but will save memory. Default is 0.
#' @param datsize Number of rows to be  take from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @return df
#' @export
#'


reshape_long_dt <- function(wide_df, case_id_var, time_id_var, chunks = 0, datsize = Inf){
  
  #make df a data.table
  if(is.infinite(datsize) | nrow(wide_df) <= datsize){
    data.table::setDT(wide_df)
  } else{
    wide_df <- data.table::setDT(wide_df) %>%
      #select first rows 1:datsize
      .[1:datsize, ]
  }
  
  #number of patient IDs at start of function
  n_start <- wide_df %>% nrow()
  
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
  
  #check whether final number of patient IDs matches number at start.
  n_end <- long_df %>%
    .[,get(case_id_var)] %>%
    unique() %>%
    length()
  
  if(n_end != n_start){
    warning('Unique n in long and wide dataset do not match. Possibly dataset was split between cases of same ID. Multiple entries for same ID still need to be merged')
  }
  
  return(long_df)
  
}
