
#' Reshape dataset to wide format using data.table
#'
#' @param df dataframe
#' @param case_id_var String with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param timevar_max Numeric; default 6. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted before reshaping.  
#' @param chunks Number of last digits taken from case_id_var as grouping variable. Increasing chunks will increase computational time,
#'                    but might save memory. Default is 0.
#' @param datsize Number of rows to be  take from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @return df
#' @export
#'

reshape_wide_dt <- function(df, case_id_var, time_id_var, timevar_max = 6, chunks = 0, datsize = Inf){
  
  #make df a data.table
  if(is.infinite(datsize) | nrow(df) <= datsize){
    data.table::setDT(df)
  } else{
    df <- data.table::setDT(df) %>%
      #select first rows 1:datsize
      .[1:datsize, ]
  }
  
  #number of patient IDs at start of function
  n_start <- df %>%
    .[,get(case_id_var)] %>%
    unique() %>%
    length()
  
  #determine maximum number of cases per patient and deleting all cases > timevar_max
  max_time <- df %>%
    .[, get(time_id_var)] %>%
    as.numeric() %>%
    max(., na.rm = TRUE)
  
  #create custom variables
  trans_vars <- names(df)[!names(df) %in% c(case_id_var, time_id_var)]
  
  if(max_time > timevar_max){
    warning(glue::glue('Long dataset had too many cases per patient. Wide dataset is limited to {timevar_max} cases per id.'))
    
    #creat new var counter that counts incidences per case
    #!!!Performance - at this step computation time might be saved
    df <- df %>%  
      .[, counter := seq_len(.N), by=get(case_id_var)] %>%
      .[counter <= timevar_max, ] %>% 
      .[, counter := NULL]
    
  }
  
  #if 0 chunks, then reshape whole dataset at once
  if(chunks == 0){
    wide_df <- data.table::dcast(df, formula = get(case_id_var) ~ get(time_id_var), #unquoting done with get() if arguments are provided as quotes
                                 sep = ".",
                                 fill = NA,
                                 value.var = trans_vars) %>%
      #dcase function automatically renames case_id_var which is reverted now
      data.table::setnames(., old = c("case_id_var"), new = case_id_var) %>%
      #results are sorted by case_id_var
      .[base::order(as.numeric(.[, get(case_id_var)]), method = "radix")]
  } 
  #if chunks > 0, then split up dataset, reshape and bind
  else{
    
    df <- df %>%
      #create new variable "group_ids" that takes the last n(chunks) digits from ID
      data.table::set(., j = "group_ids", value = as.numeric(.[, get(case_id_var)]) %% 10^chunks) %>%
      split(., by = c("group_ids"), flatten = FALSE)
    
    wide_df <- list()
    
    for(i in 1:length(df)){
      
      wide_df[[i]] <- df[[i]] %>%
        data.table::dcast(., formula = get(case_id_var) ~ get(time_id_var), #unquoting done with get() if arguments are provided as quotes
                          sep = ".",
                          fill = NA,
                          value.var = trans_vars)
      df[[i]] <- 0
    }
    
    
    wide_df <- wide_df %>%
      #bind all data.tables together
      data.table::rbindlist(., fill = TRUE) %>%
      #dcase function automatically renames case_id_var which is reverted now
      data.table::setnames(., old = c("case_id_var"), new = case_id_var) %>%
      #results are sorted by case_id_var
      .[base::order(as.numeric(.[, get(case_id_var)]), method = "radix")]
  }
  
  #check whether final number of patient IDs matches number at start.
  n_end <- wide_df %>% nrow()
  
  if(n_end != n_start){
    warning('Unique n in long and wide dataset do not match. Possibly dataset was split between cases of same ID. Multiple entries for same ID still need to be merged')
  }
  
  return(wide_df)
  
}
