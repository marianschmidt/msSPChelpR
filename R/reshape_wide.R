
#' Reshape dataset to wide format
#'
#' @param df dataframe
#' @param case_id_var String with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param timevar_max Numeric; default 6. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted before reshaping.              
#' @param datsize Number of rows to be taken from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @param chunks Numeric; default 10. Technical parameter how the data is split during reshaping.
#' @return df
#' @export
#'


reshape_wide <- function(df, case_id_var, time_id_var, timevar_max = 6, datsize = Inf, chunks = 10){
  
  # restrict size of data.frame to datsize number of rows
  if(nrow(df) > datsize){
    df <- df[c(1:datsize), ]
  }
  
  #number of patient IDs at start of function
  n_start <- df %>% dplyr::select(tidyselect::all_of(case_id_var)) %>% dplyr::n_distinct()
  
  #determine maximum number of cases per patient and deleting all cases > timevar_max
  max_time <- df %>%
    dplyr::select(tidyselect::all_of(time_id_var)) %>%
    unlist %>%
    as.numeric %>%
    max(na.rm = TRUE)
  
  if(max_time > timevar_max){
    rlang::inform(paste("Long dataset had too many cases per patient. Wide dataset is limited to ", timevar_max," cases per id as defined in timevar_max option."))    
    
    df <- df %>%
      dplyr::mutate(counter = as.integer(!! rlang::sym(time_id_var))) %>%
      dplyr::filter(counter <= timevar_max) %>%
      dplyr::select(-counter)
    
  }
  
  # split dataset in equal chunks and store in list
  
  df <- split(df, as.numeric(as.factor(df[[case_id_var]])) %% chunks)
  
  
  #perform reshape command on each chunk
  wide_df <- list()
  
  for(i in 1:chunks){
    
    wide_df[[i]] <- df[[i]] %>%
      as.data.frame %>%
      stats::reshape(timevar=time_id_var, idvar=case_id_var, direction = "wide", sep=".")
    
    df[[i]] <- 0
    
  }
  
  #rbind chunks into one dataframe
  wide_df <- dplyr::bind_rows(wide_df) %>%
    #sort by case_id_var
    dplyr::arrange(as.numeric(rlang::eval_tidy(rlang::ensym(case_id_var))))
  
  #check whether final number of patient IDs matches number at start.
  n_end <- wide_df %>% nrow()
  
  if(n_end != n_start){
    warning('Unique n in long and wide dataset do not match. Possibly dataset was split between cases of same ID. Multiple entries for same ID still need to be merged')
  }
  
  return(wide_df)
  
}

