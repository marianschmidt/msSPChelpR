
#' Reshape dataset to long format
#'
#' @param wide_df dataframe
#' @param case_id_var String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param chunks Numeric; default 10.
#' @return long_df
#' @export
#'


reshape_long_reshape <- function(wide_df, case_id_var, time_id_var, chunks = 1,
                                 datsize = Inf){
  
  if(nrow(wide_df) > datsize){
    wide_df <- wide_df[c(1:datsize), ]
  }
  
  #number of patient IDs at start of function
  n_start <- nrow(wide_df)
  
  #in list of variable names find variables that have a dot separator followed by digits in the end or NA in the end
  varying_vars <- colnames(wide_df) %>% stringr::str_subset(.,
                                                            paste0("\\.", "(?=[:digit:]$|(?=[:digit:](?=[:digit:]$))|(?=N(?=A$)))"))
  
  
  #split dataset in equal chunks and store in list
  rows_pc <- (nrow(wide_df) / chunks) %>% round(0)
  
  wide_df <- split(wide_df, (as.numeric(rownames(wide_df))-1) %/% rows_pc)
  
  #perform reshape command on each chunk
  long_df <- list()
  
  for(i in 1:chunks){
    
    long_df[[i]] <- wide_df[[i]] %>%
      as.data.frame %>%
      stats::reshape(timevar=time_id_var, idvar=case_id_var, direction = "long", varying = varying_vars, sep=".")
    
    wide_df[[i]] <- 0
    
  }
  
  #rbind chunks into one dataframe
  long_df <- dplyr::bind_rows(long_df) %>% 
    dplyr::arrange(!! rlang::sym(case_id_var), !! rlang::sym(time_id_var))
  
  #filter empty rows
  col_subset <- colnames(long_df)[!colnames(long_df) %in% (c(case_id_var, time_id_var))]
  long_df <- long_df %>%
    dplyr::filter_at(.vars = dplyr::vars(tidyselect::one_of(col_subset)),
                     dplyr::any_vars(!is.na(.))) %>%
    #sorting
    dplyr::arrange(!! rlang::sym(case_id_var), !! rlang::sym(time_id_var))
  
  #check whether final number of patient IDs matches number at start.
  n_end <- long_df %>% dplyr::select(tidyselect::all_of(case_id_var)) %>% dplyr::n_distinct()
  
  if(n_end != n_start){
    warning('Unique n in long and wide dataset do not match. There may have been an error!')
  }
  
  return(long_df)
  
}
