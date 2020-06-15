
#' Reshape dataset to long format - stats::reshape version
#'
#' @param wide_df dataframe in wide format
#' @param case_id_var String with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param time_id_var String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param datsize Number of rows to be taken from df. This parameter is mainly for testing. Default is Inf so that df is fully processed.
#' @param chunks Numeric; default 1. Technical parameter how the data is split during reshaping.
#' @return long df
#' @export
#'


reshape_long <- function(wide_df, case_id_var, time_id_var,
                         datsize = Inf, chunks = 1){
  
  case_id_var <- rlang::ensym(case_id_var)
  time_id_var <- rlang::ensym(time_id_var)
  
  # restrict size of data.frame to datsize number of rows
  if(nrow(wide_df) > datsize){
    wide_df <- wide_df[c(1:datsize), ]
  }
  
  #number of patient IDs at start of function
  n_start <- nrow(wide_df)
  
  #get data type of case_id_var
  class_case_id_start <- class(wide_df[[rlang::quo_text(case_id_var)]])
  
  
  #in list of variable names find variables that have a dot separator followed by digits in the end or NA in the end
  varying_vars <- colnames(wide_df) %>% stringr::str_subset(.,
                                                            paste0("\\.", "(?=[:digit:]$|(?=[:digit:](?=[:digit:]$))|(?=N(?=A$)))"))
  
  
  #split dataset in equal chunks and store in list
  rows_pc <- (nrow(wide_df) / chunks) %>% round(0)
  
  wide_df <- split(wide_df, as.numeric(as.factor(wide_df[[rlang::quo_text(case_id_var)]])) %% chunks)
  
  #perform reshape command on each chunk
  long_df <- list()
  
  for(i in 1:chunks){
    
    long_df[[i]] <- wide_df[[i]] %>%
      as.data.frame %>%
      stats::reshape(timevar=rlang::quo_text(time_id_var), idvar=rlang::quo_text(case_id_var), direction = "long", varying = varying_vars, sep=".")
    
    wide_df[[i]] <- 0
    
  }
  
  #rbind chunks into one dataframe
  long_df <- dplyr::bind_rows(long_df) %>% 
    dplyr::arrange(as.numeric(.data[[!!case_id_var]]), .data[[!!time_id_var]])
  
  #filter empty rows
  col_subset <- colnames(long_df)[!colnames(long_df) %in% (c(rlang::quo_text(case_id_var), rlang::quo_text(time_id_var)))]
  long_df <- long_df %>%
    dplyr::filter_at(.vars = dplyr::vars(tidyselect::one_of(col_subset)),
                     dplyr::any_vars(!is.na(.))) %>%
    #sorting
    dplyr::arrange(as.numeric(.data[[!!case_id_var]]), .data[[!!time_id_var]])
  
  #---- Checks 
  
  #check whether final number of patient IDs matches number at start.
  n_end <- long_df %>% dplyr::select(!!case_id_var) %>% dplyr::n_distinct()
  
  if(n_end != n_start){
    rlang::abort('Unique n in long and wide dataset do not match. There may have been an error!')
  }
  
  #check whether final data type of case_id_var is the same as at start
  
  class_case_id_end <- class(long_df[[rlang::quo_text(case_id_var)]])
  
  if(class_case_id_end != class_case_id_start){
    rlang::inform(paste0("Data type of case_id_var has been changed to: ", class_case_id_end, 
                         ". Was ", class_case_id_start, " before."))
  }
  
  #---- Return results
  
  return(long_df)
  
}

