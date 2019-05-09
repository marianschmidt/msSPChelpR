
#' Reshape dataset to wide format
#'
#' @param df dataframe
#' @param idvar String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param timevar String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param chunks Numeric; default 10.
#' @param timevar_max Numeric; default 6. Maximum number of cases per id. 
#'                    All tumors > timevar_max will be deleted before reshaping.              
#' @return df
#' @export
#' @importFrom utils str 
#' @importFrom rlang .data
#'


reshape_wide <- function(df, idvar, timevar, chunks = 10, timevar_max = 6){
  
  #number of patient IDs at start of function
  n_start <- df %>% dplyr::select(idvar) %>% dplyr::n_distinct()
  
  #determine maximum number of cases per patient and deleting all cases > timevar_max
  max_time <- df %>%
    dplyr::select(timevar) %>%
    unlist %>%
    as.numeric %>%
    max(na.rm = TRUE)
  
  if(max_time > timevar_max){
    warning('Long dataset had too many cases per patient. Wide dataset is limited to timevar_max cases per id.')
    
    df <- df %>%
      dplyr::mutate(counter = as.numeric(!! rlang::sym(timevar))) %>%
      dplyr::filter(counter <= timevar_max) %>%
      dplyr::select(-counter)
    
    }
  
  #split dataset in equal chunks and store in list
  rows_pc <- (nrow(df) / chunks) %>% round(0)
  
  df <- split(df, (as.numeric(rownames(df))-1) %/% rows_pc)
  
  
  #perform reshape command on each chunk
  wide_df <- list()
  
  for(i in 1:chunks){
    
    wide_df[[i]] <- df[[i]] %>%
      as.data.frame %>%
      stats::reshape(timevar=timevar, idvar=idvar, direction = "wide", sep=".")
    
    df[[i]] <- 0
    
  }
  
  #rbind chunks into one dataframe
  wide_df <- dplyr::bind_rows(wide_df) %>% 
    dplyr::arrange(!! rlang::sym(idvar))
  
  #check whether final number of patient IDs matches number at start.
  n_end <- wide_df %>% nrow()
  
  if(n_end != n_start){
    warning('Unique n in long and wide dataset do not match. Possibly dataset was split between cases of same ID. Multiple entries for same ID still need to be merged')
  }
  
  return(wide_df)
  
}
