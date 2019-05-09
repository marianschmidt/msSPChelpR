
#' Reshape dataset to wide format
#'
#' @param df dataframe
#' @param timevar String with name of variable that indicates diagnosis per patient.
#'                E.g. \code{timevar="SEQ_NUM"} for SEER data.
#' @param idvar String or vector of strings with name of ID variable indicating same patient.
#'                E.g. \code{idvar="PUBCSNUM"} for SEER data.
#' @param chunks Numeric; default 10.              
#' @return df
#' @export
#' @importFrom utils str 
#'


reshape_wide <- function(df, timevar, idvar, chunks = 10){
  
  n_start <- df %>% dplyr::select(idvar) %>% dplyr::n_distinct()
  
  rows_pc <- (nrow(df) / chunks) %>% round(0)
  
  df <- split(df, (as.numeric(rownames(df))-1) %/% rows_pc)
  
  wide_df <- list()
  
  for(i in 1:chunks){
    
    wide_df[[i]] <- df[[i]] %>%
      as.data.frame %>%
      stats::reshape(timevar=timevar, idvar=idvar, direction = "wide", sep=".")
    
    df[[i]] <- 0
    
  }
  
  wide_df <- dplyr::bind_rows(wide_df) %>% dplyr::arrange(idvar, timevar)
  
  n_end <- wide_df %>% nrow()
  
  if(n_end != n_start){
    warning('Unique n in long and wide dataset do not match. Possibly dataset was split between cases of same patient.')
  }
  
  return(wide_df)
  
}
