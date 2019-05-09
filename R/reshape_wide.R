
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
  
  rows_pc <- (nrow(df) / chunks) %>% round(0)
  
  df <- split(df, (as.numeric(rownames(df))-1) %/% rows_pc)
  
  wide_df <- list()
  
  for(i in 1:chunks){
    
    wide_df[i] <- stats::reshape(df[[i]], timevar=timevar, idvar=idvar, direction = "wide", sep=".") %>% list
    
    df[i] <- 0
    
  }
  
  dplyr::bind_rows(wide_df)
  
}