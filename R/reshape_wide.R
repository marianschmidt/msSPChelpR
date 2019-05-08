
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
  
  rows_pc <- round(nrow(df) / chunks,0)
  
  dfs <- split(df, (as.numeric(rownames(df))-1) %/% rows_pc)
  
  single_reshape <- function(x){
    x %>%
      as.data.frame() %>%
      stats::reshape (timevar=timevar, idvar=idvar, direction = "wide", sep=".")
  }
  
  wide_dfs <- lapply(dfs, single_reshape)
  
  dplyr::bind_rows(wide_dfs)
  
}