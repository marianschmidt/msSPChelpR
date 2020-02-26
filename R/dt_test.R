
#' Test function to make sure referencing in data.table works
#'
#' @param df dataframe in wide format
#' @param ref_var1 Referenced variable 1
#' @param ref_var2 Referenced variable 2
#' @return df
#' @export
#'

dt_test <- function(df, ref_var1, ref_var2){
  df <- 
    data.table::setDT(df)
    #.[, (ref_var1) := 123L] %>% #seems not to be working
  df <- data.table::set[df, j=get(ref_var1), value = 234L]
  df <- data.table::set[df, j="ref_var2", value = 45L]
  
  return(df)
}
  