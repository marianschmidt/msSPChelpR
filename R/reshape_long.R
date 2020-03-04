
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


reshape_long <- function(wide_df, case_id_var, time_id_var, chunks = 10){
  
  #number of patient IDs at start of function
  n_start <- wide_df %>% dplyr::select(case_id_var) %>% dplyr::n_distinct()
  
  #calculate times of variable repetition
  
  #split dataset in equal chunks and store in list
  rows_pc <- (nrow(wide_df) / chunks) %>% round(0)
  
  wide_df <- split(wide_df, (as.numeric(rownames(wide_df))-1) %/% rows_pc)
  
  
  #perform reshape command on each chunk
  wide_wide_df <- list()
  
  for(i in 1:chunks){
    
    long_df[[i]] <- wide_df[[i]] %>%
      as.data.frame %>%
      stats::reshape(timevar=time_id_var, idvar=case_id_var, direction = "long", sep=".")
    
    # wide_zfkddata_new <- wide_zfkddata %>%
    #   select(PSEUDOPATID, p_spc, p_futime, SEX.1, SEX.2, SEX.3, SEX.4, SEX.5, SEX.6)
    # 
    # varying_vars <- grep("\\.1$|\\.2$|\\.3$|\\.4$|\\.5$|\\.6$", names(wide_zfkddata_new))
    # 
    # zfkddata_new <- wide_zfkddata_new %>%
    #   as.data.frame() %>%         #
    #   reshape (timevar="TUMID3", idvar=c("PSEUDOPATID"), varying = varying_vars, direction = "long", times = 6, sep=".") %>%
    #   as_tibble %>%
    #   filter(!is.na(SEX)) %>%
    #   arrange(PSEUDOPATID) %>%
    #   select(-SEX)
    
    wide_df[[i]] <- 0
    
  }
  
  #rbind chunks into one dataframe
  long_df <- dplyr::bind_rows(long_df) %>% 
    dplyr::arrange(!! rlang::sym(case_id_var), !! rlang::sym(time_id_var))
  
  #check whether final number of patient IDs matches number at start.
  n_end <- long_df %>% nrow()
  
  if(n_end != n_start){
    warning('Unique n in long and wide dataset do not match. There may have been an error!')
  }
  
  return(long_df)
  
}