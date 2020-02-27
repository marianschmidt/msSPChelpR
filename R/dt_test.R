
#' Test function to make sure referencing in data.table works
#'
#' @param df dataframe in wide format
#' @param ref_var1 Referenced variable 1
#' @param ref_var2 Referenced variable 2
#' @param gender_var Variable for gender
#' @param fun reference to function (can be "v1", "v2", etc.)
#' @return df
#' @export
#'

dt_test <- function(df, ref_var1, ref_var2, gender_var = "SEX", fun = "v1"){
  
  dt_test1 <- function(df, ref_var1, ref_var2, gender_var){
    df <- 
      data.table::setDT(df)
    #.[, (ref_var1) := 123L] %>% #seems not to be working
    df <- data.table::set(df, i=NULL, j=ref_var1, value = 123L)
    df <- data.table::set(df, i=NULL, j=ref_var2, value = as.character(df[, get(gender_var)]))
    
    return(df)
  }
  
  dt_test2 <- function(df, ref_var1, ref_var2, gender_var){
    gender_var_quo <- rlang::ensym(gender_var)
    
    df <- 
      data.table::setDT(df) %>%
      .[, (ref_var1) := 123L] %>%
      .[, (ref_var2) := as.character(.[[rlang::eval_tidy(gender_var_quo)]])]
    
    return(df)
  }
  
  dt_test3 <- function(df, ref_var1, ref_var2, gender_var){
    df <- 
      data.table::setDT(df) %>%
      .[, `:=`(ref_var1 = 123L, ref_var2 = as.character(get(gender_var)))] %>%
      data.table::setnames(., old = c("ref_var1", "ref_var2"), new = c(ref_var1, ref_var2))
    
    return(df)
  }
  
  dt_test4 <- function(df, ref_var1, ref_var2, gender_var){
    
    gender_var_quo <- rlang::enquo(gender_var)
    
    df <- 
      data.table::setDT(df)
    #.[, (ref_var1) := 123L] %>% #seems not to be working
    df <- data.table::set(df, i=NULL, j=ref_var1, value = 123L)
    df <- data.table::set(df, i=NULL, j=ref_var2, value = as.character(df[[rlang::eval_tidy(gender_var_quo)]]))
    
    return(df)
  }
  
  
  dt_test5 <- function(df, ref_var1, ref_var2, gender_var){
    df <- 
      data.table::setDT(df)
    #.[, (ref_var1) := 123L] %>% #seems not to be working
    df <- data.table::set(df, i=NULL, j=ref_var1, value = 123L)
    df <- data.table::set(df, i=NULL, j=ref_var2, value = data.table::fcase(
      df[, get(gender_var)] == "Male", "M",
      df[, get(gender_var)] == "Female", "F",
      default = NA_character_
    ))
    
    return(df)
  }
  
  dt_test6 <- function(df, ref_var1, ref_var2, gender_var){
    gender_var_quo <- rlang::ensym(gender_var)
    
    df <- 
      data.table::setDT(df) %>%
      .[, (ref_var1) := 123L] %>%
      .[, (ref_var2) := data.table::fcase(
        .[[rlang::eval_tidy(gender_var_quo)]] == "Male", "M",
        .[[rlang::eval_tidy(gender_var_quo)]] == "Female", "F",
        default = NA_character_)]
    
    return(df)
  }
  
  dt_test7 <- function(df, ref_var1, ref_var2, gender_var){
    df <- 
      data.table::setDT(df) %>%
      .[, `:=`(ref_var1 = 123L, ref_var2 = data.table::fcase(
        get(gender_var) == "Male", "M",
        get(gender_var) == "Female", "F",
        default = NA_character_
      ))] 
    
    return(df)
  }
  
  dt_test8 <- function(df, ref_var1, ref_var2, gender_var){
    
    gender_var_quo <- rlang::ensym(gender_var)
    
    df <- 
      data.table::setDT(df)
    #.[, (ref_var1) := 123L] %>% #seems not to be working
    df <- data.table::set(df, i=NULL, j=ref_var1, value = 123L)
    df <- data.table::set(df, i=NULL, j=ref_var2, value = data.table::fcase(
      df[[rlang::eval_tidy(gender_var_quo)]] == "Male", "M",
      df[[rlang::eval_tidy(gender_var_quo)]] == "Female", "F",
      default = NA_character_
    ))
    
    return(df)
  }
  
  if(fun == "v1"){
    df <- dt_test1(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  if(fun == "v2"){
    df <- dt_test2(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  if(fun == "v3"){
    df <- dt_test3(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  
  if(fun == "v4"){
    df <- dt_test4(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  
  if(fun == "v5"){
    df <- dt_test5(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  if(fun == "v6"){
    df <- dt_test6(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  if(fun == "v7"){
    df <- dt_test7(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  if(fun == "v8"){
    df <- dt_test8(df=df, ref_var1=ref_var1, ref_var2=ref_var2, gender_var = gender_var)
  }
  
  return(df)
}


  