
#' Calculate patient status at specific end of follow-up
#'
#' @param df dataframe in wide format
#' @param fu_end end of follow-up in time format YYYY-MM-DD.
#' @param dattype Type of cancer registry data. Can be "seer" or "zfkd". Default is "zfkd".
#' @param life_var Name of variable containing life status. Will override dattype preset.  
#' @param spc_var Name of variable containing SPC status. Will override dattype preset.   
#' @param status_var Name of the newly calculated variable for patient status. Default is p_status.     
#' @param check Check newly calculated variable p_status      
#' @return df
#' @export
#'

pat_status <- function(df, fu_end = NULL, dattype = "zfkd", 
                       status_var = "p_status", life_var = NULL, spc_var = NULL, lifedat_var = NULL, 
                       life_stat_alive = NULL, life_stat_dead = NULL, spc_stat_yes = NULL, spc_stat_no = NULL,
                       check = TRUE){
  #setting default var names and values for SEER data
  if (dattype == "seer"){
    if(is.null(live_var)){
      life_var <- rlang::quo("STAT_REC.1")
    } else{
      life_var <- rlang::enquo(life_var)
    }
    if(is.null(spc_var)){
      spc_var <- rlang::quo("p_spc")
    } else{
      spc_var <- rlang::enquo(spc_var)
    }
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::quo("p_dodeath")
    } else{
      lifedat_var <- rlang::enquo(lifedat_var)
    }
    if(is.null(life_stat_alive)){
      life_stat_alive <- rlang::quo("Alive")
    } else{
      life_stat_alive <- rlang::enquo(life_stat_alive)
    }
    if(is.null(life_stat_dead)){
      life_stat_dead <- rlang::quo("Dead")
    } else{
      life_stat_dead <- rlang::enquo(life_stat_dead)
    }
    if(is.null(spc_stat_yes)){
      spc_stat_yes <- rlang::quo("SPC developed")
    } else{
      spc_stat_yes <- rlang::enquo(spc_stat_yes)
    }
    if(is.null(spc_stat_no)){
      spc_stat_no <- rlang::quo("no SPC")
    } else{
      spc_stat_no <- rlang::enquo(spc_stat_no)
    }
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(live_var)){
      life_var <- rlang::quo("TOD.1")
    } else{
      life_var <- rlang::enquo(life_var)
    }
    if(is.null(spc_var)){
      spc_var <- rlang::quo("p_spc")
    } else{
      spc_var <- rlang::enquo(spc_var)
    }
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::quo("SDIMP.1")
    } else{
      lifedat_var <- rlang::enquo(lifedat_var)
    }
    if(is.null(life_stat_alive)){
      life_stat_alive <- rlang::quo("no (patient alive)")
    } else{
      life_stat_alive <- rlang::enquo(life_stat_alive)
    }
    if(is.null(life_stat_dead)){
      life_stat_dead <- rlang::quo("yes (patient deceased)")
    } else{
      life_stat_dead <- rlang::enquo(life_stat_dead)
    }
    if(is.null(spc_stat_yes)){
      spc_stat_yes <- rlang::quo("SPC developed")
    } else{
      spc_stat_yes <- rlang::enquo(spc_stat_yes)
    }
    if(is.null(spc_stat_no)){
      spc_stat_no <- rlang::quo("no SPC")
    } else{
      spc_stat_no <- rlang::enquo(spc_stat_no)
    }
  }
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(life_var, spc_var, lifedat_var, status_var)
  
  not_found <- defined_vars[!(defined_vars %in% colnames(df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", not_found))
  }
  
  #check whether date was provided in correct format
  fu_end <- rlang::enquo(fu_end)
  if(!lubridate::is.Date(as.Date("2005-01-01", date.format = "%y-%m-%d"))) {
    rlang::abort("You have not provided a correct Follow-up date in the format YYYY-MM-DD")
  }

  #calculate new p_status variable and label it
 df %>%
  dplyr::mutate(!!p_status := dplyr::case_when(.data[[!!spc_var]] == !!spc_stat_no & .data[[!!live_var]] == !!life_stat_alive ~ 1,
                                 .data[[!!spc_var]] == !!spc_stat_no & .data[[!!live_var]] == !!life_stat_dead & .data[[!!livedat_var]] > !!fu_end ~ 1,
                                 .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!live_var]] == !!life_stat_alive ~ 2,
                                 .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!live_var]] == !!life_stat_dead & .data[[!!livedat_var]] > !!fu_end ~ 2,
                                 .data[[!!spc_var]] == !!spc_stat_no & .data[[!!live_var]] == !!life_stat_dead & .data[[!!livedat_var]] <= !!fu_end ~ 3,
                                 .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!live_var]] == !!life_stat_dead & .data[[!!livedat_var]] <= !!fu_end ~ 4,
                                TRUE ~ NA_real_)) %>%
  sjlabelled::var_labels(!!p_status := paste0("Patient Status at end of follow-up", rlang::quo_name(fu_end))) %>%
  sjlabelled::set_labels(!!p_status, labels = c("patient alive" = 1,
                                              "patient alive with SPC" = 2,
                                              "patient dead" = 3,
                                              "patient dead after SPC" = 4)) %>%
  dplyr::mutate_at(vars(!!p_status), sjlabelled::as_label, keep.labels=TRUE) 

 #conduct check on new variable
  if(check == TRUE){
  table(df[[!!live_var]], df[[!!p_status]], useNA = "ifany") %>% 
      as.data.frame.matrix() %>% 
      print()
  }
 
  return(df)

}