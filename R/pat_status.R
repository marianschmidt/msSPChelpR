
#' Calculate patient status at specific end of follow-up
#'
#' @param df dataframe in wide format
#' @param fu_end end of follow-up in time format YYYY-MM-DD.
#' @param dattype Type of cancer registry data. Can be "seer" or "zfkd". Default is "zfkd".
#' @param life_var Name of variable containing life status. Will override dattype preset.  
#' @param spc_var Name of variable containing SPC status. Will override dattype preset.   
#' @param status_var Name of the newly calculated variable for patient status. Default is p_status.    
#' @param birthdat_var Name of variable containing Date of Birth. Will override dattype preset.    
#' @param lifedat_var Name of variable containing Date of Death. Will override dattype preset.     
#' @param fcdat_var Name of variable containing Date of Primary Cancer diagnosis. Will override dattype preset.     
#' @param spcdat_var Name of variable containing Date of SPC diagnosis Will override dattype preset.
#' @param life_stat_alive Value for alive status in life_var. Will override dattype preset.
#' @param life_stat_dead Value for dead status in life_var. Will override dattype preset.
#' @param spc_stat_yes Value for SPC occured in spc_var. Will override dattype preset.
#' @param spc_stat_no Value for no SPC in spc_var. Will override dattype preset.
#' @param lifedat_fu_end Date of last FU of alive status in registry data. Will override dattype preset (2017-03-31 for zfkd; 2018-12-31 for seer).
#' @param check Check newly calculated variable p_status    
#' @return df
#' @export
#'

pat_status <- function(df, fu_end = NULL, dattype = "zfkd", 
                       status_var = "p_status", life_var = NULL, spc_var = NULL, birthdat_var = NULL, lifedat_var = NULL, fcdat_var = NULL, spcdat_var = NULL, 
                       life_stat_alive = NULL, life_stat_dead = NULL, spc_stat_yes = NULL, spc_stat_no = NULL, lifedat_fu_end = NULL,
                       check = TRUE){
  
  status_var <- rlang::enquo(status_var)
  
  #setting default var names and values for SEER data
  
  if (dattype == "seer"){
    if(is.null(life_var)){
      life_var <- rlang::quo("STAT_REC.1")
    } else{
      life_var <- rlang::enquo(life_var)
    }
    if(is.null(spc_var)){
      spc_var <- rlang::quo("p_spc")
    } else{
      spc_var <- rlang::enquo(spc_var)
    }
    if(is.null(birthdat_var)){
      birthdat_var <- rlang::quo("p_dobirth")
    } else{
      birthdat_var <- rlang::enquo(birthdat_var)
    }
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::quo("p_dodeath")
    } else{
      lifedat_var <- rlang::enquo(lifedat_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::quo("p_dofirst")
    } else{
      fcdat_var <- rlang::enquo(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::quo("p_dospc")
    } else{
      spcdat_var <- rlang::enquo(spcdat_var)
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
    if(is.null(lifedat_fu_end)){
      lifedat_fu_end <- rlang::quo("2018-12-31")
    } else{
      lifedat_fu_end <- rlang::enquo(lifedat_fu_end)
    }
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(life_var)){
      life_var <- rlang::quo("TOD.1")
    } else{
      life_var <- rlang::enquo(life_var)
    }
    if(is.null(spc_var)){
      spc_var <- rlang::quo("p_spc.1")
    } else{
      spc_var <- rlang::enquo(spc_var)
    }
    if(is.null(birthdat_var)){
      birthdat_var <- rlang::quo("GDIMP.1")
    } else{
      birthdat_var <- rlang::enquo(birthdat_var)
    }
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::quo("SDIMP.1")
    } else{
      lifedat_var <- rlang::enquo(lifedat_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::quo("DDIMP.1")
    } else{
      fcdat_var <- rlang::enquo(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::quo("DDIMP.2")
    } else{
      spcdat_var <- rlang::enquo(spcdat_var)
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
    if(is.null(lifedat_fu_end)){
      lifedat_fu_end <- rlang::quo("2017-03-31")
    } else{
      lifedat_fu_end <- rlang::enquo(lifedat_fu_end)
    }
  }
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::quo_name(life_var), rlang::quo_name(spc_var), rlang::quo_name(lifedat_var))

  not_found <- defined_vars[!(defined_vars %in% colnames(df))]

  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", paste(not_found, collapse=", ")))
  }
  
  #check whether date was provided in correct format
  fu_end <- rlang::enquo(fu_end)
  if(!lubridate::is.Date(as.Date("2005-01-01", date.format = "%y-%m-%d"))) {
    rlang::abort("You have not provided a correct Follow-up date in the format YYYY-MM-DD")
  }
  
  #make label for new variable
  statvar_label <- paste("Patient Status at end of follow-up", quo_name(fu_end))

  #calculate new status_var variable and label it
  #todo: implement check on date of spc_diagnosis and date of birth and introduce new status.
 df <- df %>%
  dplyr::mutate(!!status_var := dplyr::case_when(
    #patient is not born before end of follow-up
    .data[[!!birthdat_var]] > !!fu_end ~ 97,
    #patient has not developed FC before end of follow-up
    .data[[!!fcdat_var]] > !!fu_end ~ 98,
    #patient date of death is missing
    .data[[!!life_var]] == !!life_stat_dead & is.na(.data[[!!lifedat_var]]) & !!lifedat_fu_end > !!fu_end ~ 99,
    #patient is alive after FC and before end of FU (independet of whether SPC has developed or not after FU)
    .data[[!!spc_var]] == !!spc_stat_no & .data[[!!life_var]] == !!life_stat_alive ~ 1,
    .data[[!!spc_var]] == !!spc_stat_no & .data[[!!life_var]] == !!life_stat_dead & .data[[!!lifedat_var]] > !!fu_end ~ 1,
    .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!spcdat_var]] > !!fu_end & .data[[!!life_var]] == !!life_stat_alive ~ 1,
    .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!spcdat_var]] > !!fu_end & .data[[!!life_var]] == !!life_stat_dead & .data[[!!lifedat_var]] > !!fu_end ~ 1,
    #patient is alive after SPC and before end of FU
    .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!spcdat_var]] <= !!fu_end & .data[[!!life_var]] == !!life_stat_alive ~ 2,
    .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!spcdat_var]] <= !!fu_end & .data[[!!life_var]] == !!life_stat_dead & .data[[!!lifedat_var]] > !!fu_end ~ 2,
    #patient is dead after FC and before end of FU
    .data[[!!spc_var]] == !!spc_stat_no & .data[[!!life_var]] == !!life_stat_dead & .data[[!!lifedat_var]] <= !!fu_end ~ 3,
    .data[[!!spc_var]] == !!spc_stat_no & .data[[!!life_var]] == !!life_stat_dead & is.na(.data[[!!lifedat_var]]) & !!lifedat_fu_end <= !!fu_end ~ 3,
    #patient is dead after SPC and before end of FU
    .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!spcdat_var]] <= !!fu_end & .data[[!!life_var]] == !!life_stat_dead & .data[[!!lifedat_var]] <= !!fu_end ~ 4,
    .data[[!!spc_var]] == !!spc_stat_yes & .data[[!!spcdat_var]] <= !!fu_end & .data[[!!life_var]] == !!life_stat_dead & is.na(.data[[!!lifedat_var]]) & !!lifedat_fu_end <= !!fu_end ~ 4,
    TRUE ~ NA_real_)) %>%
   #label new variable
   sjlabelled::var_labels(!!status_var := !!statvar_label) %>%
   sjlabelled::set_labels(!!status_var, labels = c("patient alive after FC (with or without following SPC)" = 1,
                                                   "patient alive after SPC" = 2,
                                                   "patient dead after FC" = 3,
                                                   "patient dead after SPC" = 4,
                                                   "NA - patient not born before end of FU" = 97,
                                                   "NA - patient did not develop cancer before end of FU" = 98,
                                                   "NA - patient date of death is missing" = 99)) #%>%
   #dplyr::mutate_at(dplyr::vars(!!status_var), sjlabelled::as_label, keep.labels=TRUE) 

 #conduct check on new variable
  if(check == TRUE){
  check_tab <- df %>%
    dplyr::count(.data[[!!life_var]], .data[[!!status_var]])
  
  print(check_tab)
  
  }
 
  return(df)

}