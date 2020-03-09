
#' Calculate patient status at specific end of follow-up using data.table
#'
#' @param wide_df dataframe in wide format
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
#' @param as_labelled_factor If true, output status_var as labelled factor variable. Default is FALSE.
#' @return wide_df
#' @export
#'

pat_status_dt <- function(wide_df, fu_end = NULL, dattype = "zfkd", 
                          status_var = "p_status", life_var = NULL, spc_var = NULL, birthdat_var = NULL, lifedat_var = NULL, fcdat_var = NULL, spcdat_var = NULL, 
                          life_stat_alive = NULL, life_stat_dead = NULL, spc_stat_yes = NULL, spc_stat_no = NULL, lifedat_fu_end = NULL,
                          check = TRUE, as_labelled_factor = FALSE){
  
  
  
  #setting default var names and values for SEER data
  
  if (dattype == "seer"){
    if(is.null(life_var)){
      life_var <- "STAT_REC.1"
    } 
    if(is.null(spc_var)){
      spc_var <- "p_spc"
    } 
    if(is.null(birthdat_var)){
      birthdat_var <- "p_dobirth"
    } 
    if(is.null(lifedat_var)){
      lifedat_var <- "p_dodeath"
    } 
    if(is.null(fcdat_var)){
      fcdat_var <- "p_dofirst"
    } 
    if(is.null(spcdat_var)){
      spcdat_var <- "p_dospc"
    } 
    if(is.null(life_stat_alive)){
      life_stat_alive <- "Alive"
    } 
    if(is.null(life_stat_dead)){
      life_stat_dead <- "Dead"
    } 
    if(is.null(spc_stat_yes)){
      spc_stat_yes <- "SPC developed"
    } 
    if(is.null(spc_stat_no)){
      spc_stat_no <- "no SPC"
    } 
    if(is.null(lifedat_fu_end)){
      lifedat_fu_end <- "2018-12-31"
    } 
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(life_var)){
      life_var <- "TOD.1"
    } 
    if(is.null(spc_var)){
      spc_var <- "p_spc.1"
    } 
    if(is.null(birthdat_var)){
      birthdat_var <- "GDIMP.1"
    } 
    if(is.null(lifedat_var)){
      lifedat_var <- "SDIMP.1"
    } 
    if(is.null(fcdat_var)){
      fcdat_var <- "DDIMP.1"
    } 
    if(is.null(spcdat_var)){
      spcdat_var <- "DDIMP.2"
    }
    if(is.null(life_stat_alive)){
      life_stat_alive <- "no (patient alive)"
    } 
    if(is.null(life_stat_dead)){
      life_stat_dead <- "yes (patient deceased)"
    } 
    if(is.null(spc_stat_yes)){
      spc_stat_yes <- "SPC developed"
    } 
    if(is.null(spc_stat_no)){
      spc_stat_no <- "no SPC"
    } 
    if(is.null(lifedat_fu_end)){
      lifedat_fu_end <- "2017-03-31"
    } 
  }
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::quo_name(life_var), rlang::quo_name(spc_var), rlang::quo_name(lifedat_var),
                    rlang::quo_name(birthdat_var), rlang::quo_name(fcdat_var), rlang::quo_name(spcdat_var))
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", paste(not_found, collapse=", ")))
  }
  
  #check whether date was provided in correct format
  if(!lubridate::is.Date(as.Date(fu_end, date.format = "%y-%m-%d"))) {
    rlang::abort("You have not provided a correct Follow-up date in the format YYYY-MM-DD")
  }
  
  #make label for new variable
  statvar_label <- paste("Patient Status at end of follow-up", fu_end)
  
  #calculate new status_var variable and label it
  #todo: implement check on date of spc_diagnosis and date of birth and introduce new status.
  data.table::setDT(wide_df)
  wide_df <- data.table::set(wide_df, i=NULL, j=status_var, value = data.table::fcase(
        #patient is not born before end of follow-up
        wide_df[[birthdat_var]] > fu_end,    97L,
        #patient has not developed FC before end of follow-up
        wide_df[[fcdat_var]] > fu_end,       98L, 
        #patient date of death is missing
        wide_df[[life_var]] == life_stat_dead & is.na(wide_df[[lifedat_var]]) & lifedat_fu_end > fu_end,   99L,
        #patient is alive after FC and before end of FU (independet of whether SPC has developed or not after FU)
        wide_df[[spc_var]] == spc_stat_no & wide_df[[life_var]] == life_stat_alive,    1L,
        wide_df[[spc_var]] == spc_stat_no & wide_df[[life_var]] == life_stat_dead & wide_df[[lifedat_var]] > fu_end,   1L,
        wide_df[[spc_var]] == spc_stat_yes & wide_df[[spcdat_var]] > fu_end & wide_df[[life_var]] == life_stat_alive,  1L,
        wide_df[[spc_var]] == spc_stat_yes & wide_df[[spcdat_var]] > fu_end & wide_df[[life_var]] == life_stat_dead & wide_df[[lifedat_var]] > fu_end,  1L,
        #patient is alive after SPC and before end of FU
        wide_df[[spc_var]] == spc_stat_yes & wide_df[[spcdat_var]] <= fu_end & wide_df[[life_var]] == life_stat_alive, 2L,
        wide_df[[spc_var]] == spc_stat_yes & wide_df[[spcdat_var]] <= fu_end & wide_df[[life_var]] == life_stat_dead & wide_df[[lifedat_var]] > fu_end, 2L,
        #patient is dead after FC and before end of FU
        wide_df[[spc_var]] == spc_stat_no & wide_df[[life_var]] == life_stat_dead & wide_df[[lifedat_var]] <= fu_end,  3L,
        wide_df[[spc_var]] == spc_stat_no & wide_df[[life_var]] == life_stat_dead & is.na(wide_df[[lifedat_var]]) & lifedat_fu_end <= fu_end,     3L,
        #patient is dead after SPC and before end of FU
        wide_df[[spc_var]] == spc_stat_yes & wide_df[[spcdat_var]] <= fu_end & wide_df[[life_var]] == life_stat_dead & wide_df[[lifedat_var]] <= fu_end, 4L,
        wide_df[[spc_var]] == spc_stat_yes & wide_df[[spcdat_var]] <= fu_end & wide_df[[life_var]] == life_stat_dead & is.na(wide_df[[lifedat_var]]) & lifedat_fu_end <= fu_end, 4L,
        #missings
        default = NA_integer_
      ))
  
  #add variable label to status_var
  sjlabelled::set_label(wide_df[[status_var]]) <- statvar_label
  
  #add value labels to status_var
  wide_df[[status_var]] <- sjlabelled::set_labels(wide_df[[status_var]], labels =  c("patient alive after FC (with or without following SPC after end of FU)" = 1,
                                                                                     "patient alive after SPC" = 2,
                                                                                     "patient dead after FC" = 3,
                                                                                     "patient dead after SPC" = 4,
                                                                                     "NA - patient not born before end of FU" = 97,
                                                                                     "NA - patient did not develop cancer before end of FU" = 98,
                                                                                     "NA - patient date of death is missing" = 99),
                                                  force.labels = TRUE)
  
  
  #enforce option as_labelled_factor = TRUE
  if(as_labelled_factor == TRUE){
    wide_df <- data.table::set(wide_df, i=NULL, j=status_var, value =
                                            sjlabelled::as_label(wide_df[[status_var]], keep.labels=TRUE))
  }
  
  #conduct check on new variable
  if(check == TRUE){
    #count n for life_var and status_var
    check_tab <- wide_df %>%
      .[, .N, by = c(life_var, status_var)]

    print(check_tab)
    
    #count n of new status_var
    freq_tab <- wide_df %>%
      .[, .N, by = c(status_var)]
    
    print(freq_tab)
    
  }
  
  return(wide_df)
  
}

