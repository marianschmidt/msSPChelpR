
#' Calculate age-standardized incidence rates
#'
#' @param df dataframe in wide format
#' @param dattype can be "zfkd" or "seer" or empty. Will set default variable names from dataset.
#' @param std_pop can be either "ESP2013, ESP1976, WHO1960
#' @param truncate_std_pop if TRUE standard population will be truncated for all age-groups that do not occur in df
#' @param futime_src can bei either "refpop" or "cohort"
#' @param summarize_groups option to define summarizing stratified groups. Default is "none". 
#'                 If you want to define variables that should be summarized into one group, you can chose from region_var, sex_var, year_var. 
#'                 Define multiple summarize variables by summarize_groups = c("region", "sex", "year")
#' @param count_var variable to be counted as observed case. Should be 1 for case to be counted.
#' @param stdpop_df df where standard population is defined. It is assumed that stdpop_df has the columns "sex" for gender, "age" for age-groups,
#'                  "standard_pop" for name of standard population (e.g. "European Standard Population 2013) and "population_n" for size of standard population age-group.
#'                  stdpop_df must use the same category coding of age and sex as agegroup_var and sex_var.
#' @param refpop_df df where reference population data is defined. Only required if option futime = "refpop" is chosen. It is assumed that refpop_df has the columns 
#'                  "region" for region, "sex" for gender, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "population_pyar" for person-years at risk in the respective age/gender/year cohort.
#'                  refpop_df must use the same category coding of age, sex, region, year and icdcat as agegroup_var, sex_var, region_var, year_var and icdcat_var. 
#' @param region_var variable in df that contains information on region where case was incident. Default is set if dattype is given.
#' @param agegroup_var variable in df that contains information on age-group. Default is set if dattype is given.
#' @param sex_var variable in df that contains information on gender. Default is set if dattype is given.
#' @param year_var variable in df that contains information on year or year-period when case was incident. Default is set if dattype is given.
#' @param icdcat_var variable in df that contains information on ICD code of case diagnosis. Default is set if dattype is given.
#' @param futime_var variable in df that contains follow-up time per person (in years) in cohort (can only be used with futime_src = "cohort"). Default is set if dattype is given.
#' @param pyar_var variable in refpop_df that contains person-years-at-risk in reference population (can only be used with futime_src = "refpop") Default is set if dattype is given.
#' @param alpha signifcance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @return df
#' @importFrom rlang .data
#' @export
#'
#'


asir <-
  function(df,
           dattype = "zfkd",
           std_pop = "ESP2013",
           truncate_std_pop = FALSE,
           futime_src = "refpop",
           summarize_groups = "none",
           count_var,
           stdpop_df = standard_population,
           refpop_df = population,
           region_var = NULL,
           agegroup_var = NULL,
           sex_var = NULL,
           year_var = NULL,
           icdcat_var = NULL,
           futime_var = NULL,
           pyar_var = NULL,
           alpha = 0.05) {
    
    ###----  prepwork
    
    #setting default parameters
    options_dplyr_old <- options(dplyr.summarise.inform = TRUE) # save old setting for showing dplyr messages
    on.exit(options(options_dplyr_old), add = TRUE) #make sure old options are used when exiting function
    options(dplyr.summarise.inform = FALSE) #set new setting for not showing dplyr messages to avoid outbut by summarize()
    
    ### check if df and std_pop_df exist and are dataframes
    
    if (exists("df") && is.data.frame(get("df"))){}
    else{
      rlang::abort(paste0("The following df for for providing the observed cases does not exist or is not a dataframe: ",
                          rlang::as_name(df)))
    }
    
    if (exists("stdpop_df") && is.data.frame(get("stdpop_df"))){}
    else{
      rlang::abort(paste0("The following stdpop_df for for providing the standard population does not exist or is not a dataframe: ",
                          rlang::as_name(stdpop_df)))
    }
    
    ### remove all labels from dfs to avoid warning messages
    
    df <- sjlabelled::remove_all_labels(df)
    stdpop_df <- sjlabelled::remove_all_labels(stdpop_df)
    
    
    ### check if refpop_df exists and is dataframe
    
    if (futime_src == "refpop") {
      
      if (exists("refpop_df") && is.data.frame(get("refpop_df"))){
        
        refpop_df <- sjlabelled::remove_all_labels(refpop_df)
        
      }
      else{
        rlang::abort(paste0("The following refpop_df for for providing the reference population does not exist or is not a dataframe: ",
                            rlang::as_name(refpop_df)))
      }
      
    }
    
    ### check futime_scr
    
    if (futime_src != "cohort" & futime_src != "refpop") {
      rlang::abort(paste0("The following source for futime defined is not valid: ",
                          futime_src, " Use any of c('refpop', 'cohort') instead")
      )
    } 
    
    ### setting default var names and values for SEER data --> still need to update to final names!
    if (dattype == "seer") {
      if (is.null(region_var)) {
        region_var <- rlang::sym("p_region.1")
      } else{
        region_var <- rlang::ensym(region_var)
      }
      if (is.null(agegroup_var)) {
        agegroup_var <- rlang::sym("t_agegroup.1")
      } else{
        agegroup_var <- rlang::ensym(agegroup_var)
      }
      if (is.null(sex_var)) {
        sex_var <- rlang::sym("SEX.1")
      } else{
        sex_var <- rlang::ensym(sex_var)
      }
      if (is.null(year_var)) {
        year_var <- rlang::ensym("t_yeardiag.1")
      } else{
        year_var <- rlang::ensym(year_var)
      }
      if (is.null(icdcat_var)) {
        icdcat_var <- rlang::sym("t_icdcat.1")
      } else{
        icdcat_var <- rlang::ensym(icdcat_var)
      }
      if (is.null(futime_var)) {
        futime_var <- rlang::sym("p_futimeyrs.1")
      } else{
        futime_var <- rlang::ensym(futime_var)
      }
    }
    
    
    #setting default var names and values for ZfKD data
    if (dattype == "zfkd") {
      if (is.null(region_var)) {
        region_var <- rlang::sym("p_region.1")
      } else{
        region_var <- rlang::ensym(region_var)
      }
      if (is.null(agegroup_var)) {
        agegroup_var <- rlang::sym("t_agegroupdiag.1")
      } else{
        agegroup_var <- rlang::ensym(agegroup_var)
      }
      if (is.null(sex_var)) {
        sex_var <- rlang::sym("SEX.1")
      } else{
        sex_var <- rlang::ensym(sex_var)
      }
      if (is.null(year_var)) {
        year_var <- rlang::sym("t_yeardiag.1")
      } else{
        year_var <- rlang::ensym(year_var)
      }
      if (is.null(icdcat_var)) {
        icdcat_var <- rlang::sym("t_icdcat.1")
      } else{
        icdcat_var <- rlang::ensym(icdcat_var)
      }
      if (is.null(futime_var)) {
        futime_var <- rlang::sym("p_futimeyrs.1")
      } else{
        futime_var <- rlang::ensym(futime_var)
      }
    }
    
    #setting standard population
    
    if(!(std_pop %in% c("ESP2013", "ESP1976", "WHO1960"))){
      rlang::abort(
        paste0(
          "The following population is not a valid standard population to be used: ", std_pop, "Please use any of ESP2013, ESP1976, WHO1960."
        )
      )
    }
    
    if (std_pop == "ESP2013") {
      std_pop <- "European Standard Population 2013"
    } 
    if (std_pop == "ESP1976") {
      std_pop <- "European Standard Population 1976"
    } 
    if (std_pop == "WHO1960") {
      std_pop <- "World Standard Population 1960"
    } 
    
    
    ### handling other user inputs
    count_var <- rlang::ensym(count_var)
    
    if (futime_src == "refpop") {
      if (is.null(pyar_var)) {
        pyar_var <- rlang::sym("population_pyar")
      } else{
        pyar_var <- rlang::ensym(pyar_var)
      }
    }
    
    #CHK1: check whether all required variables are defined and present in dataset
    defined_vars <-
      c(
        rlang::as_name(region_var),
        rlang::as_name(agegroup_var),
        rlang::as_name(sex_var),
        rlang::as_name(year_var),
        rlang::as_name(icdcat_var),
        rlang::as_name(count_var),
        if(futime_src == "cohort"){rlang::as_name(futime_var)}
      )
    
    not_found <- defined_vars[!(defined_vars %in% colnames(df))]
    
    
    if (length(not_found) > 0) {
      rlang::abort(
        paste0(
          "The following variables defined are not found in the provided dataframe df: ",
          paste(not_found, collapse = ", ")
        )
      )
    }
    
    
    ### first case using the pyears within the cohort.
    
    if (futime_src == "cohort") {
      message("Using follow-up time from within cohort as pyears for calculating incidence rates.")
      
      #c_DM1: calc observe and follow-up times and absolute incidence rates
      
      #c_DM1a calc observed counts per stratum i by region, age-group, gender, year of PC diagnosis and ICD-Code of canc_id
      sircalc_count <- df %>%
        dplyr::group_by(!!region_var,
                        !!agegroup_var,
                        !!sex_var,
                        !!year_var,
                        !!icdcat_var) %>%
        dplyr::summarize(i_observed = sum(!!count_var)) %>%
        dplyr::ungroup()
      
      #c_DM1b calc follow-up times in dataset per stratum i by region, age-group, gender, year of PC diagnosis
      sircalc_fu <- df %>%
        dplyr::group_by(!!region_var,!!agegroup_var,!!sex_var,!!year_var) %>%
        dplyr::summarize(i_pyar = sum(!!futime_var, na.rm = TRUE)) %>%
        dplyr::ungroup()
      
      #c_DM1c merge sircalc_fu and sircalc_count
      sircalc <-
        dplyr::full_join(
          sircalc_fu,
          sircalc_count,
          by = c(
            rlang::expr_text(region_var),
            rlang::expr_text(agegroup_var),
            rlang::expr_text(sex_var),
            rlang::expr_text(year_var)
          )
        )
      
      
      #c_DM1d calc absolute incidence rate per 100,000 and confidence intervals per stratum i
      sircalc <- sircalc %>%
        dplyr::mutate(
          i_abs_ir = .data$i_observed / .data$i_pyar * 100000,
          i_abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$i_observed) / 2) / .data$i_pyar * 100000,
          i_abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$i_observed + 1)) / 2) / .data$i_pyar * 100000
        )
      
      #c_DM2: merge standard population
      #c_DM2a: prepare standard pop data
      
      used_sex <- sircalc %>%
        dplyr::distinct(!!sex_var) %>% #dplyr equivalent to unique()
        dplyr::pull()  %>%             #to return vector instead of df
        as.character()                       
      
      used_ages <- sircalc %>%
        dplyr::distinct(!!agegroup_var) %>%
        dplyr::pull()%>%
        as.character()
      
      used_year <- sircalc %>%
        dplyr::distinct(!!year_var) %>%
        dplyr::pull()%>%
        as.character()
      
      ref_stdpop_ages <- stdpop_df %>%
        dplyr::filter(.data$standard_pop == std_pop &
                        .data$age != "85 - 89" &             #remove options from ESP2013 that are not reflected in datasets
                        .data$age != "90 - 120") %>%
        dplyr::distinct(.data$age) %>% 
        dplyr::pull() %>%
        as.character()
      
      min_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% min()
      max_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% max()
      
      min_age <- stringr::str_sub(used_ages, 1, 2) %>% as.numeric() %>% min()
      max_age <- stringr::str_sub(used_ages, -3) %>% as.numeric() %>% max()
      
      ages_without_data <- ref_stdpop_ages[!(ref_stdpop_ages %in% used_ages)]
      
      if (length(ages_without_data) > 0) {
        message(
          paste0(
            "For the following age-groups there were no cases to be found in the dataset. Incidence and PYARs will be set to 0: ",
            paste(ages_without_data, collapse = ", ")
          )
        )
      }
      
      
      stdpop_df <- stdpop_df %>%
        dplyr::filter(.data$standard_pop == std_pop &
                        .data$sex %in% used_sex & 
                        .data$age != "Total - All age groups") %>%
        dplyr::mutate(sex = as.character(.data$sex),
                      age = as.character(.data$age))
      
      #c_DM2b: prepare sircalc data
      
      sircalc <- sircalc %>%
        dplyr::mutate(age = as.character(!!agegroup_var),
                      sex = as.character(!!sex_var)) %>%
        dplyr::select(-!!agegroup_var,-!!sex_var)
      
      #i)making nested version of df and joining populations to each df
      sircalc_nest <- sircalc %>%
        dplyr::group_by(!!region_var,!!year_var,!!icdcat_var) %>%
        tidyr::nest()
      
      #ii)function for nested join
      join_df <- function(df_nest, df_other) {
        df_all <- dplyr::right_join(df_nest, df_other, by = c("sex", "age"))
        return(df_all)
      }
      
      #iii)performing join
      sircalc_nest <- sircalc_nest %>%
        dplyr::mutate(data = purrr::map(data, ~ join_df(., stdpop_df)))
      
      #iv)unnest
      sircalc <- sircalc_nest %>%
        tidyr::unnest(cols = .data$data)
      
      
      #v) enforce truncated standard population option
      
      if(truncate_std_pop == TRUE){
        sircalc <- sircalc %>%
          dplyr::filter(.data$age %in% used_ages) 
        
        message(paste("The following age groups have been removed before calculating the age-standardized rate: ",
                      paste(ages_without_data, collapse = ", "),"
                      Population proportions of the standard population were redistributed proportionally for the remaining age groups.",
                      std_pop, "was used as standard population."))
      }
      
      #vi) calculate new group proportions in standard population given that some of the younger age-groups were truncated
      
      sum_group_population <- sircalc %>%
        dplyr::group_by(!!region_var, .data$sex, !!year_var, !!icdcat_var) %>%
        dplyr::summarize(group_pop_sum = sum(.data$population_n, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$group_pop_sum) %>%
        dplyr::distinct(.data$group_pop_sum) %>%
        dplyr::filter(.data$group_pop_sum > 0) %>%
        dplyr::pull()
      
      #CHK vi sum for group_proportion should be the same across all strata i
      
      if (length(sum_group_population) > 1) {
        rlang::abort(
          paste("Error when recalculating group proportions of standard population. Varying values for sum_group_proportions:",
                paste(sum_group_population, collapse = ", ")
          )
        )
      }
      
      sircalc <- sircalc %>%
        dplyr::mutate(group_proportion_recalc = .data$population_n / sum_group_population)
      
      #AN1: calculate incremental rates per stratum i and variance per stratum
      
      sircalc <- sircalc %>%
        dplyr::mutate(
          asir_strat = .data$i_abs_ir * .data$group_proportion_recalc,
          #variance calculation based on @brayChapterAgeStandardization2014, p. 114, Poisson distribution; var=sum(i_observed*(group_proportion_recalc / i_pyar)^2)
          i_var_strat = (.data$i_observed * (.data$group_proportion_recalc / .data$i_pyar)^2) * 100000,
          #round very small pyars to 0 for functions that would be negatively affected
          i_pyar0 = dplyr::case_when(.data$i_pyar < 0.0001 ~ NA_real_,
                                     TRUE ~ .data$i_pyar)
        )
      
      #AN2: calculate age-standardized rate
      
      #sum over all age-groups to get age standardized incidence rate
      asir_results <- sircalc %>%
        dplyr::group_by(!!region_var, .data$sex,!!year_var,!!icdcat_var) %>%
        dplyr::summarize(
          group_observed = sum(.data$i_observed, na.rm = TRUE),
          group_pyar = sum(.data$i_pyar, na.rm = TRUE),
          group_abs_ir = round(.data$group_observed / .data$group_pyar * 100000, 2),
          group_abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$group_observed) / 2) / .data$group_pyar * 100000,
          group_abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$group_observed + 1)) / 2) / .data$group_pyar * 100000,
          asir = sum(.data$asir_strat, na.rm = TRUE),
          #confidence intervals based on Poisson approximation based on @brayChapterAgeStandardization2014, p. 114, Poisson distribution; var=sum(i_observed*(group_proportion_recalc / i_pyar)^2)
          var_asir = sum(.data$i_var_strat, na.rm = TRUE),
          asir_lci = .data$asir + stats::qnorm(p = alpha / 2) * sqrt(.data$var_asir),
          asir_uci = .data$asir + stats::qnorm(p = 1 - alpha / 2) * sqrt(.data$var_asir),
          #Exact Confidence Intervals based on gamma distribution
          #function adapted from epitools::ageadjust.direct (@aragonEpitoolsEpidemiologyTools2017a), based on Fay and Feuer 2017 @fayConfidenceIntervalsDirectly1997)
          asir_e6 = .data$asir / 100000,
          asir_copy = .data$asir,
          var_asir_gam = sum((.data$group_proportion_recalc ^ 2) * (.data$i_observed /.data$i_pyar ^ 2), na.rm = TRUE),
          asir_lci_gam = stats::qgamma(
            alpha / 2,
            shape = (.data$asir_e6 ^ 2) / .data$var_asir_gam,
            scale = .data$var_asir_gam / .data$asir_e6
          ) * 100000,
          asir_uci_gam = stats::qgamma(
            1 - alpha / 2,
            shape = ((.data$asir_e6 + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE)) ^ 2) / (.data$var_asir_gam + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE) ^ 2),
            scale = (.data$var_asir_gam + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE) ^ 2) / (.data$asir_e6 + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE))
          ) * 100000,
          age = paste0("Age standardized by ", std_pop)
        ) %>%
        dplyr::ungroup()
      
      #enforce truncated standard population option
      if(truncate_std_pop == TRUE){
        asir_results <- asir_results %>%
          dplyr::mutate(age = paste0("Age standardized by truncated ", std_pop, ": truncated to ages ", min_age, " - ", max_age))
      }
      
      asir_results <- asir_results %>%
        dplyr::rename(
          observed = .data$group_observed,
          pyar = .data$group_pyar,
          abs_ir = .data$group_abs_ir,
          abs_ir_lci = .data$group_abs_ir_lci,
          abs_ir_uci = .data$group_abs_ir_uci
        ) %>%
        dplyr::mutate(dplyr::across(.cols = c(.data$asir, .data$asir_copy, .data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci, .data$asir_lci, .data$asir_uci, 
                                              .data$asir_lci_gam, .data$asir_uci_gam), .fns = ~round(.,2))) %>%
        dplyr::mutate(dplyr::across(.cols = .data$pyar, ~ round(.,0))) %>%
        dplyr::select(.data$age, !!region_var, .data$sex, !!year_var, !!icdcat_var, .data$asir, .data$observed, .data$pyar, .data$abs_ir, .data$abs_ir_lci, 
                      .data$abs_ir_uci, .data$asir_copy, .data$asir_lci, .data$asir_lci_gam, .data$asir_uci, .data$asir_uci_gam, .data$asir_e6)
      
      
      return(asir_results)
      
    }
    
    ### second case: Using person-years at risk [PYAR] from reference population as pyears for calculating incidence rates.
    
    if (futime_src == "refpop") {
      message(
        "Using person-years at risk [PYAR] from reference population as pyears for calculating incidence rates."
      )
      
      
      
      #r_DM1: calc observe and make nested dataframe
      
      #r_1b calc observed counts per stratum i by region, age-group, gender, year of PC diagnosis and ICD-Code of canc_id
      sircalc_count <- df %>%
        dplyr::group_by(!!region_var,
                        !!agegroup_var,
                        !!sex_var,
                        !!year_var,
                        !!icdcat_var) %>%
        dplyr::summarize(i_observed = sum(!!count_var)) %>%
        dplyr::ungroup()
      
      #r_DM2: merge standard population
      #r_DM2a: prepare standard pop data
      
      used_sex <- sircalc_count %>%
        dplyr::distinct(!!sex_var) %>% #dplyr equivalent to unique()
        dplyr::pull()    %>%
        as.character()                      #to return vector instead of df
      
      used_region <- sircalc_count %>%
        dplyr::distinct(!!region_var) %>%
        dplyr::pull()%>%
        as.character()
      
      used_year <- sircalc_count %>%
        dplyr::distinct(!!year_var) %>%
        dplyr::pull()%>%
        as.character()
      
      used_ages <- sircalc_count %>%
        dplyr::distinct(!!agegroup_var) %>%
        dplyr::pull()%>%
        as.character()
      
      used_icdcat <- sircalc_count %>%
        dplyr::distinct(!!icdcat_var) %>%
        dplyr::pull()%>%
        as.character()
      
      stdpop_df <- stdpop_df %>%
        dplyr::filter(.data$standard_pop == std_pop &
                        .data$sex %in% used_sex & .data$age != "Total - All age groups") %>%
        dplyr::mutate(sex = as.character(.data$sex),
                      age = as.character(.data$age))
      
      #r_DM2b: prepare sircalc data
      
      sircalc_count <- sircalc_count %>%
        dplyr::mutate(age = as.character(!!agegroup_var),
                      sex = as.character(!!sex_var))
      
      #o)filling-up emty categories of region, year and icdcat
      
      max_sircalc <- sircalc_count %>%
        tidyr::expand(.data$age, .data$sex, !!region_var, !!icdcat_var, !!year_var)
      
      sircalc_count <-  dplyr::full_join(sircalc_count, max_sircalc, by = c("age", "sex", rlang::as_name(region_var) , rlang::as_name(year_var), rlang::as_name(icdcat_var)))
      
      min_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% min()
      max_year <- stringr::str_sub(used_year, 1, 4) %>% as.numeric() %>% max()
      
      min_age <- stringr::str_sub(used_ages, 1, 2) %>% as.numeric() %>% min()
      max_age <- stringr::str_sub(used_ages, -3) %>% as.numeric() %>% max()
      
      message(paste0("Be careful, in this calculation it is assumed that all included regions have collected data for the full time period: ", min_year, " to ", max_year, "
                       If you have included registries with differing times, please check this assumption by looking at groups with 0 incidence and specify option 'inclusion_restrictions' if needed."))
      
      #i)making nested version of df and joining populations to each df
      sircalc_count_nest <- sircalc_count %>%
        dplyr::group_by(!!region_var,!!year_var,!!icdcat_var) %>%
        tidyr::nest()
      
      #ii)function for nested join
      join_df <- function(df_nest, df_other) {
        df_all <- dplyr::right_join(df_nest, df_other, by = c("sex", "age"))
        return(df_all)
      }
      
      #iii)performing join
      sircalc_count_nest <- sircalc_count_nest %>%
        dplyr::mutate(data = purrr::map(data, ~ join_df(., stdpop_df)))
      
      #iv)unnest
      sircalc_count <- sircalc_count_nest %>%
        tidyr::unnest(cols = .data$data)
      
      #DM3: merge reference population
      
      #CHK3: check if all regions, ages, genders, years can be found in reference population dataset and give error if not
      
      
      message(paste("The following regions, age groups, years, genders and ICD codes are considered: ",
                    paste(used_region, collapse = ", "),
                    paste(used_year, collapse = ", "),
                    paste(used_ages, collapse = ", "),
                    paste(used_sex, collapse = ", "),
                    paste(used_icdcat, collapse = ", ")))
      
      ref_sex <- refpop_df %>%
        dplyr::distinct(.data$sex) %>% 
        dplyr::pull() %>%
        as.character()
      
      ref_region <- refpop_df %>%
        dplyr::distinct(.data$region) %>%
        dplyr::pull() %>%
        as.character()
      
      ref_year <- refpop_df %>%
        dplyr::distinct(.data$year) %>% 
        dplyr::pull() %>%
        as.character()
      
      ref_ages <- refpop_df %>%
        dplyr::distinct(.data$age) %>% 
        dplyr::pull() %>%
        as.character()
      
      ref_stdpop_ages <- stdpop_df %>%
        dplyr::filter(.data$standard_pop == std_pop &
                        .data$age != "85 - 89" &             #remove options from ESP2013 that are not reflected in datasets
                        .data$age != "90 - 120") %>%
        dplyr::distinct(.data$age) %>% 
        dplyr::pull() %>%
        as.character()
      
      not_found_sex <- used_sex[!(used_sex %in% ref_sex)]
      not_found_region <- used_region[!(used_region %in% ref_region)]
      not_found_year <- used_year[!(used_year %in% ref_year)]
      not_found_ages <- used_ages[!(used_ages %in% ref_ages)]
      
      
      if (sum(length(not_found_ages), length(not_found_region), length(not_found_sex), length(not_found_year)) > 0) {
        rlang::abort(
          paste0(
            "The following population strata defined in the dataframe are not found in the reference population data: ",
            paste(not_found_sex, not_found_region, not_found_year, not_found_sex, collapse = ", ")
          )
        )
      }
      
      ages_without_data <- ref_stdpop_ages[!(ref_stdpop_ages %in% used_ages)]
      
      if (length(ages_without_data) > 0) {
        message(
          paste0(
            "For the following age-groups there were no cases to be found in the dataset. Incidence and PYARs will be set to 0: ",
            paste(ages_without_data, collapse = ", ")
          )
        )
      }
      
      #3a: prepare reference pop data
      
      refpop_df <- refpop_df %>%
        dplyr::filter(.data$sex %in% used_sex & 
                        .data$region %in% used_region &
                        .data$year %in% used_year &
                        .data$age %in% ref_stdpop_ages) 
      
      #enforce truncated standard population option
      
      if(truncate_std_pop == TRUE){
        refpop_df <- refpop_df %>%
          dplyr::filter(.data$age %in% used_ages) 
      }
      
      refpop_df <- refpop_df %>%
        dplyr::mutate(sex = as.character(.data$sex),
                      region = as.character(.data$region),
                      year = as.character(.data$year),
                      age = as.character(.data$age))
      
      
      
      #3b: prepare sircalc_count data
      
      sircalc_count <- sircalc_count %>%
        dplyr::mutate(region = as.character(!!region_var),
                      year = as.character(!!year_var)) %>%
        dplyr::filter(.data$sex %in% used_sex & 
                        .data$region %in% used_region &
                        .data$year %in% used_year)
      
      #3c: join pyars from refpop_df
      sircalc <-  dplyr::full_join(sircalc_count, refpop_df, by = c("sex","region", "year", "age")) %>%
        dplyr::ungroup()
      
      
      #3d: remove unused age-groups
      #enforce truncated standard population option
      if(truncate_std_pop == TRUE){
        removed_ages <- sircalc %>%
          dplyr::filter(is.na(.data$population_pyar)) %>%
          dplyr::distinct(.data$age) %>%
          dplyr::pull()
        
        message(paste("The following age groups have been removed before calculating the age-standardized rate: ",
                      paste(removed_ages, collapse = ", "),"
                      Population proportions of the standard population were redistributed proportionally for the remaining age groups.",
                      std_pop, "was used as standard population."))
      }
      
      sircalc <- sircalc %>%
        dplyr::filter(!is.na(.data$population_pyar)) %>%
        dplyr::select(-!!agegroup_var, -!!region_var, -!!sex_var, -!!year_var)
      
      #3e: fill up observed=0 for empty groups
      
      sircalc <- sircalc %>%
        dplyr::mutate(i_observed = dplyr::case_when(is.na(.data$i_observed) == TRUE ~ 0,
                                                    TRUE ~ .data$i_observed))
      
      #3f: set incidence and pyars to 0 for ages_without_data
      
      if (length(ages_without_data) > 0) {
        sircalc <- sircalc %>%
          dplyr::mutate(population_pyar = dplyr::case_when(.data$age %in% ages_without_data == TRUE ~ 0.000000001,
                                                           TRUE ~ .data$population_pyar))
      }
      
      
      #AN1: calculate rate
      
      sircalc <- sircalc %>%
        dplyr::mutate(
          i_pyar = .data$population_pyar,
          i_abs_ir = .data$i_observed / .data$i_pyar * 100000,
          i_abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$i_observed) / 2) / .data$i_pyar * 100000,
          i_abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$i_observed + 1)) / 2) / .data$i_pyar * 100000
        )
      
      
      #AN2: calculate incremental rates per stratum i and variance per stratum
      
      #2a calculate new group proportions in standard population given that some of the younger age-groups were truncated
      
      sum_group_population <- sircalc %>%
        dplyr::group_by(.data$region, .data$sex, .data$year,!!icdcat_var) %>%
        dplyr::summarize(group_pop_sum = sum(.data$population_n, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$group_pop_sum) %>%
        dplyr::distinct(.data$group_pop_sum) %>%
        dplyr::filter(.data$group_pop_sum > 0) %>%
        dplyr::pull()
      
      #CHK_AN2a sum for group_proportion should be the same across all strata i
      
      if (length(sum_group_population) > 1) {
        rlang::abort(
          paste("Error when recalculating group proportions of standard population. Varying values for sum_group_proportions:",
                paste(sum_group_population, collapse = ", ")
          )
        )
      }
      
      sircalc <- sircalc %>%
        dplyr::mutate(group_proportion_recalc = .data$population_n / sum_group_population)
      
      sircalc <- sircalc %>%
        dplyr::mutate(
          asir_strat = .data$i_abs_ir * .data$group_proportion_recalc,
          #variance calculation based on @brayChapterAgeStandardization2014, p. 114, Poisson distribution; var=sum(i_observed*(group_proportion_recalc / i_pyar)^2)
          i_var_strat = (.data$i_observed * (.data$group_proportion_recalc / .data$i_pyar)^2) * 100000,
          #round very small pyars to 0 for functions that would be negatively affected
          i_pyar0 = dplyr::case_when(.data$i_pyar < 0.0001 ~ NA_real_,
                                     TRUE ~ .data$i_pyar)
        )
      
      #AN2: calculate age-standardized rate
      
      #sum over all age-groups to get age standardized incidence rate
      asir_results <- sircalc %>%
        dplyr::group_by(.data$region, .data$sex, .data$year, !!icdcat_var) %>%
        dplyr::summarize(
          group_observed = sum(.data$i_observed, na.rm = TRUE),
          group_pyar = sum(.data$i_pyar, na.rm = TRUE),
          group_abs_ir = .data$group_observed / .data$group_pyar * 100000,
          group_abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$group_observed) / 2) / .data$group_pyar * 100000,
          group_abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$group_observed + 1)) / 2) / .data$group_pyar * 100000,
          asir = sum(.data$asir_strat, na.rm = TRUE),
          #confidence intervals based on Poisson approximation based on @brayChapterAgeStandardization2014, p. 114, Poisson distribution; var=sum(i_observed*(group_proportion_recalc / i_pyar)^2)
          var_asir = sum(.data$i_var_strat, na.rm = TRUE),
          asir_lci = .data$asir + stats::qnorm(p = alpha / 2) * sqrt(.data$var_asir),
          asir_uci = .data$asir + stats::qnorm(p = 1 - alpha / 2) * sqrt(.data$var_asir),
          #Exact Confidence Intervals based on gamma distribution
          #function adapted from epitools::ageadjust.direct (@aragonEpitoolsEpidemiologyTools2017a), based on Fay and Feuer 2017 @fayConfidenceIntervalsDirectly1997)
          asir_e6 = .data$asir / 100000,
          asir_copy = .data$asir,
          var_asir_gam = sum((.data$group_proportion_recalc ^ 2) * (.data$i_observed /.data$i_pyar ^ 2), na.rm = TRUE),
          asir_lci_gam = stats::qgamma(
            alpha / 2,
            shape = (.data$asir_e6 ^ 2) / .data$var_asir_gam,
            scale = .data$var_asir_gam / .data$asir_e6
          ) * 100000,
          asir_uci_gam = stats::qgamma(
            1 - alpha / 2,
            shape = ((.data$asir_e6 + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE)) ^ 2) / (.data$var_asir_gam + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE) ^ 2),
            scale = (.data$var_asir_gam + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE) ^ 2) / (.data$asir_e6 + max(.data$group_proportion_recalc / .data$i_pyar0, na.rm = TRUE))
          ) * 100000,
          age = paste0("Age standardized by ", std_pop)
        ) %>%
        dplyr::ungroup()
      
      #enforce truncated standard population option
      if(truncate_std_pop == TRUE){
        asir_results <- asir_results %>%
          dplyr::mutate(age = paste0("Age standardized by truncated ", std_pop, ": truncated to ages ", min_age, " - ", max_age))
      }
      
      asir_results <- asir_results %>%
        dplyr::mutate(icdcat = !!icdcat_var) %>%
        dplyr::rename(
          observed = .data$group_observed,
          pyar = .data$group_pyar,
          abs_ir = .data$group_abs_ir,
          abs_ir_lci = .data$group_abs_ir_lci,
          abs_ir_uci = .data$group_abs_ir_uci
        ) %>%
        dplyr::mutate(dplyr::across(.cols = c(.data$asir, .data$asir_copy, .data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci, .data$asir_lci, .data$asir_uci, 
                                              .data$asir_lci_gam, .data$asir_uci_gam), .fns = ~round(.,2))) %>%
        dplyr::mutate(dplyr::across(.cols = .data$pyar, ~ round(.,0))) %>%
        dplyr::select(.data$age, .data$region, .data$sex, .data$year, .data$icdcat, .data$asir, .data$observed, .data$pyar, .data$abs_ir, .data$abs_ir_lci, 
                      .data$abs_ir_uci, .data$asir_copy, .data$asir_lci, .data$asir_lci_gam, .data$asir_uci, .data$asir_uci_gam, .data$asir_e6)
      
      
      
      #AN3: summarizing groups if option summarize_groups is used
      
      if(summarize_groups[1] == "none"){
        return(asir_results)
      } else{
        
        #3a get grouping vars
        
        summarize_groups <- rlang::enquo(summarize_groups)
        
        #CHK 3a check if summarize_groups are present in results_df
        
        sum_var_names <- rlang::eval_tidy(summarize_groups)
        
        not_found <- sum_var_names[!(sum_var_names %in% colnames(asir_results))]
        
        
        if (length(not_found) > 0) {
          rlang::abort(
            paste0(
              "The following variables defined in summarize_groups are not found in the results dataframe: ",
              paste(not_found, collapse = ", ")
            )
          )
        }
        
        #all variables that could be grouping vars
        all_grouping_vars <- c("region", "sex", "year", "icdcat")
        
        #3b remove from grouping vars those who should be summarized
        grouping_vars <- all_grouping_vars[!(all_grouping_vars %in% sum_var_names)]
        
        #3c group with age to make summary of observed, pyrs, group_proportion_recalc
        asir_results_sum_tmp <- sircalc %>%
          dplyr::mutate(icdcat = as.character(!!icdcat_var)) %>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(c(grouping_vars, "age"))), .drop = FALSE) %>%
          dplyr::summarize(
            group_observed = sum(.data$i_observed, na.rm = TRUE),
            group_pyar = sum(.data$i_pyar, na.rm = TRUE),
            group_proportion_recalc = mean(.data$group_proportion_recalc),
            group_pyar0 = sum(.data$i_pyar0, na.rm = TRUE)
          ) %>%
          dplyr::mutate(
            group_abs_ir = .data$group_observed / .data$group_pyar * 100000,
            group_asir_strat = .data$group_abs_ir * .data$group_proportion_recalc,
            group_var_strat = (.data$group_observed * (.data$group_proportion_recalc / .data$group_pyar)^2) * 100000,
            group_pyar0 = dplyr::case_when(.data$group_pyar0 < 0.00001 ~ NA_real_,
                                           TRUE ~ .data$group_pyar0)
          ) %>%
          dplyr::ungroup()
        
        
        #3c get summary by aggregating over age    
        asir_results_sum <- asir_results_sum_tmp%>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_vars)), .drop = FALSE) %>%
          dplyr::summarize(
            asir = sum(.data$group_asir_strat, na.rm = TRUE),
            #variance based on Poisson approximation based on @brayChapterAgeStandardization2014, p. 114, Poisson distribution; var=sum(i_observed*(group_proportion_recalc / i_pyar)^2)
            var_asir = sum(.data$group_var_strat, na.rm = TRUE),
            #Exact Confidence Intervals for ASIR based on gamma distribution
            #function adapted from epitools::ageadjust.direct (@aragonEpitoolsEpidemiologyTools2017a), based on Fay and Feuer 2017 @fayConfidenceIntervalsDirectly1997)
            var_asir_gam = sum((.data$group_proportion_recalc ^ 2) * (.data$group_observed /.data$group_pyar ^ 2), na.rm = TRUE),
            asir_e6 = asir / 100000,
            asir_lci_gam = stats::qgamma(
              alpha / 2,
              shape = (.data$asir_e6 ^ 2) / .data$var_asir_gam,
              scale = .data$var_asir_gam / .data$asir_e6
            ) * 100000,
            asir_uci_gam = stats::qgamma(
              1 - alpha / 2,
              shape = ((.data$asir_e6 + max(.data$group_proportion_recalc / .data$group_pyar0, na.rm = TRUE)) ^ 2) / (.data$var_asir_gam + max(.data$group_proportion_recalc / .data$group_pyar0, na.rm = TRUE) ^ 2),
              scale = (.data$var_asir_gam + max(.data$group_proportion_recalc / .data$group_pyar0, na.rm = TRUE) ^ 2) / (.data$asir_e6 + max(.data$group_proportion_recalc / .data$group_pyar0, na.rm = TRUE))
            ) * 100000,
            sum_group_observed = sum(.data$group_observed, na.rm = TRUE),
            sum_group_pyar = sum(.data$group_pyar, na.rm = TRUE)
          ) %>%
          dplyr::mutate(
            #absolute IR incl. confidence intervals
            sum_group_abs_ir = .data$sum_group_observed / .data$sum_group_pyar * 100000,
            sum_group_abs_ir_lci = (stats::qchisq(p = alpha / 2, df = 2 * .data$sum_group_observed) / 2) / .data$sum_group_pyar * 100000,
            sum_group_abs_ir_uci = (stats::qchisq(p = 1 - alpha / 2, df = 2 * (.data$sum_group_observed + 1)) / 2) / .data$sum_group_pyar * 100000,
            #confidence intervals for ASIR based on Poisson approximation based on @brayChapterAgeStandardization2014, p. 114, Poisson distribution; var=sum(i_observed*(group_proportion_recalc / i_pyar)^2)
            asir_lci = .data$asir + stats::qnorm(p = alpha / 2) * sqrt(.data$var_asir),
            asir_uci = .data$asir + stats::qnorm(p = 1 - alpha / 2) * sqrt(.data$var_asir),
            asir_copy = .data$asir,
            age = paste0("Age standardized by ", std_pop)
          ) %>%
          dplyr::ungroup()
        
        #enforce truncated standard population option
        if(truncate_std_pop == TRUE){
          asir_results_sum <- asir_results_sum %>%
            dplyr::mutate(age = paste0("Age standardized by truncated ", std_pop, ": truncated to ages ", min_age, " - ", max_age))
        }
        
        #add grouping information for summarized variables
        if("region" %in% sum_var_names == TRUE){
          asir_results_sum <- asir_results_sum %>%
            dplyr::mutate(region = paste0("Total - All included regions: ", paste(used_region, collapse = ", ")))
        }
        
        if("sex" %in% sum_var_names == TRUE){
          asir_results_sum <- asir_results_sum %>%
            dplyr::mutate(sex = paste0("Total - All included genders: ", paste(used_sex, collapse = ", ")))
        }
        
        if("year" %in% sum_var_names == TRUE){
          asir_results_sum <- asir_results_sum %>%
            dplyr::mutate(year = paste0("Total - All included years: ", min_year, " - ", max_year))
        }
        
        if("icdcat" %in% sum_var_names == TRUE){
          asir_results_sum <- asir_results_sum %>%
            dplyr::mutate(icdcat = paste0("Total - All included ICD categories: ", paste(used_icdcat, collapse = ", ")))
        }
        
        asir_results_sum <- asir_results_sum %>%
          dplyr::rename(
            observed = .data$sum_group_observed,
            pyar = .data$sum_group_pyar,
            abs_ir = .data$sum_group_abs_ir,
            abs_ir_lci = .data$sum_group_abs_ir_lci,
            abs_ir_uci = .data$sum_group_abs_ir_uci
          ) %>%
          dplyr::mutate(dplyr::across(.cols = c(.data$asir, .data$asir_copy, .data$abs_ir, .data$abs_ir_lci, .data$abs_ir_uci, .data$asir_lci, .data$asir_uci, .data$asir_lci_gam, 
                                                .data$asir_uci_gam), .fns = ~ round(.,2))) %>%
          dplyr::mutate(dplyr::across(.cols = .data$pyar, ~ round(.,0))) %>%
          dplyr::select(.data$age, .data$region, .data$sex, .data$year, .data$icdcat, .data$asir, .data$observed, .data$pyar, .data$abs_ir, .data$abs_ir_lci, 
                        .data$abs_ir_uci, .data$asir_copy, .data$asir_lci, .data$asir_lci_gam, .data$asir_uci, .data$asir_uci_gam, .data$asir_e6)
        
        return(asir_results_sum)
      }
      
    }
  }


