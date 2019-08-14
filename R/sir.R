
#' Calculate standardized incidence ratios with costum grouping variables
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
#' @param refrates_df df where reference rate from general population are defined. It is assumed that refrates_df has the columns 
#'                  "region" for region, "sex" for gender, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "incidence_crude_rate" for incidence rate in the respective age/gender/year cohort.
#'                  refrates_df must use the same category coding of age, sex, region, year and icdcat as agegroup_var, sex_var, region_var, year_var and icdcat_var. 
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


sir <-
  function(df,
           dattype = "zfkd",
           std_pop = "ESP2013",
           truncate_std_pop = FALSE,
           futime_src = "refpop",
           summarize_groups = "none",
           count_var,
           stdpop_df = standard_population, #optional for indirect standardization
           refpop_df = population,           #optional for indirect standardization
           refrates_df = rates,
           region_var = NULL,
           agegroup_var = NULL,
           sex_var = NULL,
           year_var = NULL,
           icdcat_var = NULL,
           futime_var = NULL,
           pyar_var = NULL,
           alpha = 0.05) {
  
    return(sir_result)
    
    }