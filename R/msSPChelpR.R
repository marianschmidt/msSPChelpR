#' msSPChelpR
#'
#' R helper functions for Second Primary Cancer analyses
#'
#' @section Main functions:
#' 
#' Functions include asir, ir_crosstab, pat_status, reshape, sir_byfutime and summarize_sir_results.
#' 
#' @section Package context:
#' This package follows the principles of the "tidyverse".
#'
#'
#' @docType package
#' @name msSPChelpR
#' @keywords internal
NULL
## quiets warnings of R CMD check standard objects that appear in function
if(getRversion() >= "2.15.1")  utils::globalVariables(c(":=", #data.table and tidyverse programming
                                                        ".", 
                                                        ".N", 
                                                        "data", 
                                                        "counter", 
                                                        "standard_population", 
                                                        "population", 
                                                        "rates", 
                                                        "p_datedeath_orig",
                                                        "status_var_orig",
                                                        "where", #temporary fix because tidyverse::where is not exported,
                                                        "age", #caused by sir_byfutime_tt (.SD$ predicate seems not to work)
                                                        "fu_time", 
                                                        "futimegroup",
                                                        "i_expected",
                                                        "i_observed",
                                                        "i_pyar",
                                                        "incidence_cases",
                                                        "incidence_crude_rate",
                                                        "max_pyar",
                                                        "min_pyar",
                                                        "n_base",
                                                        "population_n_per_year",
                                                        "population_pyar",
                                                        "pyar",
                                                        "region",
                                                        "sex",
                                                        "sir",
                                                        "sir_lci",
                                                        "sir_uci",
                                                        "t_site",
                                                        "t_site_orig",
                                                        "race",
                                                        "yvar_name",
                                                        "yvar_label",
                                                        "xvar_name",
                                                        "xvar_label",
                                                        "year",
                                                        "observed",
                                                        "ref_inc_cases", 
                                                        "ref_inc_crude_rate",
                                                        "ref_population_pyar",
                                                        "expected",
                                                        "group_pyar",
                                                        "group_observed",
                                                        "group_expected",
                                                        "group_ref_inc_cases",
                                                        "group_incidence_crude_rate", 
                                                        "group_ref_population_pyar",
                                                        "group_n_base"
                                                        )) 
release_questions <- function() {
  c(
    "Is the version number updated in DESCRIPTION, NEWS, and README?"
  )
}


# #to avoid problems at rhub check use the following code
# rhub::check(platform="windows-x86_64-devel", env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))
# rhub::check_for_cran(env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))


# previous_checks <- rhub::list_my_checks(email = "marian.eberl@tum.de")
# previous_checks[[3]][[15]]2
