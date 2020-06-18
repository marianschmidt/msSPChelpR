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
                                                        "stat_var_orig",
                                                        "where" #temporary fix because tidyverse::where is not exported
                                                        )) 