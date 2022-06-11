
#' Calculate Ratio of two SIRs or SMRs 
#' 
#' Calculate ratio of two SIRs by providing observed and expected counts to \code{sir_ratio}
#' The related functions \code{sir_ratio_lci} and \code{sir_ratio_uci} can also calculate lower and upper estimates of the confidence interval
#' Calculations are based on formulas suggested by Breslow & Day 1987
#'
#' @param o1 observed count for SIR 1
#' @param o2 observed count for SIR 2
#' @param e1 expected count for SIR 1
#' @param e2 observed count for SIR 2
#' @return num numeric value of SIR / SMR estimate
#' @export
#' @references Breslow NE, Day NE. Statistical Methods in Cancer Research Volume II: The Design and Analysis of Cohort Studies. Lyon, France: IARC; 1987. (IARC Scientific Publications IARC Scientific Publications No. 82). Available from: http://publications.iarc.fr/Book-And-Report-Series/Iarc-Scientific-Publications/Statistical-Methods-In-Cancer-Research-Volume-II-The-Design-And-Analysis-Of-Cohort-Studies-1986

#' @examples 
#' #provide the two expected and observed count to get the ratio of SIRs/SMRs
#' msSPChelpR::sir_ratio(o1 = 2140, o2 = 3158, e1 = 1993, e2 = 2123)
#'
#' #calculate lower confidence limit
#' msSPChelpR::sir_ratio_lci(o1 = 2140, o2 = 3158, e1 = 1993, e2 = 2123, alpha = 0.05)
#'
#' #calculate upper confidence limit
#' msSPChelpR::sir_ratio_uci(o1 = 2140, o2 = 3158, e1 = 1993, e2 = 2123, alpha = 0.05)
#' 
#' #functions can be easily used inside dplyr::mutate function
#' library(dplyr)
#' test_df <- data.frame(sir_oth = c(1.07, 1.36, 0.96), 
#'                   sir_smo = c(1.49, 1.81, 1.41),
#'                   observed_oth = c(2140, 748, 1392),
#'                   expected_oth = c(1993, 550, 1443),
#'                   observed_smo = c(3158, 744, 2414),
#'                   expected_smo = c(2123, 412, 1711))
#' 
#' test_df %>%
#'   mutate(smo_ratio = sir_ratio(observed_oth, observed_smo, expected_oth, expected_smo),
#'          smo_ratio_lci = sir_ratio_lci(observed_oth, observed_smo, expected_oth, expected_smo),
#'          smo_ratio_uci = sir_ratio_uci(observed_oth, observed_smo, expected_oth, expected_smo))

sir_ratio <- function(o1, o2, e1, e2){
  #calculate SIR ratio (Breslow Day, p. 94, formula 3.8)
  (o2 * e1) / (o1 * e2)
}

#' @rdname sir_ratio
#' @param alpha alpha significance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @export
#' 
sir_ratio_lci <- function(o1, o2, e1, e2, alpha = 0.05){
  
  #calculate pi_lci (Breslow Day, p.95, formula 3.9)
  pi_lci <- o2 / (o2 + (o1 + 1) * stats::qf(1 - alpha / 2, 2 * o1 + 2, 2 * o2))  
  
  #transform pi_lci (Breslow Day, p.95, formula 3.9) to psi_lci (Breslow Day p94, formula 3.8)
  (pi_lci * e1) / ((1 - pi_lci) * e2)
}

#' @rdname sir_ratio
#' @param alpha alpha significance level for confidence interval calculations. Default is alpha = 0.05 which will give 95 percent confidence intervals.
#' @export

sir_ratio_uci <- function(o1, o2, e1, e2, alpha = 0.05){
  
  #calculate pi_uci (Breslow Day, p.95, formula 3.9)
  pi_uci <- ((o2 + 1) * stats::qf(1 - alpha / 2, 2 * o2 + 2, 2 * o1)) / #enumerator
    (o1 + (o2 + 1) * stats::qf(1 - alpha / 2, 2 * o2 + 2, 2 * o1)) #denominator
  
  #transform pi_uci (Breslow Day, p.95, formula 3.9) to psi_uci (Breslow Day p. 94, formula 3.8)
  (pi_uci * e1) / ((1 - pi_uci) * e2)
  
}
