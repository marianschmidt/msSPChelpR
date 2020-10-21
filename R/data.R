#' US Second Cancer
#'
#' Synthetic dataset of patients with cancer to demonstrate package functions
#'
#' @format A data frame with the following variables:
#' \describe{
#' \item{\code{fake_id}}{ID of patient}
#' \item{\code{SEQ_NUM}}{Original tumor sequence}
#' \item{\code{registry}}{SEER registry}
#' \item{\code{sex}}{Biological sex of patient}
#' \item{\code{race}}{Race}
#' \item{\code{datebirth}}{Date of birth}
#' \item{\code{t_datediag}}{Date of diagnosis of tumor}
#' \item{\code{t_site_icd}}{Primary site of tumor in ICD-O coding}
#' \item{\code{t_dco}}{Tumor diagnosis is based on Death Certificate only}
#' \item{\code{fc_age}}{Age at first primary cancer in years}
#' \item{\code{datedeath}}{Date of death}
#' \item{\code{p_alive}}{Patient alive at end of follow-up 2019}
#' \item{\code{p_dodmin}}{Minimum Date of Death if datedeath is missing}
#' \item{\code{fc_agegroup}}{Age group of first cancer diagnosis}
#' \item{\code{t_yeardiag}}{Time period of diagnosis of tumor}
#' }
#'
#'
"us_second_cancer"

#' US Reference Rates for Cancer using ICD-O 2digit code for cancer site
#'
#' Synthetic dataset of reference incidence rates for the US population to demonstrate package functions
#'
#' @format A data frame with the following variables:
#' \describe{
#' \item{\code{t_site}}{Tumor Site}
#' \item{\code{region}}{Region / Region groups}
#' \item{\code{year}}{Year / Periods}
#' \item{\code{sex}}{Sex}
#' \item{\code{age}}{Age / Age groups}
#' \item{\code{race}}{Race}
#' \item{\code{comment}}{Comment}
#' \item{\code{incidence_cases}}{Incident Cases (raw count)}
#' \item{\code{incidence_crude_rate}}{Incidence Rate (crude rate)}
#' \item{\code{population_pyar}}{Population Years used for rate calculation (PYAR)}
#' \item{\code{population_n_per_year}}{Absolute Population number used for rate calculation (PYAR / 5 years)}
#' }
#'
#'
"us_refrates_icd2"

#' Standard Populations
#'
#' Dataset that contains different standard populations needed to run some package functions
#'
#' @format A data frame with the following variables:
#' \describe{
#' \item{\code{standard_pop}}{Standard Population}
#' \item{\code{sex}}{Sex}
#' \item{\code{age}}{Age group}
#' \item{\code{population_n}}{Absolute Population number in standard population age group}
#' \item{\code{group_proportion}}{Proportion of age-group in gender-specific total population}
#' }
#'
#'
"standard_population"


#' US Populations
#'
#' Dataset that contains different standard populations needed to run some package functions
#'
#' @format A data frame with the following variables:
#' \describe{
#' \item{\code{region}}{Region / Registry}
#' \item{\code{year}}{Year group}
#' \item{\code{sex}}{Sex}
#' \item{\code{age}}{Age group}
#' \item{\code{race}}{Race}
#' \item{\code{population_pyar}}{Population Years used for rate calculation (PYAR)}
#' \item{\code{population_n_per_year}}{Absolute Population in single years or periods (PYAR / 5 years)]}
#' }
#'
#'
"population_us"
