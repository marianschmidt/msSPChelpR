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