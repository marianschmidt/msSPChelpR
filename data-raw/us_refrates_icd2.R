## code to prepare `us_refrates_icd2` dataset goes here

pre_refrates <- tibble(
  t_site = c("C34"),
  region = c("SEER Reg 01 - San Francisco-Oadkland SMSA"),
  year = c(1975),
  sex = c("Male"),
  age = c("30"),
  race = c("Black"),
  incidence_cases = c(5),
  population_pyar = c(15000)
)

us_refrates_icd2 <- pre_refrates

usethis::use_data(us_refrates_icd2, overwrite = TRUE)
