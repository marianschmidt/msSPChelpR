## code to prepare `us_refrates_icd2` dataset goes here

library(tidyverse)

set.seed(2048)

pre_refrates <- tibble(
  t_site = c("C34"),
  region = c("SEER Reg 01 - San Francisco-Oakland SMSA"),
  year = c("1990 - 1994"),
  sex = c("Male"),
  age = c("30 - 34"),
  comment = c("including DCO"),
  race = c("Black"),
  incidence_cases = c(5),
  population_pyar = c(15000)
)

site_levels <- c("C14", "C18", "C21", "C34", "C44",
                 "C50", "C54", "C61", "C64", "C80")
region_levels <- c("SEER Reg 01 - San Francisco-Oakland SMSA",
                   "SEER Reg 02 - Connecticut",
                   "SEER Reg 20 - Detroit (Metropolitan)",
                   "SEER Reg 21 - Hawaii",
                   "SEER Reg 22 - Iowa")

year_levels <- c("1990 - 1994", "1995 - 1999", "2000 - 2004",
                 "2005 - 2009", "2010 - 2014", "2015 - 2019")

sex_levels <- c("Male", "Female")

age_levels <- c("00 - 04", "05 - 09", "10 - 14", "15 - 19",
                "20 - 24", "25 - 29", "30 - 34", "35 - 39",
                "40 - 44", "45 - 49", "50 - 54", "55 - 59",
                "60 - 64", "65 - 69", "70 - 74", "75 - 79",
                "80 - 84", "85 - 120")

race_levels <- c("White", "Black", "Other")

#set n for checks

n_t_site <- length(site_levels)
n_regyears <- length(region_levels) * length(year_levels)
n_sex <- length(sex_levels)
n_age <- length(age_levels)
n_race <- length(race_levels)


#create all combinations of site, region, year, sex, age, race

pre_refrates <- pre_refrates %>%
  complete(t_site = site_levels, region = region_levels, year = year_levels, sex = sex_levels, 
           age = age_levels, race = race_levels)

pre_refrates <- pre_refrates %>%
  mutate(incidence_cases = sample(c(0:25), size = n(), replace = TRUE),
         population_pyar = sample(c(5000:25000), size = n(), replace = TRUE))

#calculate and bind totals
#race total
sum_rate_race <- pre_refrates %>%
  mutate(race_group = "Total - All races") %>%
  filter(race_group != "") %>%
  group_by(race_group, t_site, region, year, sex, age) %>%
  summarise(incidence_cases = sum(incidence_cases),
            population_pyar = sum(population_pyar)) %>%
  rename(race = race_group) %>%
  ungroup()

#check
if(nrow(sum_rate_race) != (1 * n_age * n_regyears * n_sex * n_t_site)){
  warning("Check calculations. Number of rows not matching")
}

pre_refrates2 <- bind_rows(pre_refrates, sum_rate_race) 

#sex groups
sum_rate_sex <- pre_refrates2 %>%
  mutate(sex_group = "Total - All genders") %>%
  filter(sex_group != "") %>%
  group_by(sex_group, t_site, region, year, age, race) %>%
  summarise(incidence_cases = sum(incidence_cases),
            population_pyar = sum(population_pyar)) %>%
  rename(sex = sex_group) %>%
  ungroup()

#check
if(nrow(sum_rate_sex) != ((n_race + 1) * n_age * n_regyears * 1 * n_t_site)){
  warning("Check calculations. Number of rows not matching")
}

#merge with existing data
pre_refrates2 <- bind_rows(pre_refrates2, sum_rate_sex)



us_refrates_icd2 <- pre_refrates2 %>%
  mutate(population_n_per_year = case_when(stringr::str_length(year) == 4 ~ as.numeric(population_pyar),
                                           stringr::str_length(year) > 4 ~ as.numeric(population_pyar) / 5,
                                           TRUE ~ NA_real_)) %>%
  mutate(incidence_crude_rate = incidence_cases / population_pyar * 100000) %>%
  mutate(region = as_factor(region),
         sex = as.factor(sex))


rm(pre_refrates, pre_refrates2, sum_rate_race, sum_rate_sex,
   age_levels, race_levels, region_levels, sex_levels, site_levels, year_levels)

usethis::use_data(us_refrates_icd2, overwrite = TRUE)
