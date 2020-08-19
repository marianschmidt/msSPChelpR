## code to prepare `us_second_cancer` dataset goes here
library(tidyverse)
library(lubridate)

nn <- 100000

set.seed(2048)

spc <- tibble(fake_id = as.character(sample(nn:(nn*10), size = nn, replace = FALSE)), #using character variable as ID
              #sex sampled 50:50
              sex = sample(c("Male", "Female"), size = nn, replace = TRUE),
              #race (76% White, 14% Black, 10% Other)
              race = sample(c(rep("White", 76), rep("Black", 14), rep("Other", 11)), size = nn, replace = TRUE),
              #SEER registry (for now using no weight 1 for all)
              registry = sample(c(rep("SEER Reg 01 - San Francisco-Oakland SMSA", 3),
                                  rep("SEER Reg 20 - Detroit (Metropolitan)", 3),
                                  rep("SEER Reg 02 - Connecticut", 2),
                                  rep("SEER Reg 21 - Hawaii", 1),
                                  rep("SEER Reg 22 - Iowa", 3)
                                  ), 
                                size = nn, replace = TRUE),
              #age
              fc_age = sample(c(rep(0:20, 5),
                                rep(21:40, 15),
                                rep(41:59, 20),
                                rep(60:64, 25),
                                rep(65:69, 30),
                                rep(70:74, 19),
                                rep(75:79, 17),
                                rep(80:84, 10),
                                rep(85:89, 6),
                                rep(90:100, 1),
                                rep(100:105, 1))
                                   ,size = nn, replace = TRUE),
              #grouping of fc_age
              fc_agegroup = case_when(fc_age < 5                  ~ "00 - 04" ,
                                      fc_age >= 5  & fc_age < 10  ~ "05 - 09" ,
                                      fc_age >= 10 & fc_age < 15  ~ "10 - 14" ,
                                      fc_age >= 15 & fc_age < 20  ~ "15 - 19" ,
                                      fc_age >= 20 & fc_age < 25  ~ "20 - 24" ,
                                      fc_age >= 25 & fc_age < 30  ~ "25 - 29" ,
                                      fc_age >= 30 & fc_age < 35  ~ "30 - 34" ,
                                      fc_age >= 35 & fc_age < 40  ~ "35 - 39" ,
                                      fc_age >= 40 & fc_age < 45  ~ "40 - 44" ,
                                      fc_age >= 45 & fc_age < 50  ~ "45 - 49" ,
                                      fc_age >= 50 & fc_age < 55  ~ "50 - 54" ,
                                      fc_age >= 55 & fc_age < 60  ~ "55 - 59" ,
                                      fc_age >= 60 & fc_age < 65  ~ "60 - 64" ,
                                      fc_age >= 65 & fc_age < 70  ~ "65 - 69" ,
                                      fc_age >= 70 & fc_age < 75  ~ "70 - 74" ,
                                      fc_age >= 75 & fc_age < 80  ~ "75 - 79" ,
                                      fc_age >= 80 & fc_age < 85  ~ "80 - 84" ,
                                      fc_age >= 85                ~ "85 - 120",
                                      TRUE ~ NA_character_),
              #tmp var for creating missings
              miss = sample(c(1, rep(0, 500)), size = nn, replace = TRUE),
              #tmp var for random month
              ran_mon = sample(1:12, size = nn, replace = TRUE))

tumors <- tibble(fake_id = as.character(sample(spc$fake_id, size = nn * 1.14, replace = TRUE)),
              #Site of  cancer FC, weighted by approximated relative incidence
              t_site_icd = sample(c( rep("C14",  1), 
                                     rep("C18", 31), 
                                     rep("C21",  2), 
                                     rep("C34", 91), 
                                     rep("C44", 30), 
                                     rep("C50", 75), 
                                     rep("C54", 14), 
                                     rep("C61", 83), 
                                     rep("C64", 19), 
                                     rep("C80",  8))
                                   , size = nn*1.14, replace = TRUE))
              

tumors2 <- tumors %>%
  mutate(t_year = 1990 + sample(1:29, size = n(), replace = TRUE),
         t_month = sample(1:12, size = n(), replace = TRUE),
         t_datediag = lubridate::make_date(year = t_year, month = t_month, day = 15),
         t_yeardiag = case_when(year(t_datediag) >= 1970 &  year(t_datediag) < 1975  ~ "1970 - 1974",
                                year(t_datediag) >= 1975 &  year(t_datediag) < 1980  ~ "1975 - 1979",   
                                year(t_datediag) >= 2015 &  year(t_datediag) < 2020  ~ "2015 - 2019",
                                year(t_datediag) >= 1980 &  year(t_datediag) < 1985  ~ "1980 - 1984",
                                year(t_datediag) >= 1985 &  year(t_datediag) < 1990  ~ "1985 - 1989",
                                year(t_datediag) >= 1990 &  year(t_datediag) < 1995  ~ "1990 - 1994",
                                year(t_datediag) >= 1995 &  year(t_datediag) < 2000  ~ "1995 - 1999",
                                year(t_datediag) >= 2000 &  year(t_datediag) < 2005  ~ "2000 - 2004",
                                year(t_datediag) >= 2005 &  year(t_datediag) < 2010  ~ "2005 - 2009",
                                year(t_datediag) >= 2010 &  year(t_datediag) < 2015  ~ "2010 - 2014",
                                TRUE ~ NA_character_),
         t_dco = sample(c(rep("DCO case", 1), rep("histology", 9)), size = n(), replace = TRUE)) %>%
  #calculate new renumbered variable #group by case_id_var
  arrange(fake_id, t_datediag) %>%
  tidytable::mutate.(SEQ_NUM := as.integer(tidytable::row_number.()), .by = fake_id)

cancer_pre <- tumors2 %>% 
  as_tibble() %>%
  left_join(., spc, by = "fake_id") %>%
  arrange(fake_id, SEQ_NUM)
  

us_second_cancer <- cancer_pre %>%
  #year of birth (first cancer diagnosis year minus age)
  mutate(p_yeardob = case_when(SEQ_NUM == 1 ~ t_year - fc_age, TRUE ~ NA_real_)) %>%
  fill(p_yeardob) %>%
  #calculate year of death by taking year of last SEQ_NUM and adding a random number of years
  group_by(fake_id) %>%
  mutate(p_lastdiag = dplyr::last(t_datediag),
         p_lastyear = dplyr::last(t_year)) %>%
  ungroup() %>%
  mutate(p_yeardod = case_when(SEQ_NUM == 1 ~ p_lastyear + sample(0:25, size = n(), replace = TRUE),
                               TRUE ~ NA_real_))%>%
  fill(p_yeardod) %>%
  #add random missings to date of death
  mutate(p_yeardod = case_when(miss == 0 ~ p_yeardod,
                               TRUE ~ NA_real_)) %>%
  #make dates of years
  mutate(
    datebirth = lubridate::make_date(year = p_yeardob, month = 01, day = 01),
    datedeath = lubridate::make_date(year = p_yeardod, month = ran_mon, day = 01),
    #calculate life status for year 2019
    p_alive = case_when(p_yeardod > 2019 ~ "Alive",
                        TRUE ~ "Dead"),
    #set date of death missing if Alive
    datedeath = case_when(p_alive == "Dead" ~ datedeath,
                            TRUE ~ NA_Date_)) %>%
  #minimum year of death for missing dod
  mutate(p_dodmin = case_when(p_alive == "Dead" & is.na(datedeath)  ~ p_lastdiag,
                                       TRUE ~ NA_Date_)) %>%
  #sort columns
  select(fake_id, SEQ_NUM, registry, sex, race, datebirth, t_datediag, t_site_icd, t_dco, fc_age, datedeath, 
         p_alive, p_dodmin, fc_agegroup, t_yeardiag,
         -t_year, -t_month, -p_yeardob, -p_yeardod, -p_lastyear, -p_lastdiag, -miss, -ran_mon)


usethis::use_data(us_second_cancer, overwrite = TRUE)
