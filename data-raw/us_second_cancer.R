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
              registry = sample(c(rep(1501, 1), rep(1502, 1), rep(1520, 1), rep(1521, 1), rep(1522, 1), rep(1523, 1),
                                  rep(1525, 1), rep(1526, 1), rep(1527, 1), rep(1529, 1), rep(1531, 1), rep(1535, 1),
                                  rep(1537, 1), rep(1541, 1), rep(1542, 1), rep(1543, 1), rep(1544, 1), rep(1547, 1)), 
                                size = nn, replace = TRUE),
              #age according to overall age-distribution in 2010
              fc_agegroup = sample(c(rep("00 - 04", 20),
                                     rep("05 - 09", 20),
                                     rep("10 - 14", 20),
                                     rep("15 - 19", 22),
                                     rep("20 - 24", 21),
                                     rep("25 - 29", 21),
                                     rep("30 - 34", 19),
                                     rep("35 - 39", 20),
                                     rep("40 - 44", 20),
                                     rep("45 - 49", 22),
                                     rep("50 - 54", 22),
                                     rep("55 - 59", 19),
                                     rep("60 - 64", 16),
                                     rep("65 - 69", 12),
                                     rep("70 - 74", 9),
                                     rep("75 - 79", 7),
                                     rep("80 - 84", 5),
                                     rep("85 - 120", 5))
                                   ,size = nn, replace = TRUE))

tumors <- tibble(fake_id = as.character(sample(spc$fake_id, size = nn * 1.14, replace = TRUE)),
              #Site of  cancer FC, weighted by approximated relative incidence
              site_icd = sample(c(rep("C00",  1), 
                                     rep("C01",  2), 
                                     rep("C02",  1), 
                                     rep("C03",  1), 
                                     rep("C04",  1), 
                                     rep("C05",  1), 
                                     rep("C06",  1), 
                                     rep("C07",  1), 
                                     rep("C08",  1), 
                                     rep("C09",  2), 
                                     rep("C10",  1), 
                                     rep("C11",  1), 
                                     rep("C12",  1), 
                                     rep("C13",  1), 
                                     rep("C14",  1), 
                                     rep("C15",  6), 
                                     rep("C16", 10), 
                                     rep("C17",  3), 
                                     rep("C18", 31), 
                                     rep("C19",  2), 
                                     rep("C20",  8), 
                                     rep("C21",  2), 
                                     rep("C22", 11), 
                                     rep("C23",  1), 
                                     rep("C24",  2), 
                                     rep("C25", 18), 
                                     rep("C26",  1), 
                                     rep("C30",  1), 
                                     rep("C31",  1), 
                                     rep("C32",  4), 
                                     rep("C33",  1), 
                                     rep("C34", 91), 
                                     rep("C37",  1), 
                                     rep("C38",  1), 
                                     rep("C39",  1), 
                                     rep("C40",  1), 
                                     rep("C41",  1), 
                                     rep("C43",  1), 
                                     rep("C44", 30), 
                                     rep("C45",  1), 
                                     rep("C46",  1), 
                                     rep("C47",  1), 
                                     rep("C48",  1), 
                                     rep("C49",  3), 
                                     rep("C50", 75), 
                                     rep("C51",  1), 
                                     rep("C52",  1), 
                                     rep("C53",  1), 
                                     rep("C54", 14), 
                                     rep("C55",  1), 
                                     rep("C56",  6), 
                                     rep("C57",  1), 
                                     rep("C58",  1), 
                                     rep("C60",  1), 
                                     rep("C61", 83), 
                                     rep("C62",  1), 
                                     rep("C63",  1), 
                                     rep("C64", 19), 
                                     rep("C65",  1), 
                                     rep("C66",  1), 
                                     rep("C67", 30), 
                                     rep("C68",  1), 
                                     rep("C69",  1), 
                                     rep("C70",  1), 
                                     rep("C71",  6), 
                                     rep("C72",  1), 
                                     rep("C73",  7), 
                                     rep("C74",  1), 
                                     rep("C75",  1), 
                                     rep("C76",  1), 
                                     rep("C77", 16), 
                                     rep("C78",  1), 
                                     rep("C79",  1), 
                                     rep("C80",  8))
                                   , size = nn*1.14, replace = TRUE))
              

tumors2 <- tumors %>%
  mutate(t_year = 1970 + sample(1:40, size = n(), replace = TRUE),
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
  tidytable::mutate.(SEQ_NUM := as.integer(tidytable::row_number.()), .by = fake_id)

#life_var
#birthdat_var
#lifedat_var
#lifedatmin_var


cancer_pre <- tumors2 %>% 
  as_tibble() %>%
  left_join(., spc, by = "fake_id")
  

us_second_cancer <- cancer_pre %>%
  select(-t_year, -t_month)

usethis::use_data(us_second_cancer, overwrite = TRUE)
