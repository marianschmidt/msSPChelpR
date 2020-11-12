test_that("custom variable name for status_var works", {
  data("us_second_cancer")
  
  t1_ti <- us_second_cancer %>% 
    pat_status(fu_end = "2017-12-31", status_var = "other_var_name", dattype = "zfkd")
  
  expect_message(reshape_wide(us_second_cancer, case_id_var = "fake_id", time_id_var = "SEQ_NUM", timevar_max = 2, datsize = 100), 
                 "Wide dataset is limited to  2  cases per id")
  expect_true((ncol(t1_ti5) > ncol(t1_ti2)))
})