
#' Create variable for groups of malignant neoplasms considered to be histologically 'different' for the purpose of defining multiple tumors, ICD-O-3 
#'
#' @param df dataframe in long or wide format
#' @param hist_var variable in df that contains first 4 digits of tumor histology (without behavior)
#' @param new_var_hist Name of the newly calculated variable for histology groups. Default is t_histgroupiarc.
#' @param version Version of ICD-O-3 classification used.  Can be either "3.0" for 2000 publication, "3.1" for 2013 first revision or "3.2" for 2019 second revision.
#'                Default is `version = "3.1"` for ICD-O-3 revision 1, released 2013.
#' @return df
#' @export
#' @examples 
#' #load sample data
#' data("us_second_cancer")
#' 
#' us_second_cancer %>% 
#'    msSPChelpR::histgroup_iarc(., hist_var = t_hist) %>%
#'    dplyr::select(fake_id, t_hist, t_histgroupiarc) 
#'                 


histgroup_iarc <- function(df, hist_var, new_var_hist = t_histgroupiarc, version = "3.1"){
  
  
  #check for valid revision used
  if (!as.character(version) %in% c("3.0", "3.1", "3.2")) {
    rlang::abort(
      c("You have specified an invalid `version` parameter",
        "x" = paste0("Version ", version, " is not valid."),
        "i" = "`version` must be either \"3.0\" for ICD-O-3 (2000), \"3.1\" for ICD-O-3 revision 1 (2013), or \"3.2\" for ICD-O-3 revision 2 (2019)."
      )
    )
  }
  
  if(as.character(version) == "3.0"){
    
    #Histologically different groups 
    #Source: Table 25 Fritz AG, Percy C, Jack A, Shanmugaratnam K, Sobin L, Parkin DM, et al., 
    #        editors. International classification of diseases for oncology: ICD-O [Internet]. 
    #        First Revision. Geneva: World Health Organization; 2000 [cited 2023 Jun 14]. 
    #          Available from: https://www.who.int/publications/i/item/international-classification-of-diseases-for-oncology
    
    
    df <- df %>%
      tidytable::mutate(new_var := tidytable::case_when(
        as.numeric({{hist_var}}) >= 8050 & as.numeric({{hist_var}}) <=  8084 ~ 1,
        as.numeric({{hist_var}}) >= 8120 & as.numeric({{hist_var}}) <=  8131 ~ 1,
        as.numeric({{hist_var}}) >= 8090 & as.numeric({{hist_var}}) <=  8110 ~ 2,
        as.numeric({{hist_var}}) >= 8140 & as.numeric({{hist_var}}) <=  8149 ~ 3,
        as.numeric({{hist_var}}) >= 8160 & as.numeric({{hist_var}}) <=  8162 ~ 3,
        as.numeric({{hist_var}}) >= 8190 & as.numeric({{hist_var}}) <=  8221 ~ 3,
        as.numeric({{hist_var}}) >= 8260 & as.numeric({{hist_var}}) <=  8337 ~ 3,
        as.numeric({{hist_var}}) >= 8350 & as.numeric({{hist_var}}) <=  8551 ~ 3,
        as.numeric({{hist_var}}) >= 8570 & as.numeric({{hist_var}}) <=  8576 ~ 3,
        as.numeric({{hist_var}}) >= 8940 & as.numeric({{hist_var}}) <=  8941 ~ 3,
        as.numeric({{hist_var}}) >= 8030 & as.numeric({{hist_var}}) <=  8046 ~ 4,
        as.numeric({{hist_var}}) >= 8150 & as.numeric({{hist_var}}) <=  8157 ~ 4,
        as.numeric({{hist_var}}) >= 8170 & as.numeric({{hist_var}}) <=  8180 ~ 4,
        as.numeric({{hist_var}}) >= 8230 & as.numeric({{hist_var}}) <=  8255 ~ 4,
        as.numeric({{hist_var}}) >= 8340 & as.numeric({{hist_var}}) <=  8347 ~ 4,
        as.numeric({{hist_var}}) >= 8560 & as.numeric({{hist_var}}) <=  8562 ~ 4,
        as.numeric({{hist_var}}) >= 8580 & as.numeric({{hist_var}}) <=  8671 ~ 4,
        as.numeric({{hist_var}}) >= 8010 & as.numeric({{hist_var}}) <=  8015 ~ 5,
        as.numeric({{hist_var}}) >= 8020 & as.numeric({{hist_var}}) <=  8022 ~ 5,
        as.numeric({{hist_var}}) >= 8680 & as.numeric({{hist_var}}) <=  8713 ~ 6,
        as.numeric({{hist_var}}) >= 8800 & as.numeric({{hist_var}}) <=  8921 ~ 6,
        as.numeric({{hist_var}}) >= 8990 & as.numeric({{hist_var}}) <=  8991 ~ 6,
        as.numeric({{hist_var}}) >= 9040 & as.numeric({{hist_var}}) <=  9044 ~ 6,
        as.numeric({{hist_var}}) >= 9120 & as.numeric({{hist_var}}) <=  9125 ~ 6,
        as.numeric({{hist_var}}) >= 9130 & as.numeric({{hist_var}}) <=  9136 ~ 6,
        as.numeric({{hist_var}}) >= 9141 & as.numeric({{hist_var}}) <=  9252 ~ 6,
        as.numeric({{hist_var}}) >= 9370 & as.numeric({{hist_var}}) <=  9373 ~ 6,
        as.numeric({{hist_var}}) >= 9540 & as.numeric({{hist_var}}) <=  9582 ~ 6,
        as.numeric({{hist_var}}) >= 9590 & as.numeric({{hist_var}}) <=  9729 ~ 7,
        as.numeric({{hist_var}}) >= 9800 & as.numeric({{hist_var}}) <=  9949 ~ 8,
        as.numeric({{hist_var}}) >= 9950 & as.numeric({{hist_var}}) <=  9950 ~ 8,
        as.numeric({{hist_var}}) >= 9960 & as.numeric({{hist_var}}) <=  9964 ~ 8,
        as.numeric({{hist_var}}) >= 9980 & as.numeric({{hist_var}}) <=  9989 ~ 8,
        as.numeric({{hist_var}}) >= 9140 & as.numeric({{hist_var}}) <=  9140 ~ 9,
        as.numeric({{hist_var}}) >= 9050 & as.numeric({{hist_var}}) <=  9055 ~ 10,
        as.numeric({{hist_var}}) >= 8720 & as.numeric({{hist_var}}) <=  8790 ~ 11,
        as.numeric({{hist_var}}) >= 8930 & as.numeric({{hist_var}}) <=  8936 ~ 11,
        as.numeric({{hist_var}}) >= 8950 & as.numeric({{hist_var}}) <=  8983 ~ 11,
        as.numeric({{hist_var}}) >= 9000 & as.numeric({{hist_var}}) <=  9030 ~ 11,
        as.numeric({{hist_var}}) >= 9060 & as.numeric({{hist_var}}) <=  9110 ~ 11,
        as.numeric({{hist_var}}) >= 9260 & as.numeric({{hist_var}}) <=  9365 ~ 11,
        as.numeric({{hist_var}}) >= 9380 & as.numeric({{hist_var}}) <=  9539 ~ 11,
        as.numeric({{hist_var}}) >= 9730 & as.numeric({{hist_var}}) <=  9759 ~ 11,
        as.numeric({{hist_var}}) >= 9760 & as.numeric({{hist_var}}) <=  9769 ~ 11,
        as.numeric({{hist_var}}) >= 8000 & as.numeric({{hist_var}}) <=  8005 ~ 12,
        as.numeric({{hist_var}}) >= 9970 & as.numeric({{hist_var}}) <=  9975 ~ 12,
        .default = NA)) %>%
      sjlabelled::var_labels(new_var = "IARC Histology groups (Morphology ICD-O-3 based recoding IARC 'histologically different' groups)") %>%
      sjlabelled::set_labels(new_var, labels = c("Squamous carcinomas" = 1,
                                                 "Basal cell carcinomas" = 2,
                                                 "Adenocarcinomas" = 3,
                                                 "Other specific carcinomas" = 4,
                                                 "Unspecified carcinomas (NOS)" = 5,
                                                 "Sarcomas and soft tissue tumours" = 6,
                                                 "Lymphomas" = 7,
                                                 "Leukemia" = 8,
                                                 "Kaposi sarcoma" = 9,
                                                 "Mesothelioma" = 10,
                                                 "Other specified types of cancer" = 11,
                                                 "Unspecified types of cancer" = 12)) %>%
      tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
      tidytable::rename({{new_var_hist}} := new_var)
    
    return(df)
  } else {
    
    if(as.character(version) == "3.1"){
      
      #Histologically different groups 
      #Source: Table 25 Fritz AG, Percy C, Jack A, Shanmugaratnam K, Sobin L, Parkin DM, et al., 
      #        editors. International classification of diseases for oncology: ICD-O [Internet]. 
      #        First Revision. Geneva: World Health Organization; 2013 [cited 2019 Aug 14]. 
      #          Available from: https://apps.who.int/iris/handle/10665/96612
      
      
      df <- df %>%
        tidytable::mutate(new_var := tidytable::case_when(
          as.numeric({{hist_var}}) >= 8051 & as.numeric({{hist_var}}) <= 8084 ~ 1,
          as.numeric({{hist_var}}) >= 8120 & as.numeric({{hist_var}}) <= 8131 ~ 1,
          as.numeric({{hist_var}}) >= 8090 & as.numeric({{hist_var}}) <= 8110 ~ 2,
          as.numeric({{hist_var}}) >= 8140 & as.numeric({{hist_var}}) <= 8149 ~ 3,
          as.numeric({{hist_var}}) >= 8160 & as.numeric({{hist_var}}) <= 8162 ~ 3,
          as.numeric({{hist_var}}) >= 8190 & as.numeric({{hist_var}}) <= 8221 ~ 3,
          as.numeric({{hist_var}}) >= 8260 & as.numeric({{hist_var}}) <= 8337 ~ 3,
          as.numeric({{hist_var}}) >= 8350 & as.numeric({{hist_var}}) <= 8551 ~ 3,
          as.numeric({{hist_var}}) >= 8570 & as.numeric({{hist_var}}) <= 8576 ~ 3,
          as.numeric({{hist_var}}) >= 8940 & as.numeric({{hist_var}}) <= 8941 ~ 3,
          as.numeric({{hist_var}}) >= 8030 & as.numeric({{hist_var}}) <= 8046 ~ 4,
          as.numeric({{hist_var}}) >= 8150 & as.numeric({{hist_var}}) <= 8157 ~ 4,
          as.numeric({{hist_var}}) >= 8170 & as.numeric({{hist_var}}) <= 8180 ~ 4,
          as.numeric({{hist_var}}) >= 8230 & as.numeric({{hist_var}}) <= 8255 ~ 4,
          as.numeric({{hist_var}}) >= 8340 & as.numeric({{hist_var}}) <= 8347 ~ 4,
          as.numeric({{hist_var}}) >= 8560 & as.numeric({{hist_var}}) <= 8562 ~ 4,
          as.numeric({{hist_var}}) >= 8580 & as.numeric({{hist_var}}) <= 8671 ~ 4,
          as.numeric({{hist_var}}) >= 8010 & as.numeric({{hist_var}}) <= 8015 ~ 5,
          as.numeric({{hist_var}}) >= 8020 & as.numeric({{hist_var}}) <= 8022 ~ 5,
          as.numeric({{hist_var}}) >= 8050 & as.numeric({{hist_var}}) <= 8050 ~ 5,
          as.numeric({{hist_var}}) >= 8680 & as.numeric({{hist_var}}) <= 8713 ~ 6,
          as.numeric({{hist_var}}) >= 8800 & as.numeric({{hist_var}}) <= 8921 ~ 6,
          as.numeric({{hist_var}}) >= 8990 & as.numeric({{hist_var}}) <= 8991 ~ 6,
          as.numeric({{hist_var}}) >= 9040 & as.numeric({{hist_var}}) <= 9044 ~ 6,
          as.numeric({{hist_var}}) >= 9120 & as.numeric({{hist_var}}) <= 9125 ~ 6,
          as.numeric({{hist_var}}) >= 9130 & as.numeric({{hist_var}}) <= 9136 ~ 6,
          as.numeric({{hist_var}}) >= 9141 & as.numeric({{hist_var}}) <= 9252 ~ 6,
          as.numeric({{hist_var}}) >= 9370 & as.numeric({{hist_var}}) <= 9373 ~ 6,
          as.numeric({{hist_var}}) >= 9540 & as.numeric({{hist_var}}) <= 9582 ~ 6,
          as.numeric({{hist_var}}) >= 9050 & as.numeric({{hist_var}}) <= 9055 ~ 7,
          as.numeric({{hist_var}}) >= 9840 & as.numeric({{hist_var}}) <= 9840 ~ 8,
          as.numeric({{hist_var}}) >= 9861 & as.numeric({{hist_var}}) <= 9931 ~ 8,
          as.numeric({{hist_var}}) >= 9945 & as.numeric({{hist_var}}) <= 9946 ~ 8,
          as.numeric({{hist_var}}) >= 9950 & as.numeric({{hist_var}}) <= 9950 ~ 8,
          as.numeric({{hist_var}}) >= 9961 & as.numeric({{hist_var}}) <= 9964 ~ 8,
          as.numeric({{hist_var}}) >= 9965 & as.numeric({{hist_var}}) <= 9967 ~ 8, #new coding
          as.numeric({{hist_var}}) >= 9980 & as.numeric({{hist_var}}) <= 9987 ~ 8,
          as.numeric({{hist_var}}) >= 9992 & as.numeric({{hist_var}}) <= 9992 ~ 8, #new coding
          as.numeric({{hist_var}}) >= 9670 & as.numeric({{hist_var}}) <= 9699 ~ 9,
          as.numeric({{hist_var}}) >= 9728 & as.numeric({{hist_var}}) <= 9728 ~ 9,
          as.numeric({{hist_var}}) >= 9731 & as.numeric({{hist_var}}) <= 9734 ~ 9,
          as.numeric({{hist_var}}) >= 9735 & as.numeric({{hist_var}}) <= 9738 ~ 9,
          as.numeric({{hist_var}}) >= 9761 & as.numeric({{hist_var}}) <= 9767 ~ 9,
          as.numeric({{hist_var}}) >= 9769 & as.numeric({{hist_var}}) <= 9769 ~ 9,
          as.numeric({{hist_var}}) >= 9823 & as.numeric({{hist_var}}) <= 9826 ~ 9,
          as.numeric({{hist_var}}) >= 9833 & as.numeric({{hist_var}}) <= 9833 ~ 9,
          as.numeric({{hist_var}}) >= 9836 & as.numeric({{hist_var}}) <= 9836 ~ 9,
          as.numeric({{hist_var}}) >= 9940 & as.numeric({{hist_var}}) <= 9940 ~ 9,
          as.numeric({{hist_var}}) >= 9700 & as.numeric({{hist_var}}) <= 9719 ~ 10,
          as.numeric({{hist_var}}) >= 9726 & as.numeric({{hist_var}}) <= 9726 ~ 10,
          as.numeric({{hist_var}}) >= 9729 & as.numeric({{hist_var}}) <= 9729 ~ 10,
          as.numeric({{hist_var}}) >= 9768 & as.numeric({{hist_var}}) <= 9768 ~ 10,
          as.numeric({{hist_var}}) >= 9827 & as.numeric({{hist_var}}) <= 9831 ~ 10,
          as.numeric({{hist_var}}) >= 9834 & as.numeric({{hist_var}}) <= 9834 ~ 10,
          as.numeric({{hist_var}}) >= 9837 & as.numeric({{hist_var}}) <= 9837 ~ 10,
          as.numeric({{hist_var}}) >= 9948 & as.numeric({{hist_var}}) <= 9948 ~ 10,
          as.numeric({{hist_var}}) >= 9650 & as.numeric({{hist_var}}) <= 9667 ~ 11,
          as.numeric({{hist_var}}) >= 9740 & as.numeric({{hist_var}}) <= 9742 ~ 12,
          as.numeric({{hist_var}}) >= 9750 & as.numeric({{hist_var}}) <= 9758 ~ 13,
          as.numeric({{hist_var}}) >= 9590 & as.numeric({{hist_var}}) <= 9591 ~ 14,
          as.numeric({{hist_var}}) >= 9596 & as.numeric({{hist_var}}) <= 9596 ~ 14,
          as.numeric({{hist_var}}) >= 9597 & as.numeric({{hist_var}}) <= 9597 ~ 14,
          as.numeric({{hist_var}}) >= 9727 & as.numeric({{hist_var}}) <= 9727 ~ 14,
          as.numeric({{hist_var}}) >= 9760 & as.numeric({{hist_var}}) <= 9760 ~ 14,
          as.numeric({{hist_var}}) >= 9800 & as.numeric({{hist_var}}) <= 9801 ~ 14,
          as.numeric({{hist_var}}) >= 9805 & as.numeric({{hist_var}}) <= 9805 ~ 14,
          as.numeric({{hist_var}}) >= 9806 & as.numeric({{hist_var}}) <= 9809 ~ 14, #new coding
          as.numeric({{hist_var}}) >= 9811 & as.numeric({{hist_var}}) <= 9816 ~ 14, #new coding
          as.numeric({{hist_var}}) >= 9820 & as.numeric({{hist_var}}) <= 9820 ~ 14,
          as.numeric({{hist_var}}) >= 9832 & as.numeric({{hist_var}}) <= 9832 ~ 14,
          as.numeric({{hist_var}}) >= 9835 & as.numeric({{hist_var}}) <= 9835 ~ 14,
          as.numeric({{hist_var}}) >= 9860 & as.numeric({{hist_var}}) <= 9860 ~ 14,
          as.numeric({{hist_var}}) >= 9960 & as.numeric({{hist_var}}) <= 9960 ~ 14,
          as.numeric({{hist_var}}) >= 9970 & as.numeric({{hist_var}}) <= 9970 ~ 14,
          as.numeric({{hist_var}}) >= 9971 & as.numeric({{hist_var}}) <= 9971 ~ 14, #new coding
          as.numeric({{hist_var}}) >= 9975 & as.numeric({{hist_var}}) <= 9975 ~ 14,
          as.numeric({{hist_var}}) >= 9989 & as.numeric({{hist_var}}) <= 9989 ~ 14,
          as.numeric({{hist_var}}) >= 9140 & as.numeric({{hist_var}}) <= 9140 ~ 15,
          as.numeric({{hist_var}}) >= 8720 & as.numeric({{hist_var}}) <= 8790 ~ 16,
          as.numeric({{hist_var}}) >= 8930 & as.numeric({{hist_var}}) <= 8936 ~ 16,
          as.numeric({{hist_var}}) >= 8950 & as.numeric({{hist_var}}) <= 8983 ~ 16,
          as.numeric({{hist_var}}) >= 9000 & as.numeric({{hist_var}}) <= 9030 ~ 16,
          as.numeric({{hist_var}}) >= 9060 & as.numeric({{hist_var}}) <= 9110 ~ 16,
          as.numeric({{hist_var}}) >= 9260 & as.numeric({{hist_var}}) <= 9365 ~ 16,
          as.numeric({{hist_var}}) >= 9380 & as.numeric({{hist_var}}) <= 9539 ~ 16,
          as.numeric({{hist_var}}) >= 8000 & as.numeric({{hist_var}}) <= 8005 ~ 17,
          .default = NA)) %>%
        sjlabelled::var_labels(new_var = "IARC Histology groups (Morphology ICD-O-3 based recoding IARC 'histologically different' groups)") %>%
        sjlabelled::set_labels(new_var, labels = c("Squamous carcinomas" = 1,
                                                   "Basal cell carcinomas" = 2,
                                                   "Adenocarcinomas" = 3,
                                                   "Other specific carcinomas" = 4,
                                                   "Unspecified carcinomas (NOS)" = 5,
                                                   "Sarcomas and soft tissue tumours" = 6,
                                                   "Mesothelioma" = 7,
                                                   "Myeloid" = 8,
                                                   "B-cell neoplasms" = 9,
                                                   "T-cell and NK-cell neoplasms" = 10,
                                                   "Hodgkin lymphoma" = 11,
                                                   "Mast-cell Tumours" = 12,
                                                   "Histiocytes and Accessory Lymphoid cells" = 13,
                                                   "Unspecified haematopoietic cancers" = 14,
                                                   "Kaposi sarcoma" = 15,
                                                   "Other specified types of cancer" = 16,
                                                   "Unspecified types of cancer" = 17)) %>%
        tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
        tidytable::rename({{new_var_hist}} := new_var)
      
      return(df)
      
    } else{
      
      if(as.character(version) == "3.2"){
        
        #Histologically different groups 
        #Source: Table 25 Fritz AG, Percy C, Jack A, Shanmugaratnam K, Sobin L, Parkin DM, et al., 
        #        editors. International classification of diseases for oncology: ICD-O [Internet]. 
        #        Second Revision. Lyon: International Agency for Research on Cancer (IARC); 2019 [cited 2023 Jun 14]. 
        #        Available from: http://www.iacr.com.fr/index.php?Itemid=577
        
        df <- df %>%
          tidytable::mutate(new_var := tidytable::case_when(
            as.numeric({{hist_var}}) >= 8051 & as.numeric({{hist_var}}) <= 8086 ~ 1, 
            as.numeric({{hist_var}}) >= 8120 & as.numeric({{hist_var}}) <= 8131 ~ 1,
            as.numeric({{hist_var}}) >= 8090 & as.numeric({{hist_var}}) <= 8110 ~ 2,
            as.numeric({{hist_var}}) >= 8140 & as.numeric({{hist_var}}) <= 8149 ~ 3,
            as.numeric({{hist_var}}) >= 8160 & as.numeric({{hist_var}}) <= 8163 ~ 3,
            as.numeric({{hist_var}}) >= 8190 & as.numeric({{hist_var}}) <= 8221 ~ 3,
            as.numeric({{hist_var}}) >= 8250 & as.numeric({{hist_var}}) <= 8552 ~ 3,
            as.numeric({{hist_var}}) >= 8570 & as.numeric({{hist_var}}) <= 8576 ~ 3,
            as.numeric({{hist_var}}) >= 8940 & as.numeric({{hist_var}}) <= 8941 ~ 3,
            as.numeric({{hist_var}}) >= 9110 & as.numeric({{hist_var}}) <= 9110 ~ 3,
            as.numeric({{hist_var}}) >= 8026 & as.numeric({{hist_var}}) <= 8026 ~ 4,
            as.numeric({{hist_var}}) >= 8030 & as.numeric({{hist_var}}) <= 8046 ~ 4,
            as.numeric({{hist_var}}) >= 8150 & as.numeric({{hist_var}}) <= 8158 ~ 4,
            as.numeric({{hist_var}}) >= 8170 & as.numeric({{hist_var}}) <= 8180 ~ 4,
            as.numeric({{hist_var}}) >= 8230 & as.numeric({{hist_var}}) <= 8249 ~ 4,
            as.numeric({{hist_var}}) >= 8560 & as.numeric({{hist_var}}) <= 8562 ~ 4,
            as.numeric({{hist_var}}) >= 8580 & as.numeric({{hist_var}}) <= 8589 ~ 4,
            as.numeric({{hist_var}}) >= 8010 & as.numeric({{hist_var}}) <= 8015 ~ 5,
            as.numeric({{hist_var}}) >= 8020 & as.numeric({{hist_var}}) <= 8022 ~ 5,
            as.numeric({{hist_var}}) >= 8050 & as.numeric({{hist_var}}) <= 8050 ~ 5,
            as.numeric({{hist_var}}) >= 8680 & as.numeric({{hist_var}}) <= 8714 ~ 6,
            as.numeric({{hist_var}}) >= 8800 & as.numeric({{hist_var}}) <= 8921 ~ 6,
            as.numeric({{hist_var}}) >= 8930 & as.numeric({{hist_var}}) <= 8936 ~ 6,
            as.numeric({{hist_var}}) >= 8990 & as.numeric({{hist_var}}) <= 8992 ~ 6,
            as.numeric({{hist_var}}) >= 9040 & as.numeric({{hist_var}}) <= 9045 ~ 6,
            as.numeric({{hist_var}}) >= 9120 & as.numeric({{hist_var}}) <= 9125 ~ 6,
            as.numeric({{hist_var}}) >= 9130 & as.numeric({{hist_var}}) <= 9138 ~ 6,
            as.numeric({{hist_var}}) >= 9141 & as.numeric({{hist_var}}) <= 9252 ~ 6,
            as.numeric({{hist_var}}) >= 9370 & as.numeric({{hist_var}}) <= 9373 ~ 6,
            as.numeric({{hist_var}}) >= 9540 & as.numeric({{hist_var}}) <= 9582 ~ 6,
            as.numeric({{hist_var}}) >= 9050 & as.numeric({{hist_var}}) <= 9055 ~ 7,
            as.numeric({{hist_var}}) >= 9840 & as.numeric({{hist_var}}) <= 9840 ~ 8,
            as.numeric({{hist_var}}) >= 9860 & as.numeric({{hist_var}}) <= 9931 ~ 8,
            as.numeric({{hist_var}}) >= 9945 & as.numeric({{hist_var}}) <= 9946 ~ 8,
            as.numeric({{hist_var}}) >= 9950 & as.numeric({{hist_var}}) <= 9950 ~ 8,
            as.numeric({{hist_var}}) >= 9960 & as.numeric({{hist_var}}) <= 9964 ~ 8,
            as.numeric({{hist_var}}) >= 9966 & as.numeric({{hist_var}}) <= 9966 ~ 8, 
            as.numeric({{hist_var}}) >= 9975 & as.numeric({{hist_var}}) <= 9975 ~ 8, 
            as.numeric({{hist_var}}) >= 9980 & as.numeric({{hist_var}}) <= 9989 ~ 8,
            as.numeric({{hist_var}}) >= 9993 & as.numeric({{hist_var}}) <= 9993 ~ 8, 
            as.numeric({{hist_var}}) >= 9597 & as.numeric({{hist_var}}) <= 9597 ~ 9,
            as.numeric({{hist_var}}) >= 9671 & as.numeric({{hist_var}}) <= 9699 ~ 9,
            as.numeric({{hist_var}}) >= 9712 & as.numeric({{hist_var}}) <= 9712 ~ 9,
            as.numeric({{hist_var}}) >= 9731 & as.numeric({{hist_var}}) <= 9738 ~ 9,
            as.numeric({{hist_var}}) >= 9761 & as.numeric({{hist_var}}) <= 9767 ~ 9,
            as.numeric({{hist_var}}) >= 9769 & as.numeric({{hist_var}}) <= 9769 ~ 9,
            as.numeric({{hist_var}}) >= 9811 & as.numeric({{hist_var}}) <= 9819 ~ 9,
            as.numeric({{hist_var}}) >= 9823 & as.numeric({{hist_var}}) <= 9823 ~ 9,
            as.numeric({{hist_var}}) >= 9833 & as.numeric({{hist_var}}) <= 9833 ~ 9,
            as.numeric({{hist_var}}) >= 9836 & as.numeric({{hist_var}}) <= 9836 ~ 9,
            as.numeric({{hist_var}}) >= 9940 & as.numeric({{hist_var}}) <= 9940 ~ 9,
            as.numeric({{hist_var}}) >= 9700 & as.numeric({{hist_var}}) <= 9709 ~ 10,
            as.numeric({{hist_var}}) >= 9714 & as.numeric({{hist_var}}) <= 9719 ~ 10,
            as.numeric({{hist_var}}) >= 9724 & as.numeric({{hist_var}}) <= 9726 ~ 10,
            as.numeric({{hist_var}}) >= 9768 & as.numeric({{hist_var}}) <= 9768 ~ 10,
            as.numeric({{hist_var}}) >= 9827 & as.numeric({{hist_var}}) <= 9831 ~ 10,
            as.numeric({{hist_var}}) >= 9834 & as.numeric({{hist_var}}) <= 9834 ~ 10,
            as.numeric({{hist_var}}) >= 9837 & as.numeric({{hist_var}}) <= 9837 ~ 10,
            as.numeric({{hist_var}}) >= 9948 & as.numeric({{hist_var}}) <= 9948 ~ 10,
            as.numeric({{hist_var}}) >= 9650 & as.numeric({{hist_var}}) <= 9667 ~ 11,
            as.numeric({{hist_var}}) >= 9740 & as.numeric({{hist_var}}) <= 9742 ~ 12,
            as.numeric({{hist_var}}) >= 9749 & as.numeric({{hist_var}}) <= 9749 ~ 13,
            as.numeric({{hist_var}}) >= 9750 & as.numeric({{hist_var}}) <= 9759 ~ 13,
            as.numeric({{hist_var}}) >= 9590 & as.numeric({{hist_var}}) <= 9591 ~ 14,
            as.numeric({{hist_var}}) >= 9596 & as.numeric({{hist_var}}) <= 9596 ~ 14,
            as.numeric({{hist_var}}) >= 9727 & as.numeric({{hist_var}}) <= 9727 ~ 14,
            as.numeric({{hist_var}}) >= 9760 & as.numeric({{hist_var}}) <= 9760 ~ 14,
            as.numeric({{hist_var}}) >= 9800 & as.numeric({{hist_var}}) <= 9801 ~ 14,
            as.numeric({{hist_var}}) >= 9805 & as.numeric({{hist_var}}) <= 9809 ~ 14,
            as.numeric({{hist_var}}) >= 9820 & as.numeric({{hist_var}}) <= 9820 ~ 14,
            as.numeric({{hist_var}}) >= 9832 & as.numeric({{hist_var}}) <= 9832 ~ 14,
            as.numeric({{hist_var}}) >= 9835 & as.numeric({{hist_var}}) <= 9835 ~ 14,
            as.numeric({{hist_var}}) >= 9965 & as.numeric({{hist_var}}) <= 9965 ~ 14,
            as.numeric({{hist_var}}) >= 9967 & as.numeric({{hist_var}}) <= 9968 ~ 14,
            as.numeric({{hist_var}}) >= 9970 & as.numeric({{hist_var}}) <= 9971 ~ 14,
            as.numeric({{hist_var}}) >= 9140 & as.numeric({{hist_var}}) <= 9140 ~ 15,
            as.numeric({{hist_var}}) >= 8590 & as.numeric({{hist_var}}) <= 8671 ~ 16,
            as.numeric({{hist_var}}) >= 8720 & as.numeric({{hist_var}}) <= 8790 ~ 16,
            as.numeric({{hist_var}}) >= 8950 & as.numeric({{hist_var}}) <= 8983 ~ 16,
            as.numeric({{hist_var}}) >= 9000 & as.numeric({{hist_var}}) <= 9030 ~ 16,
            as.numeric({{hist_var}}) >= 9060 & as.numeric({{hist_var}}) <= 9105 ~ 16,
            as.numeric({{hist_var}}) >= 9260 & as.numeric({{hist_var}}) <= 9365 ~ 16,
            as.numeric({{hist_var}}) >= 9380 & as.numeric({{hist_var}}) <= 9539 ~ 16,
            as.numeric({{hist_var}}) >= 8000 & as.numeric({{hist_var}}) <= 8005 ~ 17,
            TRUE ~ NA_real_)) %>%
          sjlabelled::var_labels(new_var = "IARC Histology groups (Morphology ICD-O-3.2 based recoding IARC 'histologically different' groups)") %>%
          sjlabelled::set_labels(new_var, labels = c("Squamous and transitional cell carcinoma" = 1,
                                                     "Basal cell carcinomas" = 2,
                                                     "Adenocarcinomas" = 3,
                                                     "Other specific carcinomas" = 4,
                                                     "Unspecified carcinomas (NOS)" = 5,
                                                     "Sarcomas and soft tissue tumours" = 6,
                                                     "Mesothelioma" = 7,
                                                     "Myeloid" = 8,
                                                     "B-cell neoplasms" = 9,
                                                     "T-cell and NK-cell neoplasms" = 10,
                                                     "Hodgkin lymphoma" = 11,
                                                     "Mast-cell Tumours" = 12,
                                                     "Histiocytes and Accessory Lymphoid cells" = 13,
                                                     "Unspecified haematopoietic cancers" = 14,
                                                     "Kaposi sarcoma" = 15,
                                                     "Other specified types of cancer" = 16,
                                                     "Unspecified types of cancer" = 17)) %>%
          tidytable::mutate(tidytable::across(.cols = new_var, .fns = ~ sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
          tidytable::rename({{new_var_hist}} := new_var)
        
        return(df)
        
      }
    }
  }
}

