##### angina ###################################################################
data_angina                  <- readr::read_csv("./data-raw/angina.csv")
data_angina$period           <- as.factor(data_angina$period)
data_angina$treatment        <- factor(data_angina$treatment, c("M", "L"))
data_angina$subject          <- as.factor(data_angina$subject)
data_angina$sequence         <- factor(data_angina$sequence, c("ML", "LM"))
data_angina$`sequence index` <- as.factor(data_angina$`sequence index`)
devtools::use_data(data_angina, overwrite = T)
##### Source: Table 4.9 from Senn (2002)
################################################################################

##### asthma ###################################################################
data_asthma                  <- readr::read_csv("./data-raw/asthma.csv")
data_asthma$period           <- as.factor(data_asthma$period)
data_asthma$treatment        <- as.factor(data_asthma$treatment)
data_asthma$subject          <- as.factor(data_asthma$subject)
data_asthma$sequence         <- as.factor(data_asthma$sequence)
data_asthma$`sequence index` <- as.factor(data_asthma$`sequence index`)
devtools::use_data(data_asthma, overwrite = T)
##### Source: Table 2.13 from Jones and Kenward (2014). Also used in Ratkowsky
##### et al. (1992) on p92
################################################################################

##### asthma_2 #################################################################
data_asthma_2                  <- readr::read_csv("./data-raw/asthma_2.csv")
data_asthma_2$period           <- as.factor(data_asthma_2$period)
data_asthma_2$treatment        <- as.factor(data_asthma_2$treatment)
data_asthma_2$subject          <- as.factor(data_asthma_2$subject)
data_asthma_2$sequence         <- as.factor(data_asthma_2$sequence)
data_asthma_2$`sequence index` <- as.factor(data_asthma_2$`sequence index`)
data_asthma_2$sex              <- as.factor(data_asthma_2$sex)
devtools::use_data(data_asthma_2, overwrite = T)
##### Source: Tables 3.1 and 3.8 from Senn (2002)
################################################################################

##### asthma_3 #################################################################
data_asthma_3                  <- readr::read_csv("./data-raw/asthma_3.csv")
data_asthma_3$period           <- as.factor(data_asthma_3$period)
data_asthma_3$treatment        <- as.factor(data_asthma_3$treatment)
data_asthma_3$subject          <- as.factor(data_asthma_3$subject)
data_asthma_3$sequence         <- as.factor(data_asthma_3$sequence)
data_asthma_3$`sequence index` <- as.factor(data_asthma_3$`sequence index`)
data_asthma_3$`pd20 censored`  <- as.factor(data_asthma_3$`pd20 censored`)
devtools::use_data(data_asthma_3, overwrite = T)
##### Source: Tables 4.2 and 4.19 from Senn (2002)
################################################################################

##### asthma_4 #################################################################
data_asthma_4                  <- readr::read_csv("./data-raw/asthma_4.csv")
data_asthma_4$period           <- as.factor(data_asthma_4$period)
data_asthma_4$treatment        <- as.factor(data_asthma_4$treatment)
data_asthma_4$subject          <- as.factor(data_asthma_4$subject)
data_asthma_4$sequence         <- as.factor(data_asthma_4$sequence)
data_asthma_4$`sequence index` <- as.factor(data_asthma_4$`sequence index`)
devtools::use_data(data_asthma_4, overwrite = T)
##### Source: Table 4.17 from Senn (2002)
################################################################################

##### asthma_5 #################################################################
data_asthma_5                  <- readr::read_csv("./data-raw/asthma_5.csv")
data_asthma_5$period           <- as.factor(data_asthma_5$period)
data_asthma_5$treatment        <- factor(data_asthma_5$treatment,
                                         c("P", "I6", "I12", "I24", "M6", "M12",
                                           "M24"))
data_asthma_5$subject          <- as.factor(data_asthma_5$subject)
data_asthma_5$sequence         <-
  factor(data_asthma_5$sequence,
         c("P-I6-M6-M24-I12", "P-I24-M24-I6-M12", "P-M24-M12-M6-I24",
           "I6-I12-P-M24-M12", "I6-M6-M24-I12-I24", "I6-M12-I12-M6-P",
           "I12-P-M24-M12-M6", "I12-I24-M12-P-I6", "I12-M6-P-I24-M24",
           "I24-I6-I12-P-M24", "I24-M12-P-I6-M6", "I24-M24-I6-M12-I12",
           "M6-P-I24-M24-I6", "M6-I24-I6-I12-P", "M6-M24-I12-I24-M12",
           "M12-P-I6-M6-M24", "M12-I12-M6-P-I24", "M12-M6-I24-I6-I12",
           "M24-I6-M12-I12-M6", "M24-I12-I24-M12-P", "M24-M12-M6-I24-I6"))
data_asthma_5$`sequence index` <- factor(match(data_asthma_5$sequence,
                                               levels(data_asthma_5$sequence)), 1:21)
devtools::use_data(data_asthma_5, overwrite = T)
##### Source: 'Selipati' from
##### 'http://www.senns.demon.co.uk/Data/SJS%20Datasets.htm
################################################################################

# Ammend structure
##### blood_pressure ###########################################################
data_blood_pressure                  <-
  readr::read_csv("./data-raw/blood_pressure.csv")
data_blood_pressure$period           <- as.factor(data_blood_pressure$period)
data_blood_pressure$treatment        <- as.factor(data_blood_pressure$treatment)
data_blood_pressure$subject          <- as.factor(data_blood_pressure$subject)
data_blood_pressure$sequence         <- as.factor(data_blood_pressure$sequence)
data_blood_pressure$`sequence index` <-
  as.factor(data_blood_pressure$`sequence index`)
data_blood_pressure$hours            <- as.factor(data_blood_pressure$hours)
devtools::use_data(data_blood_pressure, overwrite = T)
##### Source: Table 5.7/5.16 from Jones and Kenward (2014)
################################################################################

# Ammend structure
##### blood_sugar ##############################################################
data_blood_sugar                  <-
  readr::read_csv("./data-raw/blood_sugar.csv")
data_blood_sugar$period           <- as.factor(data_blood_sugar$period)
data_blood_sugar$treatment        <- as.factor(data_blood_sugar$treatment)
data_blood_sugar$subject          <- as.factor(data_blood_sugar$subject)
data_blood_sugar$sequence         <- as.factor(data_blood_sugar$sequence)
data_blood_sugar$`sequence index` <-
  as.factor(data_blood_sugar$`sequence index`)
data_blood_sugar$hours            <- as.factor(data_blood_sugar$hours)
devtools::use_data(data_blood_sugar, overwrite = T)
##### Source: Tables 5.14 from Jones and Kenward (2014). Also p402 of Ratkowsky
##### et al. (1992)
################################################################################

##### cerebrovascular_deficiency ###############################################
data_cerebrovascular_deficiency                  <-
  readr::read_csv("./data-raw/cerebrovascular_deficiency.csv")
data_cerebrovascular_deficiency$ecg              <-
  as.factor(data_cerebrovascular_deficiency$ecg)
data_cerebrovascular_deficiency$period           <-
  as.factor(data_cerebrovascular_deficiency$period)
data_cerebrovascular_deficiency$subject          <-
  as.factor(data_cerebrovascular_deficiency$subject)
data_cerebrovascular_deficiency$sequence         <-
  as.factor(data_cerebrovascular_deficiency$sequence)
data_cerebrovascular_deficiency$`sequence index` <-
  as.factor(data_cerebrovascular_deficiency$`sequence index`)
data_cerebrovascular_deficiency$center           <-
  as.factor(data_cerebrovascular_deficiency$center)
devtools::use_data(data_cerebrovascular_deficiency, overwrite = T)
##### Source: Table 2.36 from Jones and Kenward (2014)
################################################################################

##### claudication #############################################################
data_claudication                  <-
  readr::read_csv("./data-raw/claudication.csv")
data_claudication$period           <- as.factor(data_claudication$period)
data_claudication$treatment        <- factor(data_claudication$treatment,
                                             c("P", "A", "B", "C"))
data_claudication$subject          <- as.factor(data_claudication$subject)
data_claudication$sequence         <- factor(data_claudication$sequence,
                                             c("PABC", "PBAC", "PBCA", "ABCP",
                                               "ACBP", "ACPB", "APBC", "BCPA",
                                               "BPAC", "CABP", "CAPB", "CBAP",
                                               "CPAB"))
data_claudication$`sequence index` <-
  as.factor(data_claudication$`sequence index`)
devtools::use_data(data_claudication, overwrite = T)
##### Source: Table 5.22 from Jones and Kenward (2014)
################################################################################

##### condoms ##################################################################
data_condoms                  <- readr::read_csv("./data-raw/condoms.csv")
data_condoms$period           <- as.factor(data_condoms$period)
data_condoms$treatment        <- as.factor(data_condoms$treatment)
data_condoms$subject          <- as.factor(data_condoms$subject)
data_condoms$sequence         <- as.factor(data_condoms$sequence)
data_condoms$`sequence index` <- as.factor(data_condoms$`sequence index`)
data_condoms$periods          <- as.factor(data_condoms$periods)
devtools::use_data(data_condoms, overwrite = T)
##### Source: Table 6.6 from Senn (2002)
################################################################################

##### copd #####################################################################
data_copd                       <- readr::read_csv("./data-raw/copd.csv")
data_copd$severity              <- as.factor(data_copd$severity)
data_copd$period                <- as.factor(data_copd$period)
data_copd$treatment             <- as.factor(data_copd$treatment)
data_copd$subject               <- as.factor(data_copd$subject)
data_copd$sequence              <- as.factor(data_copd$sequence)
data_copd$`sequence index`      <- as.factor(data_copd$`sequence index`)
data_copd$`binary baseline nam` <- as.factor(data_copd$`binary baseline nam`)
devtools::use_data(data_copd, overwrite = T)
##### Source: Table 2.1 and Table 2.2 from Jones and Kenward (2014)
################################################################################

##### copd_missing #############################################################
data_copd_missing                       <-
  readr::read_csv("./data-raw/copd_missing.csv")
data_copd_missing$period                <- as.factor(data_copd_missing$period)
data_copd_missing$treatment             <-
  as.factor(data_copd_missing$treatment)
data_copd_missing$subject               <- as.factor(data_copd_missing$subject)
data_copd_missing$sequence              <- as.factor(data_copd_missing$sequence)
data_copd_missing$`sequence index`      <-
  as.factor(data_copd_missing$`sequence index`)
devtools::use_data(data_copd_missing, overwrite = T)
##### Source: Table 5.6 from Jones and Kenward (2014)
################################################################################

##### cow_1 ####################################################################
data_cow_1                  <- readr::read_csv("./data-raw/cow_1.csv")
data_cow_1$period           <- as.factor(data_cow_1$period)
data_cow_1$treatment        <- as.factor(data_cow_1$treatment)
data_cow_1$subject          <- as.factor(data_cow_1$subject)
data_cow_1$sequence         <- as.factor(data_cow_1$sequence)
data_cow_1$`sequence index` <- as.factor(data_cow_1$`sequence index`)
data_cow_1$square           <- as.factor(data_cow_1$square)
devtools::use_data(data_cow_1, overwrite = T)
##### Source: Table 1.1 of Ratkowsky et al. (1992)
################################################################################

##### cow_2 ####################################################################
data_cow_2                  <- readr::read_csv("./data-raw/cow_2.csv")
data_cow_2$period           <- as.factor(data_cow_2$period)
data_cow_2$treatment        <- as.factor(data_cow_2$treatment)
data_cow_2$subject          <- as.factor(data_cow_2$subject)
data_cow_2$sequence         <- as.factor(data_cow_2$sequence)
data_cow_2$`sequence index` <- as.factor(data_cow_2$`sequence index`)
devtools::use_data(data_cow_2, overwrite = T)
##### Source: p115 of Ratkowsky et al. (1992)
################################################################################

##### cow_3 ####################################################################
data_cow_3                  <- readr::read_csv("./data-raw/cow_3.csv")
data_cow_3$period           <- as.factor(data_cow_3$period)
data_cow_3$treatment        <- as.factor(data_cow_3$treatment)
data_cow_3$subject          <- as.factor(data_cow_3$subject)
data_cow_3$sequence         <- as.factor(data_cow_3$sequence)
data_cow_3$`sequence index` <- as.factor(data_cow_3$`sequence index`)
data_cow_3$block            <- as.factor(data_cow_3$block)
devtools::use_data(data_cow_3, overwrite = T)
##### Source: Table 5.1 of Ratkowsky et al. (1992)
################################################################################

##### cow_4 ####################################################################
data_cow_4                  <- readr::read_csv("./data-raw/cow_4.csv")
data_cow_4$period           <- as.factor(data_cow_4$period)
data_cow_4$treatment        <- as.factor(data_cow_4$treatment)
data_cow_4$subject          <- as.factor(data_cow_4$subject)
data_cow_4$sequence         <- as.factor(data_cow_4$sequence)
data_cow_4$`sequence index` <- as.factor(data_cow_4$`sequence index`)
devtools::use_data(data_cow_4, overwrite = T)
##### Source: p184 of Ratkowsky et al. (1992)
################################################################################

##### cow_5 ####################################################################
data_cow_5                  <- readr::read_csv("./data-raw/cow_5.csv")
data_cow_5$period           <- as.factor(data_cow_5$period)
data_cow_5$treatment        <- as.factor(data_cow_5$treatment)
data_cow_5$subject          <- as.factor(data_cow_5$subject)
data_cow_5$sequence         <- as.factor(data_cow_5$sequence)
data_cow_5$`sequence index` <- as.factor(data_cow_5$`sequence index`)
devtools::use_data(data_cow_5, overwrite = T)
##### Source: p185 of Ratkowsky et al. (1992)
################################################################################

##### cow_6 ####################################################################
data_cow_6                  <- readr::read_csv("./data-raw/cow_6.csv")
data_cow_6$period           <- as.factor(data_cow_6$period)
data_cow_6$treatment        <- as.factor(data_cow_6$treatment)
data_cow_6$subject          <- as.factor(data_cow_6$subject)
data_cow_6$sequence         <- as.factor(data_cow_6$sequence)
data_cow_6$`sequence index` <- as.factor(data_cow_6$`sequence index`)
data_cow_6$block            <- as.factor(data_cow_6$block)
devtools::use_data(data_cow_6, overwrite = T)
##### Source: p231 of Ratkowsky et al. (1992)
################################################################################

##### cow_7 ####################################################################
data_cow_7                  <- readr::read_csv("./data-raw/cow_7.csv")
data_cow_7$period           <- as.factor(data_cow_7$period)
data_cow_7$treatment        <- as.factor(data_cow_7$treatment)
data_cow_7$subject          <- as.factor(data_cow_7$subject)
data_cow_7$sequence         <- as.factor(data_cow_7$sequence)
data_cow_7$`sequence index` <- as.factor(data_cow_7$`sequence index`)
devtools::use_data(data_cow_7, overwrite = T)
##### Source: p385 of Ratkowsky et al. (1992)
################################################################################

##### davis_hall ###############################################################
data_davis_hall                  <- readr::read_csv("./data-raw/davis_hall.csv")
data_davis_hall$period           <- as.factor(data_davis_hall$period)
data_davis_hall$treatment        <- as.factor(data_davis_hall$treatment)
data_davis_hall$subject          <- as.factor(data_davis_hall$subject)
data_davis_hall$sequence         <- as.factor(data_davis_hall$sequence)
data_davis_hall$`sequence index` <- as.factor(data_davis_hall$`sequence index`)
data_davis_hall$block            <- as.factor(data_davis_hall$block)
devtools::use_data(data_davis_hall, overwrite = T)
##### Source: p235 of Ratkowsky et al. (1992)
################################################################################

##### dysmenorrhea #############################################################
data_dysmenorrhea                  <-
  readr::read_csv("./data-raw/dysmenorrhea.csv")
data_dysmenorrhea$relief           <- as.factor(data_dysmenorrhea$relief)
data_dysmenorrhea$period           <- as.factor(data_dysmenorrhea$period)
data_dysmenorrhea$treatment        <- as.factor(data_dysmenorrhea$treatment)
data_dysmenorrhea$subject          <- as.factor(data_dysmenorrhea$subject)
data_dysmenorrhea$sequence         <- as.factor(data_dysmenorrhea$sequence)
data_dysmenorrhea$`sequence index` <-
  as.factor(data_dysmenorrhea$`sequence index`)
devtools::use_data(data_dysmenorrhea, overwrite = T)
##### Source: Table 6.8 of Jones and Kenward (2014). Also p329 and p346 of
##### Ratkowsky et al. (1992)
################################################################################

##### eia ######################################################################
data_eia                  <- readr::read_csv("./data-raw/eia.csv")
data_eia$period           <- as.factor(data_eia$period)
data_eia$treatment        <- factor(data_eia$treatment, c("P", "F", "S"))
data_eia$subject          <- as.factor(data_eia$subject)
data_eia$sequence         <- factor(data_eia$sequence, c("PFS", "PSF", "FPS",
                                                         "FSP", "SPF", "SFP"))
data_eia$`sequence index` <- factor(match(data_eia$sequence,
                                          levels(data_eia$sequence)), 1:6)
data_eia$baseline         <- as.double(data_eia$baseline)
devtools::use_data(data_eia, overwrite = T)
##### Table 5.2 from Senn (2002). The data in the book is an extension of
##### 'Russia' from http://www.senns.demon.co.uk/Data/SJS%20Datasets.htm.
################################################################################

##### eia_2 ####################################################################
data_eia_2                  <- readr::read_csv("./data-raw/eia_2.csv")
data_eia_2$`4ps`            <- as.factor(data_eia_2$`4ps`)
data_eia_2$`2ps`            <- as.factor(data_eia_2$`2ps`)
data_eia_2$period           <- as.factor(data_eia_2$period)
data_eia_2$treatment        <- as.factor(data_eia_2$treatment)
data_eia_2$subject          <- as.factor(data_eia_2$subject)
data_eia_2$sequence         <- as.factor(data_eia_2$sequence)
data_eia_2$`sequence index` <- as.factor(data_eia_2$`sequence index`)
devtools::use_data(data_eia_2, overwrite = T)
##### Source: Table 4.12 from Senn (2002)
################################################################################

##### erythromycin #############################################################
data_erythromycin                  <-
  readr::read_csv("./data-raw/erythromycin.csv")
data_erythromycin$period           <- as.factor(data_erythromycin$period)
data_erythromycin$treatment        <- as.factor(data_erythromycin$treatment)
data_erythromycin$subject          <- as.factor(data_erythromycin$subject)
data_erythromycin$sequence         <- as.factor(data_erythromycin$sequence)
data_erythromycin$`sequence index` <-
  as.factor(data_erythromycin$`sequence index`)
devtools::use_data(data_erythromycin, overwrite = T)
##### Source:
################################################################################

##### example_5_2 ##############################################################
data_example_5_2                  <-
  readr::read_csv("./data-raw/example_5_2.csv")
data_example_5_2$period           <- as.factor(data_example_5_2$period)
data_example_5_2$treatment        <- as.factor(data_example_5_2$treatment)
data_example_5_2$subject          <- as.factor(data_example_5_2$subject)
data_example_5_2$sequence         <- as.factor(data_example_5_2$sequence)
data_example_5_2$`sequence index` <-
  as.factor(data_example_5_2$`sequence index`)
devtools::use_data(data_example_5_2, overwrite = T)
##### Source: Table 5.3 from Jones and Kenward (2014)
################################################################################

##### example_6_1 ##############################################################
data_example_6_1                  <-
  readr::read_csv("./data-raw/example_6_1.csv")
data_example_6_1$outcome          <- as.factor(data_example_6_1$outcome)
data_example_6_1$period           <- as.factor(data_example_6_1$period)
data_example_6_1$treatment        <- as.factor(data_example_6_1$treatment)
data_example_6_1$subject          <- as.factor(data_example_6_1$subject)
data_example_6_1$sequence         <- as.factor(data_example_6_1$sequence)
data_example_6_1$`sequence index` <-
  as.factor(data_example_6_1$`sequence index`)
devtools::use_data(data_example_6_1, overwrite = T)
##### Source Table 6.1 from Jones and Kenward (2014)
################################################################################

##### federer_atkinson #########################################################
data_federer_atkinson                  <-
  readr::read_csv("./data-raw/federer_atkinson.csv")
data_federer_atkinson$period           <-
  as.factor(data_federer_atkinson$period)
data_federer_atkinson$treatment        <-
  as.factor(data_federer_atkinson$treatment)
data_federer_atkinson$subject          <-
  as.factor(data_federer_atkinson$subject)
data_federer_atkinson$sequence         <-
  as.factor(data_federer_atkinson$sequence)
data_federer_atkinson$`sequence index` <-
  as.factor(data_federer_atkinson$`sequence index`)
devtools::use_data(data_federer_atkinson, overwrite = T)
##### Source: p239 of Ratkowsky et al. (1992)
################################################################################

##### formoterol ###############################################################
data_formoterol                  <- readr::read_csv("./data-raw/formoterol.csv")
data_formoterol$period           <- as.factor(data_formoterol$period)
data_formoterol$treatment        <- as.factor(data_formoterol$treatment)
data_formoterol$subject          <- as.factor(data_formoterol$subject)
data_formoterol$sequence         <- as.factor(data_formoterol$sequence)
data_formoterol$`sequence index` <- as.factor(data_formoterol$`sequence index`)
devtools::use_data(data_formoterol, overwrite = T)
##### Source: Table 7.1 from Senn (2002)
################################################################################

##### formoterol_2 #############################################################
data_formoterol_2                  <-
  readr::read_csv("./data-raw/formoterol_2.csv")
data_formoterol_2$period           <- as.factor(data_formoterol_2$period)
data_formoterol_2$treatment        <- factor(data_formoterol_2$treatment,
                                             c("P", "F12", "F24"))
data_formoterol_2$subject          <- as.factor(data_formoterol_2$subject)
data_formoterol_2$sequence         <- factor(data_formoterol_2$sequence,
                                             c("P-F12", "P-F24", "F12-P",
                                               "F12-F24", "F24-P","F24-F12"))
data_formoterol_2$`sequence index` <-
  as.factor(data_formoterol_2$`sequence index`)
devtools::use_data(data_formoterol_2, overwrite = T)
##### Source: Table 7.3 from Senn (2002)
################################################################################

##### heartburn ################################################################
data_heartburn                  <- readr::read_csv("./data-raw/heartburn.csv")
data_heartburn$cat              <- as.factor(data_heartburn$cat)
data_heartburn$period           <- as.factor(data_heartburn$period)
data_heartburn$treatment        <- as.factor(data_heartburn$treatment)
data_heartburn$sequence         <- as.factor(data_heartburn$sequence)
data_heartburn$subject          <- as.factor(data_heartburn$subject)
data_heartburn$`sequence index` <- as.factor(data_heartburn$`sequence index`)
data_heartburn$sex              <- as.factor(data_heartburn$sex)
data_heartburn$center           <- as.factor(data_heartburn$center)
devtools::use_data(data_heartburn, overwrite = T)
##### Source: Table 2.27 and 2.28 from Jones and Kenward (2014)
################################################################################

##### heart_disease ############################################################
data_heart_disease                  <-
  readr::read_csv("./data-raw/heart_disease.csv")
data_heart_disease$period           <- as.factor(data_heart_disease$period)
data_heart_disease$treatment        <- as.factor(data_heart_disease$treatment)
data_heart_disease$subject          <- as.factor(data_heart_disease$subject)
data_heart_disease$sequence         <- as.factor(data_heart_disease$sequence)
data_heart_disease$`sequence index` <-
  as.factor(data_heart_disease$`sequence index`)
devtools::use_data(data_heart_disease, overwrite = T)
##### Source: Table 2.30 from Jones and Kenward (2014)
################################################################################

##### hypertension #############################################################
data_hypertension                  <-
  readr::read_csv("./data-raw/hypertension.csv")
data_hypertension$period           <- as.factor(data_hypertension$period)
data_hypertension$treatment        <- as.factor(data_hypertension$treatment)
data_hypertension$subject          <- as.factor(data_hypertension$subject)
data_hypertension$sequence         <- as.factor(data_hypertension$sequence)
data_hypertension$`sequence index` <-
  as.factor(data_hypertension$`sequence index`)
devtools::use_data(data_hypertension, overwrite = T)
##### Source: Tables 5.11 and 5.12 from Jones and Kenward (2014). Also Table 4.1
##### from Ratkowsky et al. (1992) on p92
################################################################################

##### mental_fatigue ###########################################################
data_mental_fatigue                  <-
  readr::read_csv("./data-raw/mental_fatigue.csv")
data_mental_fatigue$period           <- as.factor(data_mental_fatigue$period)
data_mental_fatigue$treatment        <- as.factor(data_mental_fatigue$treatment)
data_mental_fatigue$subject          <- as.factor(data_mental_fatigue$subject)
data_mental_fatigue$sequence         <- as.factor(data_mental_fatigue$sequence)
data_mental_fatigue$`sequence index` <-
  as.factor(data_mental_fatigue$`sequence index`)
devtools::use_data(data_mental_fatigue, overwrite = T)
##### Source:
################################################################################

##### migraine #################################################################
data_migraine                    <- readr::read_csv("./data-raw/migraine.csv")
data_migraine$period             <- as.factor(data_migraine$period)
data_migraine$treatment          <- factor(data_migraine$treatment,
                                           c("P", "D1", "D2"))
data_migraine$subject            <- as.factor(data_migraine$subject)
data_migraine$sequence           <- factor(data_migraine$sequence,
                                           c("P-D1-D2", "P-D2-D1", "D1-P-D2",
                                             "D1-D2-P", "D2-P-D1", "D2-D1-P"))
data_migraine$`sequence index`   <- data_migraine$`sequence index`
data_migraine$`substituted data` <- as.factor(data_migraine$`substituted data`)
devtools::use_data(data_migraine, overwrite = T)
##### Source: Table 6.1 from Senn (2002)
################################################################################

##### parkinsons ###############################################################
data_parkinsons                  <- readr::read_csv("./data-raw/parkinsons.csv")
data_parkinsons$period           <- as.factor(data_parkinsons$period)
data_parkinsons$treatment        <- as.factor(data_parkinsons$treatment)
data_parkinsons$subject          <- as.factor(data_parkinsons$subject)
data_parkinsons$sequence         <- as.factor(data_parkinsons$sequence)
data_parkinsons$`sequence index` <- as.factor(data_parkinsons$`sequence index`)
devtools::use_data(data_parkinsons, overwrite = T)
##### Source: Table 5.9 from Jones and Kenward (2014). Also Table 4.3 from
##### Ratkowsky et al. (1992)
################################################################################

##### pentobarbitol ############################################################
data_pentobarbitol                  <-
  readr::read_csv("./data-raw/pentobarbitol.csv")
data_pentobarbitol$period           <- as.factor(data_pentobarbitol$period)
data_pentobarbitol$treatment        <- as.factor(data_pentobarbitol$treatment)
data_pentobarbitol$subject          <- as.factor(data_pentobarbitol$subject)
data_pentobarbitol$sequence         <- as.factor(data_pentobarbitol$sequence)
data_pentobarbitol$`sequence index` <-
  as.factor(data_pentobarbitol$`sequence index`)
devtools::use_data(data_pentobarbitol, overwrite = T)
##### Source: Table 10.5 from Senn (2002)
################################################################################

##### perceived_speed ##########################################################
data_perceived_speed                  <-
  readr::read_csv("./data-raw/perceived_speed.csv")
data_perceived_speed$period           <- as.factor(data_perceived_speed$period)
data_perceived_speed$treatment        <-
  as.factor(data_perceived_speed$treatment)
data_perceived_speed$subject          <- as.factor(data_perceived_speed$subject)
data_perceived_speed$sequence         <-
  as.factor(data_perceived_speed$sequence)
data_perceived_speed$`sequence index` <-
  as.factor(data_perceived_speed$`sequence index`)
data_perceived_speed$display          <- as.factor(data_perceived_speed$display)
data_perceived_speed$speed            <- as.factor(data_perceived_speed$speed)
devtools::use_data(data_perceived_speed, overwrite = T)
##### Source: Tables 5.28-5.30 from Jones and Kenward (2014)
################################################################################

##### pharmacokinetic ##########################################################
data_pharmacokinetic                  <-
  readr::read_csv("./data-raw/pharmacokinetic.csv")
data_pharmacokinetic$period           <- as.factor(data_pharmacokinetic$period)
data_pharmacokinetic$treatment        <-
  as.factor(data_pharmacokinetic$treatment)
data_pharmacokinetic$subject          <- as.factor(data_pharmacokinetic$subject)
data_pharmacokinetic$sequence         <-
  as.factor(data_pharmacokinetic$sequence)
data_pharmacokinetic$`sequence index` <-
  as.factor(data_pharmacokinetic$`sequence index`)
devtools::use_data(data_pharmacokinetic, overwrite = T)
##### Source: Table 7.1 of Jones and Kenward (2014)
################################################################################

##### phenytoin ################################################################
data_phenytoin                  <- readr::read_csv("./data-raw/phenytoin.csv")
data_phenytoin$period           <- as.factor(data_phenytoin$period)
data_phenytoin$treatment        <- as.factor(data_phenytoin$treatment)
data_phenytoin$subject          <- as.factor(data_phenytoin$subject)
data_phenytoin$sequence         <- as.factor(data_phenytoin$sequence)
data_phenytoin$`sequence index` <- as.factor(data_phenytoin$`sequence index`)
devtools::use_data(data_phenytoin, overwrite = T)
##### Source: Table 7.3 from Senn (2002), or 'Phenytoin' from
##### http://www.senns.demon.co.uk/Data/SJS%20Datasets.htm
################################################################################

##### plaque ###################################################################
data_plaque                  <- readr::read_csv("./data-raw/plaque.csv")
data_plaque$period           <- as.factor(data_plaque$period)
data_plaque$treatment        <- as.factor(data_plaque$treatment)
data_plaque$subject          <- as.factor(data_plaque$subject)
data_plaque$sequence         <- as.factor(data_plaque$sequence)
data_plaque$`sequence index` <- as.factor(data_plaque$`sequence index`)
devtools::use_data(data_plaque, overwrite = T)
##### Source: Table 2.20 from Jones and Kenward (2014)
################################################################################

##### rabbit ###################################################################
data_rabbit                  <- readr::read_csv("./data-raw/rabbit.csv")
data_rabbit$period           <- as.factor(data_rabbit$period)
data_rabbit$treatment        <- as.factor(data_rabbit$treatment)
data_rabbit$subject          <- as.factor(data_rabbit$subject)
data_rabbit$sequence         <- as.factor(data_rabbit$sequence)
data_rabbit$`sequence index` <- as.factor(data_rabbit$`sequence index`)
data_rabbit$square           <- as.factor(data_rabbit$square)
devtools::use_data(data_rabbit, overwrite = T)
##### Source: p70 of Ratkowsky et al. (1992)
################################################################################

##### ratkowsky_1 ##############################################################
data_ratkowsky_1                  <-
  readr::read_csv("./data-raw/ratkowsky_1.csv")
data_ratkowsky_1$period           <- as.factor(data_ratkowsky_1$period)
data_ratkowsky_1$treatment        <- as.factor(data_ratkowsky_1$treatment)
data_ratkowsky_1$subject          <- as.factor(data_ratkowsky_1$subject)
data_ratkowsky_1$sequence         <- as.factor(data_ratkowsky_1$sequence)
data_ratkowsky_1$`sequence index` <-
  as.factor(data_ratkowsky_1$`sequence index`)
devtools::use_data(data_ratkowsky_1, overwrite = T)
##### Source: p119 of Ratkowsky et al. (1992)
################################################################################

##### ratkowsky_2 ##############################################################
data_ratkowsky_2                  <-
  readr::read_csv("./data-raw/ratkowsky_2.csv")
data_ratkowsky_2$outcome          <- as.factor(data_ratkowsky_2$outcome)
data_ratkowsky_2$period           <- as.factor(data_ratkowsky_2$period)
data_ratkowsky_2$treatment        <- as.factor(data_ratkowsky_2$treatment)
data_ratkowsky_2$subject          <- as.factor(data_ratkowsky_2$subject)
data_ratkowsky_2$sequence         <- as.factor(data_ratkowsky_2$sequence)
data_ratkowsky_2$`sequence index` <-
  as.factor(data_ratkowsky_2$`sequence index`)
devtools::use_data(data_ratkowsky_2, overwrite = T)
##### Source: p313 of Ratkowsky et al. (1992)
################################################################################

##### raynaud ##################################################################
data_raynaud                  <- readr::read_csv("./data-raw/raynaud.csv")
data_raynaud$period           <- as.factor(data_raynaud$period)
data_raynaud$treatment        <- factor(data_raynaud$treatment, c("P", "N"))
data_raynaud$subject          <- as.factor(data_raynaud$subject)
data_raynaud$sequence         <- factor(data_raynaud$sequence, c("PN", "NP"))
data_raynaud$`sequence index` <- as.factor(data_raynaud$`sequence index`)
devtools::use_data(data_raynaud, overwrite = T)
##### Source:
################################################################################

##### sauter ###################################################################
data_sauter                  <- readr::read_csv("./data-raw/sauter.csv")
data_sauter$period           <- as.factor(data_sauter$period)
data_sauter$treatment        <- as.factor(data_sauter$treatment)
data_sauter$subject          <- as.factor(data_sauter$subject)
data_sauter$sequence         <- as.factor(data_sauter$sequence)
data_sauter$`sequence index` <- as.factor(data_sauter$`sequence index`)
devtools::use_data(data_sauter, overwrite = T)
##### Source:
################################################################################

##### severe_respiratory_failure ###############################################
data_severe_respiratory_failure                  <-
  readr::read_csv("./data-raw/severe_respiratory_failure.csv")
data_severe_respiratory_failure$period           <-
  as.factor(data_severe_respiratory_failure$period)
data_severe_respiratory_failure$treatment        <-
  as.factor(data_severe_respiratory_failure$treatment)
data_severe_respiratory_failure$subject          <-
  as.factor(data_severe_respiratory_failure$subject)
data_severe_respiratory_failure$sequence         <-
  as.factor(data_severe_respiratory_failure$sequence)
data_severe_respiratory_failure$`sequence index` <-
  as.factor(data_severe_respiratory_failure$`sequence index`)
devtools::use_data(data_severe_respiratory_failure, overwrite = T)
##### Source: Table 5.2 of Jones and Kenward (2014)
################################################################################

##### testicular ###############################################################
data_testicular                  <- readr::read_csv("./data-raw/testicular.csv")
data_testicular$period           <- as.factor(data_testicular$period)
data_testicular$treatment        <- as.factor(data_testicular$treatment)
data_testicular$subject          <- as.factor(data_testicular$subject)
data_testicular$sequence         <- as.factor(data_testicular$sequence)
data_testicular$`sequence index` <- as.factor(data_testicular$`sequence index`)
devtools::use_data(data_testicular, overwrite = T)
##### Source: p76 of Ratkowsky et al. (1992)
################################################################################

##### visuo_spatial_ability ####################################################
data_visuo_spatial_ability                  <-
  readr::read_csv("./data-raw/visuo_spatial_ability.csv")
data_visuo_spatial_ability$period           <-
  as.factor(data_visuo_spatial_ability$period)
data_visuo_spatial_ability$treatment        <-
  as.factor(data_visuo_spatial_ability$treatment)
data_visuo_spatial_ability$subject          <-
  as.factor(data_visuo_spatial_ability$subject)
data_visuo_spatial_ability$sequence         <-
  as.factor(data_visuo_spatial_ability$sequence)
data_visuo_spatial_ability$`sequence index` <-
  as.factor(data_visuo_spatial_ability$`sequence index`)
devtools::use_data(data_visuo_spatial_ability, overwrite = T)
##### Source: Table 2.17 from Jones and Kenward (2014)
################################################################################

##### zimmerman_rahlfs ###############################################################
data_zimmerman_rahlfs                  <-
  readr::read_csv("./data-raw/zimmerman_rahlfs.csv")
data_zimmerman_rahlfs$outcome          <-
  as.factor(data_zimmerman_rahlfs$outcome)
data_zimmerman_rahlfs$period           <-
  as.factor(data_zimmerman_rahlfs$period)
data_zimmerman_rahlfs$treatment        <-
  as.factor(data_zimmerman_rahlfs$treatment)
data_zimmerman_rahlfs$subject          <-
  as.factor(data_zimmerman_rahlfs$subject)
data_zimmerman_rahlfs$sequence         <-
  as.factor(data_zimmerman_rahlfs$sequence)
data_zimmerman_rahlfs$`sequence index` <-
  as.factor(data_zimmerman_rahlfs$`sequence index`)
devtools::use_data(data_zimmerman_rahlfs, overwrite = T)
##### Source: p309 of Ratkowsky et al. (1992)
################################################################################

# Senn's repeated measurements one and the one from Biostatistics website

test <- read_csv("GS20.csv")
test <- gather(test,`Time Period`, PEF, `PEF1 1`:`PEF2 16`)
test <- separate(test,`Time Period`, into = c("Period", "Measurement"), sep = " ")
test$Period <- recode(test$Period, PEF1 = 1, PEF2  = 2)
test$Sex <- recode(test$Sex, male = "Male", female  = "Female")
test <- rename(test, Subject = `Patient ID`)
test <- rename(test, PEF = `PEF`)
test$Time <- recode(test$Measurement, "1" = 0, "2" = 0, "3" = 20, "4" = 40, "5" = 60, "6" = 90,
                           "7" = 120, "8" = 180, "9" = 240, "10" = 300, "11" = 360, "12" = 420, "13" = 480,
                           "14" = 600, "15" = 660, "16" = 720)
test$Treatment <- factor(sapply(1:nrow(test),
                                function(i, data) strsplit(data$Sequence[i], NULL)[[1]][data$Period[i]], test),
                         levels = c("F", "S"))
test$Sequence <- as.factor(test$Sequence)
test$`Sequence Index` <- factor(match(test$Sequence, levels(test$Sequence)),
                                     levels = 1:2)
test <- test[c("PEF", "Period", "Treatment", "Subject", "Sequence",
             "Sequence Index", "Measurement", "Time", "Age", "Sex", "Height", "Weight",
             "Duration")]

write_excel_csv(test, "GS20.csv")

