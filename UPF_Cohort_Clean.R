############################################################################################################################
#Project: Ultra Processed Foods and Incidence/Mortality                                                                    #
#Analyst: Matt Masters                                                                                            #
#Cohort Reviewed by: _______________________                                                                               #
#Analysis Reviewed by: __________________________                                                                          #
############################################################################################################################
library(dplyr)

#Master
CPS2_master_women<- collect(
  tbl(scdbcon, DBI::Id(table = "CPS2SMGMST19F", schema = "RAW")) %>% 
    select(ID, RACE, starts_with('NUT_')) %>%
    mutate(
      sex='F'
    ) %>%
    filter(
      NUT_92==1 #only keep this in nutrition cohort
    )
)

CPS2_master_men<- collect(
  tbl(scdbcon, DBI::Id(table = "CPS2SMGMST19M", schema = "RAW")) %>% 
    select(ID, RACE, starts_with('NUT_')) %>%
    mutate(
      sex='M'
    ) %>%
    filter(
      NUT_92==1 #only keep this in nutrition cohort
    )
)

CPS2_master<- rbind(CPS2_master_women, CPS2_master_men)
rm(CPS2_master_women, CPS2_master_men)

#PYear
CPS2_pyear_f<- collect(
  tbl(scdbcon, DBI::Id(table = "NUT92FEMPYR1220", schema = "RAW")) %>% 
    select(ID, BDAYDATE, DATEDD)
)

CPS2_pyear_m<- collect(
  tbl(scdbcon, DBI::Id(table = "NUT92MENPYR1220", schema = "RAW")) %>% 
    select(ID, BDAYDATE, DATEDD)
)

CPS2_pyear<- rbind(CPS2_pyear_f, CPS2_pyear_m)
rm(CPS2_pyear_f, CPS2_pyear_m)

#Mortality
CPS2_mortality<- collect(
  tbl(scdbcon, DBI::Id(table = "SMGMRT19D231211", schema = "RAW")) %>% 
    select(ID, starts_with('CODE')) %>%
    mutate(
      in_mortality=1
    )
)

#Verified
CPS2_verified<- collect(
  tbl(scdbcon, DBI::Id(table = "MAR2024VERIFIED", schema = "RAW")) %>% 
    select(ID, DODDY, DODMO, DODYR, HISTOGY, INCCANV, BEHVCODE, GSSTAGE, GRDI, SITEC) %>%
    mutate(
      in_verified=1,
      DODYR = as.numeric(DODYR),
      DODMO = case_when(
        DODMO=='99' ~ 06,
        TRUE ~ as.numeric(DODMO)
      ),
      DODDY = case_when(
        DODDY=='99' ~ 15,
        DODDY=='31' & DODMO %in% c(04, 06, 09, 11) ~ 30,
        DODDY %in% c('29', '30', '31') & DODMO == 2 ~ 28,
        TRUE ~ as.numeric(DODDY)
      ),
      diagnosis_date = as.Date(paste0(DODYR, '-', DODMO, '-', DODDY)),
      GSSTAGE = case_when(
        GSSTAGE %in% c('9', 'X') ~ NA, #make unstaged and missing NA
        TRUE ~ GSSTAGE
      ),
      GRDI = case_when(
        GRDI %in% c('9', 'X') ~ NA, #make determined and missing NA
        TRUE ~ GRDI
      )
    ) %>%
    select(-DODMO, -DODDY, -DODYR) %>%
    group_by(ID) %>%
    slice_min(diagnosis_date, with_ties=TRUE) %>% #159 people with multiple tumors on earliest date
    group_by(ID) %>%
    slice_max(BEHVCODE=='3', with_ties=TRUE) %>% #143 still have dupes after taking only invasive tumors
    group_by(ID) %>%
    slice_max(as.numeric(GSSTAGE), with_ties=TRUE) %>% #67 with multiple tumors after taking highest stage
    group_by(ID) %>%
    slice_max(as.numeric(GRDI), with_ties=FALSE) #38 with multiples when taking highest grade, which will prefer lymphomas and leukemias
  #with_ties=FALSE will pick one at random for these people, many are both NA for some of these, or closely located tumors
) %>%
  ungroup()


#1982 Original
CPS2_men_1982_original<- collect(
  tbl(scdbcon, DBI::Id(table = "CPS2MENORIGINAL", schema = "RAW")) %>% 
    select(ID, EDUCATE)
)

CPS2_women_1982_original<- collect(
  tbl(scdbcon, DBI::Id(table = "CPS2FEMORIGINAL", schema = "RAW")) %>% 
    select(ID, EDUCATE) #We don't ask about military servive for women..........
)

CPS2_1982_original <- bind_rows(CPS2_men_1982_original, CPS2_women_1982_original)
rm(CPS2_men_1982_original, CPS2_women_1982_original)

#1982 Derived
CPS2_men_1982_derived<- collect(
  tbl(scdbcon, DBI::Id(table = "CPS2MENKEYVAR3", schema = "RAW")) %>% 
    select(ID, DTINT82, CA82)
)

CPS2_women_1982_derived<- collect(
  tbl(scdbcon, DBI::Id(table = "CPS2FEMKEYVAR3", schema = "RAW")) %>% 
    select(ID, DTINT82, CA82) 
)

CPS2_1982_derived <- bind_rows(CPS2_men_1982_derived, CPS2_women_1982_derived)
rm(CPS2_men_1982_derived, CPS2_women_1982_derived)

#1992
derived_1992_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY92FEMKEYVAR2", schema = "RAW")) %>% 
    select(ID, DTINT92, SMK92, CA92) 
)

derived_1992_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY92MENKEYVAR2", schema = "RAW")) %>% 
    select(ID, DTINT92, SMK92, CA92) 
)

derived_1992<- rbind(derived_1992_women, derived_1992_men)
rm(derived_1992_women, derived_1992_men)

#1997
derived_1997_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY97FEMKEYVAR2", schema = "RAW")) %>% 
    select(ID, DTINT97, SMK97) 
)

derived_1997_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY97MENKEYVAR2", schema = "RAW")) %>% 
    select(ID, DTINT97, SMK97) 
)

derived_1997<- rbind(derived_1997_women, derived_1997_men)
rm(derived_1997_women, derived_1997_men)

reported_1997<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER97FEB2015REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_1997 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#1999
original_1999_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY99WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT99, CIGNOW99) 
)

original_1999_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY99MEN", schema = "RAW")) %>% 
    select(ID, DTINT99, CIGNOW99) 
)

original_1999<- rbind(original_1999_women, original_1999_men)
rm(original_1999_women, original_1999_men)

reported_1999<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER99SEP2007REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_1999 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

FFQ_1999_raw<- collect(
  tbl(scdbcon, DBI::Id(table = "CPS2_RAW_NUTRIENTS_1999", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      in_1999_FFQ=1
      )
)

#2001
original_2001_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY01WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT01, CIGNOW01) 
)

original_2001_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY01MEN", schema = "RAW")) %>% 
    select(ID, DTINT01, CIGNOW01) 
)

original_2001<- rbind(original_2001_women, original_2001_men)
rm(original_2001_women, original_2001_men)

reported_2001<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER01MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2001 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2003
original_2003_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY03WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT03) 
)

original_2003_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY03MEN", schema = "RAW")) %>% 
    select(ID, DTINT03) 
)

original_2003<- rbind(original_2003_women, original_2003_men)
rm(original_2003_women, original_2003_men)

reported_2003<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER03MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2003 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2005
original_2005_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY05WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT05) 
)

original_2005_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY05MEN", schema = "RAW")) %>% 
    select(ID, DTINT05) 
)

original_2005<- rbind(original_2005_women, original_2005_men)
rm(original_2005_women, original_2005_men)

reported_2005<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER05MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2005 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2007
original_2007_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY07WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT07) 
)

original_2007_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY07MEN", schema = "RAW")) %>% 
    select(ID, DTINT07) 
)

original_2007<- rbind(original_2007_women, original_2007_men)
rm(original_2007_women, original_2007_men)

reported_2007<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER07MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2007 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2009
original_2009_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY09WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT09) 
)

original_2009_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY09MEN", schema = "RAW")) %>% 
    select(ID, DTINT09) 
)

original_2009<- rbind(original_2009_women, original_2009_men)
rm(original_2009_women, original_2009_men)

reported_2009<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER09MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2009 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2011
original_2011_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY11WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT11) 
)

original_2011_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY11MEN", schema = "RAW")) %>% 
    select(ID, DTINT11) 
)

original_2011<- rbind(original_2011_women, original_2011_men)
rm(original_2011_women, original_2011_men)

reported_2011<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER11MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2011 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2013
original_2013_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY13WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT13) 
)

original_2013_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY13MEN", schema = "RAW")) %>% 
    select(ID, DTINT13) 
)

original_2013<- rbind(original_2013_women, original_2013_men)
rm(original_2013_women, original_2013_men)

reported_2013<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER13MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2013 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2015
original_2015_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY15WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT15) 
)

original_2015_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY15MEN", schema = "RAW")) %>% 
    select(ID, DTINT15) 
)

original_2015<- rbind(original_2015_women, original_2015_men)
rm(original_2015_women, original_2015_men)

reported_2015<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER15MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2015 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#2017
original_2017_women<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY17WOMEN", schema = "RAW")) %>% 
    select(ID, DTINT17) 
)

original_2017_men<- collect(
  tbl(scdbcon, DBI::Id(table = "SURVEY17MEN", schema = "RAW")) %>% 
    select(ID, DTINT17) 
)

original_2017<- rbind(original_2017_women, original_2017_men)
rm(original_2017_women, original_2017_men)

reported_2017<- collect(
  tbl(scdbcon, DBI::Id(table = "CANCER17MAY2021REPORTED", schema = "RAW")) %>% 
    select(ID) %>%
    mutate(
      reported_2017 = 1
    ) %>%
    group_by(ID) %>%
    slice_sample(n=1)
)

#Start merging everything with the virgin vial people then clean things up
all_survey_info<- plyr::join_all(list(CPS2_master, 
                                      CPS2_pyear, CPS2_verified, CPS2_mortality,
                                      CPS2_1982_original, CPS2_1982_derived, 
                                      derived_1992, derived_1997, reported_1997, 
                                      original_1999, reported_1999, FFQ_1999_raw,
                                      original_2001, reported_2001, 
                                      original_2003, reported_2003, 
                                      original_2005, reported_2005,
                                      original_2007, reported_2007,
                                      original_2009, reported_2009,
                                      original_2011, reported_2011,
                                      original_2013, reported_2013,
                                      original_2015, reported_2015,
                                      original_2017, reported_2017),
                                 by='ID', type='left') #dupes from verified currently

rm(CPS2_master, CPS2_pyear, CPS2_verified, CPS2_mortality, 
   CPS2_1982_original, CPS2_1982_derived, 
   derived_1992, derived_1997, reported_1997, derived_1999, original_1999, reported_1999, FFQ_1999_raw,
   original_2001, reported_2001, original_2003, reported_2003, 
   original_2005, reported_2005, original_2007, reported_2007, 
   original_2009, reported_2009, original_2011, reported_2011,
   original_2013, reported_2013, original_2015, reported_2015, original_2017, reported_2017)

#We need to format some dates and a few other cleaning tasks, also create COD vars
all_survey_info<- all_survey_info %>%
  mutate(
    across(
      c(BDAYDATE, DATEDD, starts_with('DTINT')),
      ~ as.Date(.x, origin="1960-01-01")
    ),
    dead = case_when(
      is.na(in_mortality) ~ 0,
      in_mortality==1 & DATEDD < as.Date('2020-12-31') ~ 1,
      TRUE ~ 0
    ),
    death_date = case_when(
      dead == 1 ~ DATEDD,
      TRUE ~ NA
    ),
    across(starts_with('reported_'), ~ifelse(is.na(.x), 0, .x)),
    in_verified = ifelse(is.na(in_verified), 0, in_verified),
    in_1999_FFQ = ifelse(is.na(in_1999_FFQ), 0, in_1999_FFQ)
  ) %>%
  select(-DATEDD, -in_mortality)


#Fix 1997 DXDATE issue
all_survey_info<- all_survey_info %>%
  mutate(
    DTINT97 = case_when(
      DTINT97 == as.Date('1998-01-01') & reported_1997 == 1 & diagnosis_date > DTINT97 ~ diagnosis_date + 1,
      TRUE ~ DTINT97
    )
  )

#clear all CODE variables for the people that aren't dead during mortality FU just to be safe
all_survey_info<- all_survey_info %>%
  mutate(
    across(
      starts_with('CODE'), 
      ~case_when(
        dead==0 ~ NA,
        TRUE ~ .x
      )
    )
  )

#Cause of death
all_survey_info<- all_survey_info %>%
  mutate(
    any_ca_death = case_when(
      dead==0 ~ 0,
      
      # Code 1
      (CODETYPE == 1 & CODE1 >= "31" & CODE1 <= "99") |
        (CODETYPE == 2 & 
           ((CODE1 >= "140" & CODE1 <= "2099") |
              (CODE1 >= "230" & CODE1 <= "2399"))) |
        (CODETYPE == 3 &
           ((CODE1 >= "C00" & CODE1 <= "D099") |
              (CODE1 >= "D37" & CODE1 <= "D499"))) ~ 1,
      
      # Code 2
      (CODETYPE == 1 & CODE2 >= "31" & CODE2 <= "99") |
        (CODETYPE == 2 & 
           ((CODE2 >= "140" & CODE2 <= "2099") |
              (CODE2 >= "230" & CODE2 <= "2399"))) |
        (CODETYPE == 3 &
           ((CODE2 >= "C00" & CODE2 <= "D099") |
              (CODE2 >= "D37" & CODE2 <= "D499"))) ~ 2,
      
      # Code 3
      (CODETYPE == 1 & CODE3 >= "31" & CODE3 <= "99") |
        (CODETYPE == 2 & 
           ((CODE3 >= "140" & CODE3 <= "2099") |
              (CODE3 >= "230" & CODE3 <= "2399"))) |
        (CODETYPE == 3 &
           ((CODE3 >= "C00" & CODE3 <= "D099") |
              (CODE3 >= "D37" & CODE3 <= "D499"))) ~ 3,
      
      TRUE ~ 0
    ),
    
    primary_ca_death = case_when(
      any_ca_death == 1 ~ 1,
      TRUE ~ 0
    )
  )

#Create some timing variables
all_survey_info<- all_survey_info %>%
  mutate(
    #create a reported cancer variable for the two years without individual ones
    reported_1982 = case_when(
      CA82 %in% c('N', '73') ~ 0,
      TRUE ~ 1
    ),
    reported_1992 = case_when(
      CA92>0 ~ 1,
      TRUE ~ 0
    ),
    #and a date we first got a report of cancer
    first_reported_date = case_when(
      reported_1982==1 ~ DTINT82,
      reported_1992==1 ~ DTINT92,
      reported_1997==1 ~ DTINT97,
      reported_1999==1 ~ DTINT99,
      reported_2001==1 ~ DTINT01,
      reported_2003==1 ~ DTINT03,
      reported_2005==1 ~ DTINT05,
      reported_2007==1 ~ DTINT07,
      reported_2009==1 ~ DTINT09,
      reported_2011==1 ~ DTINT11,
      reported_2013==1 ~ DTINT13,
      reported_2015==1 ~ DTINT15,
      reported_2017==1 ~ DTINT17,
      TRUE ~ NA
    ),
    last_cancer_report_free_date = case_when(
      reported_1982==1 ~ NA,
      reported_1992==1 ~ DTINT82,
      reported_1997==1 ~ pmax(DTINT82, DTINT92, na.rm=TRUE),
      reported_1999==1 ~ pmax(DTINT82, DTINT92, DTINT97, na.rm=TRUE),
      reported_2001==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, na.rm=TRUE),
      reported_2003==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, na.rm=TRUE),
      reported_2005==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, DTINT03, na.rm=TRUE),
      reported_2007==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, DTINT03, DTINT05, na.rm=TRUE),
      reported_2009==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, DTINT03, DTINT05, DTINT07, na.rm=TRUE),
      reported_2011==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, DTINT03, DTINT05, DTINT07, DTINT09, na.rm=TRUE),
      reported_2013==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, DTINT03, DTINT05, DTINT07, DTINT09, DTINT11, na.rm=TRUE),
      reported_2015==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, DTINT03, DTINT05, DTINT07, DTINT09, DTINT11, DTINT13, na.rm=TRUE),
      reported_2017==1 ~ pmax(DTINT82, DTINT92, DTINT97, DTINT99, DTINT01, DTINT03, DTINT05, DTINT07, DTINT09, DTINT11, DTINT13, DTINT15, na.rm=TRUE),
      TRUE~NA
    ),
    last_survey = case_when(
      NUT_17 == 1 ~ 2017,
      NUT_15 == 1 ~ 2015,
      NUT_13 == 1 ~ 2013,
      NUT_11 == 1 ~ 2011,
      NUT_09 == 1 ~ 2009,
      NUT_07 == 1 ~ 2007,
      NUT_05 == 1 ~ 2005,
      NUT_03 == 1 ~ 2003,
      NUT_01 == 1 ~ 2001,
      NUT_99 == 1 ~ 1999,
      NUT_97 == 1 ~ 1997,
      NUT_92 == 1 ~ 1992,
      TRUE ~ 1982
    ),
    last_survey_return_date = case_when(
      last_survey == 2017 ~ DTINT17,
      last_survey == 2015 ~ DTINT15,
      last_survey == 2013 ~ DTINT13,
      last_survey == 2011 ~ DTINT11,
      last_survey == 2009 ~ DTINT09,
      last_survey == 2007 ~ DTINT07,
      last_survey == 2005 ~ DTINT05,
      last_survey == 2003 ~ DTINT03,
      last_survey == 2001 ~ DTINT01,
      last_survey == 1999 ~ DTINT99,
      last_survey == 1997 ~ DTINT97,
      last_survey == 1992 ~ DTINT92,
      last_survey == 1982 ~ DTINT82
    )
  )

#create end of follow up dates
all_survey_info<- all_survey_info %>%
  mutate(
    end_follow_up_incidence = case_when(
      #follow to last survey returned or death if it's before the next survey
      dead == 0 ~ last_survey_return_date,
      last_survey == 1982 & death_date < as.Date('1992-12-31') ~ death_date,
      last_survey == 1992 & death_date < as.Date('1997-12-31') ~ death_date,
      last_survey == 1997 & death_date < as.Date('1999-12-31') ~ death_date,
      last_survey == 1999 & death_date < as.Date('2001-12-31') ~ death_date,
      last_survey == 2001 & death_date < as.Date('2003-10-31') ~ death_date,
      last_survey == 2003 & death_date < as.Date('2005-10-31') ~ death_date,
      last_survey == 2005 & death_date < as.Date('2007-10-31') ~ death_date,
      last_survey == 2007 & death_date < as.Date('2009-10-31') ~ death_date,
      last_survey == 2009 & death_date < as.Date('2011-10-31') ~ death_date,
      last_survey == 2011 & death_date < as.Date('2013-10-31') ~ death_date,
      last_survey == 2013 & death_date < as.Date('2015-10-31') ~ death_date,
      last_survey == 2015 & death_date < as.Date('2017-10-31') ~ death_date,
      last_survey == 2017 ~ last_survey_return_date, #censor at 2017 if they died after 2017 since there's no later survey on which to reporta cancer
      TRUE ~ last_survey_return_date #died, but not before they could have returned another survey
    ),
    #handle report censoring
    end_follow_up_incidence = case_when(
      #if not verified, go to last cancer free survey, if it's verified, keep normal end followup
      in_verified == 0 & !is.na(first_reported_date) ~ last_cancer_report_free_date,
      #should it be >180 days after report, it'll be excluded later in exclusions
      #should dxdate happen to be after last survey, it will be censored back to last known cancer free
      TRUE ~ end_follow_up_incidence
    ),
    end_follow_up_mortality = case_when(
      dead == 0 ~ as.Date('2020-12-31'),
      TRUE ~ death_date
    )
  )

#Create outcomes and start stop times
all_survey_info<- all_survey_info %>%
  mutate(
    incident_any_cancer = case_when(
      in_verified == 1 & diagnosis_date <= end_follow_up_incidence ~ 1,
      any_ca_death == 1 & death_date == end_follow_up_incidence ~ 1,
      TRUE ~ 0
    ),
    start_time = DTINT99,
    stop_time_incidence = case_when(
      incident_any_cancer == 1 ~ pmin(diagnosis_date, death_date, na.rm=TRUE),
      incident_any_cancer == 0 ~ end_follow_up_incidence
    ),
    stop_time_mortality = end_follow_up_mortality
  )

#create exclusions
all_survey_info<- all_survey_info %>%
  mutate(
    exclude_no_1999 = case_when(
      NUT_99 == 0 ~ 1,
      TRUE ~ 0,
    ),
    exclude_no_1999_FFQ = case_when(
      in_1999_FFQ == 0 ~ 1,
      TRUE ~ 0,
    ),
    exclude_prevalent_cancer = case_when(
      diagnosis_date <= DTINT99 ~ 1,
      reported_1982 == 1 | reported_1992 == 1 | reported_1997 == 1 | reported_1999 == 1 ~ 1,
      TRUE ~ 0
    ),
    exclude_endfu_before_1999 = case_when(
      #in the current order this is mostly people being censored back to 1999 for reported cancer
      stop_time_incidence <= DTINT99 | is.na(stop_time_incidence) ~ 1,
      TRUE ~ 0
    ),
    exclude_bad_diagnosis_date = case_when(
      as.numeric(difftime(diagnosis_date, first_reported_date, unit='days')) > 180 ~ 1,
      TRUE ~ 0
    )
  )

#create exclusion cascade
all_survey_info<- all_survey_info %>%
  mutate(
    exclusion_cascade_incidence = case_when(
      exclude_no_1999 == 1  ~ '01: No 1999',
      exclude_no_1999_FFQ == 1 ~'02: No 1999 FFQ',
      exclude_prevalent_cancer == 1 ~ '03: Prevalent Cancer',
      exclude_endfu_before_1999 == 1 ~ '04: End FU before 1999',
      exclude_bad_diagnosis_date == 1 ~ '05: Bad DX Date',
      TRUE ~ '99: Nonexcluded'
    )
  )

#Do some work on the cancer vars
all_survey_info<- all_survey_info %>%
  mutate(
    #if not an incident cancer make their cancer variables NA. I don't think we would use tumor info for the mortality study
    across(c(diagnosis_date, in_verified, HISTOGY, INCCANV, BEHVCODE, GSSTAGE, GRDI, SITEC),
           ~case_when(
             incident_any_cancer==0 ~ NA,
             TRUE ~ .x
      )
    ),
    case_source = case_when(
      incident_any_cancer==0 ~ 0, #not an incident cancer
      incident_any_cancer==1 & stop_time_incidence == diagnosis_date ~ 1, #from verified
      incident_any_cancer==1 & stop_time_incidence == death_date ~ 2 #death only
      #there are 18 with a dxdate after death date, should think about how to handle these
    )
    #eventually add code here to get a cancer type from the death only cases
    #and also possible assign diagnosis_date to people with death only cases, etc.
  )


table(all_survey_info$exclusion_cascade_incidence, useNA='ifany')
nonexcluded<- all_survey_info %>% filter(exclusion_cascade_incidence == '99: Nonexcluded')

#for proposal:
table(nonexcluded$incident_any_cancer, useNA='ifany') #26,322
table(nonexcluded$dead, useNA='ifany') #68,804
table(nonexcluded$any_ca_death, useNA='ifany') #15,808 all cancer, 14,115 primary cancer
cases<- nonexcluded %>% filter(incident_any_cancer==1)
table(cases$INCCANV %in% c('C18', 'C19', 'C20', 'C50', 'C61', 'C33', 'C34'), useNA='ifany') #14,343

#random scratch work
table(nonexcluded$incident_any_cancer, nonexcluded$INCCANV, useNA='ifany')
table(nonexcluded$incident_any_cancer, nonexcluded$case_source, useNA='ifany')
table(cases$diagnosis_date > cases$stop_time_incidence, useNA='ifany')
table(cases$INCCANV, useNA='ifany')
table(cases[cases$dead==0,]$CODE1, useNA='ifany')
noncases<- nonexcluded %>% filter(incident_any_cancer==0)
table(noncases$INCCANV, useNA='ifany')
table(noncases$case_source, useNA='ifany')
table(noncases[noncases$dead==0,]$CODE1, useNA='ifany')
death_only<- nonexcluded %>% filter(case_source==2)
table(death_only$CODE1)
table(death_only$any_ca_death)

# obesity_related_cancer = case_when(
#   #need to handle heme before this is final
#   incident_any_cancer==1 & INCCANV %in% c('C18', 'C19', 'C20', 'C50', 'C61', 'C33', 'C34') ~ 1,
#   incident_any_cancer==1 ~ 0,
#   incident_any_cancer==0 ~ NA
# )