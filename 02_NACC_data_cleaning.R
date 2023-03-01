# Loading packages, options and initialization --------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx")

options(scipen = 100)

# Read ------------------------------------------------------------------

nacc_raw<-read_csv(paste0("C:/Users/tmobley/Box/G3N_NACC_HRS/Data/Raw NACC/",
                          "investigator_nacc54.csv"))

# filter based on variables we want
# Note: TMM added "DEMENTED" to get freq of analytic sample 9/22/22
nacc_filter <- 
  nacc_raw %>% dplyr::select(c('NACCID','NACCAGE','NACCVNUM','VISITDAY',
                  'VISITMO','VISITYR', 'VISION', 'VISCORR', 
                  'VISWCORR','HEARING','HEARAID','HEARWAID', 
                  'DECSUB','NACCDAD', 'NACCMOM', 'NACCAPOE', 
                  'QUITSMOK','ALCFREQ','HYPERTEN','NACCNIHR',
                  'MARISTAT', 'HISPANIC', 'SEX', 'EDUC',
                  'DIABETES','NACCGDS','NACCBMI', 'ALCOCCAS',
                  'NACCBVFT', 'DOWNS', 'NACCPPA', 'HUNT',
                  'PRION', 'NACCADMU', 'PARK', 'NACCLBDS',
                  'NACCFTDM', 'OTHMUT', 'OTHMUTX', 'DEMENTED'))

# formatting column names
colnames(nacc_filter) <- tolower(colnames(nacc_filter))

# look at the data
# colnames(nacc_filter)
# table(nacc_filter$study, exclude=NULL)
# table(nacc_filter$visit, exclude=NULL)

# id and study vars --------------------------------------------------

# Create study id variable for HRS
nacc_filter$study <- 'NACC'
nacc_filter$id <- as.character(nacc_filter$naccid)

# NACC data cleaning --------------------------------------------------

# demographic vars ---

# age
# summary(nacc_filter$agetv)
nacc_filter <- nacc_filter %>% 
  rename(agetv=naccage)

# sex/gender
# table(nacc_filter$female, exclude=NULL)
nacc_filter$female <- ifelse(nacc_filter$sex==2,1,
                             ifelse(is.na(nacc_filter$sex),NA,0))

# race/ethnicity
# table(nacc_filter$naccnihr, nacc_filter$hispanic, exclude=NULL)

# currently coded hispanic=1, white=2, black=3, other=4 
nacc_filter$raceth <- 
  ifelse(nacc_filter$naccnihr==1 & 
           (nacc_filter$hispanic==0 | nacc_filter$hispanic==9),2,
         ifelse(nacc_filter$naccnihr==2 & 
                  (nacc_filter$hispanic==0 | nacc_filter$hispanic==9),3,
                ifelse(nacc_filter$naccnihr %in% c(3,4,5,6) &
                         (nacc_filter$hispanic==0 | nacc_filter$hispanic==9),4, 
                       ifelse(nacc_filter$hispanic==1,1,NA))))
# table(nacc_filter$raceth, exclude=NULL)

# education
# table(nacc_filter$educ, exclude=NULL)

nacc_filter$educyr <- 
  ifelse(nacc_filter$educ==99,NA,nacc_filter$educ)
  # table(nacc_filter$educyr, exclude=NULL)

# marital status
# table(nacc_filter$maristat, exclude=NULL)

nacc_filter$marital <-  
  ifelse(nacc_filter$maristat==9,NA,nacc_filter$maristat)
# table(nacc_filter$marital, exclude=NULL)

# physical function ---

# vision diff
# table(nacc_filter$viscorr,nacc_filter$vision, exclude=NULL)
# table(nacc_filter$viscorr,nacc_filter$viswcorr, exclude=NULL)

nacc_filter$vision_diff <- 
  ifelse(nacc_filter$viscorr==1 & nacc_filter$viswcorr==1,0,
         ifelse(nacc_filter$viscorr==1 & nacc_filter$viswcorr==0,1,
                ifelse(nacc_filter$viscorr==0 & nacc_filter$vision==1,0,
                       ifelse(nacc_filter$viscorr==0 & 
                                nacc_filter$vision==0,1,NA))))

# table(nacc_filter$vision_diff, exclude=NULL)

# hearing diff
# table(nacc_filter$hearaid,nacc_filter$hearing, exclude=NULL)
# table(nacc_filter$hearaid,nacc_filter$hearwaid, exclude=NULL)

nacc_filter$hearing_diff <- 
  ifelse(nacc_filter$hearaid==1 & nacc_filter$hearwaid==1,0,
         ifelse(nacc_filter$hearaid==1 & nacc_filter$hearwaid==0,1,
                ifelse(nacc_filter$hearaid==0 & nacc_filter$hearing==1,0,
                       ifelse(nacc_filter$hearaid==0 & 
                                nacc_filter$hearing==0,1,NA))))

# table(nacc_filter$hearing_diff, exclude=NULL)

# sr memory 
# table(nacc_filter$decsub, exclude=NULL) 
# 9 -> NA, 8 not assessed/too impaired -> NA

nacc_filter$srmem <- 
  ifelse(nacc_filter$decsub %in% c(0,1),nacc_filter$decsub,NA)

# table(nacc_filter$srmem, exclude = NULL)

# health vars ---

# htn/high bp: note, HRS asks if ever had high BP, NACC asks sr htn 
# table(nacc_filter$hyperten, exclude=NULL) # 9,-4 should be NA

nacc_filter$htn <- 
  ifelse(nacc_filter$hyperten %in% c(-4,9),NA,nacc_filter$hyperten)

# diabetes/high blood sugar
# table(nacc_filter$diabetes, exclude=NULL)

nacc_filter$diab <- 
  ifelse(nacc_filter$diabetes %in% c(-4,9),NA,nacc_filter$diabetes)

# depression: GDS in NACC
# table(nacc_filter$naccgds, exclude=NULL)

nacc_filter$depsymp <-
  ifelse(nacc_filter$naccgds %in% c(-4,88),NA,nacc_filter$naccgds)

# NACC analytic sample creation ---------------------------------------
# age restriction to 60+ in NACC
# exclude D mutation, FTLD mutation, down syndrome, Huntington's disease
# in line with the Gleason paper

# 1. First visit in NACC and Age restriction to 60+ in NACC

  # check number of first visits -- 43999 and no missing in NACC
  # table(nacc_filter$naccvnum, exclude=NULL)

nacc_clean <- nacc_filter %>% filter(naccvnum==1)

  # checks
  # table(nacc_clean$naccvnum, exclude=NULL) # visit 1 for all
  # table(nacc_clean$visityr, exclude=NULL) # 2005-2021

  # check number of age < 60 to drop -- 5136
  # summary(nacc_clean$agetv)
  # nacc_clean %>% filter(agetv<60) %>% count()

nacc_clean <- nacc_clean %>% filter(!(agetv <60)) #n=38863

# 2. exclude missing and "other race/ethnicity" group
# nacc_clean %>% group_by(raceth) %>% count() #2122 4 or NA

nacc_clean <- nacc_clean %>% filter(raceth %in% c(1,2,3)) #n=36741

  # table(nacc_clean$raceth, exclude=NULL) 

# 3. exclude clinical diagnoses in NACC -- AD mutation, FTLD mutation, 
# down syndrome, huntingtion's disease

# number of NACC participants with dxs
  # table(nacc_clean$naccadmu,exclude=NULL) #31
  # table(nacc_clean$downs, exclude=NULL) #1
  # table(nacc_clean$hunt,exclude=NULL) #4
  # table(nacc_clean$naccftdm, exclude=NULL) #78

nacc_clean <- nacc_clean %>% 
  filter((!(naccftdm==1) & !(downs==1) & !(hunt==1) & !(naccadmu==1))) #n=36639

# Numer of pts with dementia -- 12600/36639 = 34.4% [TMM added 9/22/22]
# table(nacc_clean$demented, exclude=NULL)
# table(nacc_clean$demented, nacc_clean$raceth, exclude=NULL)

# filter columns of interest
nacc_clean <- nacc_clean %>%
  select(id, study, naccvnum, visityr, agetv, female, raceth, educyr, marital,
         vision_diff, hearing_diff, srmem, htn, diab, depsymp)

# saving
saveRDS(nacc_clean, paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                           "Data/nacc_clean.RDS"))
