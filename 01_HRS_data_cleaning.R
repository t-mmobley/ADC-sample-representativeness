# Loading packages, options and initialization --------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "survey", "tableone", "mice", "openxlsx")

options(scipen = 100)

# Read ------------------------------------------------------------------

hrs_raw<-read_dta("C:/Users/tmobley/Box/HRS Sensitive Health/hrs210826.dta")

# filter datasets based on variables on want
hrs_filter <- 
  hrs_raw %>% 
  dplyr::select(c('hhid', 'pn', 'year', 'hhidpn', 'rwtresp', 
        'firstiw', 'year', 'age', 'f236', 'f234', 'rsmoken', 'rsmokev',
        'rdrink', 'c103', 'c097', 'c096', 'c095', 'rpmbmi', 'rbmi', 
        'iwlang', 'rmo','rimrc','rdy', 'rdiabe', 'rhibpe', 'rmstat',
        'raedyrs', 'hispanic', 'rahispan', 'race', 'gender', 'radla',
        'rdepres', 'rcesd', 'riadla', 'ryr', 'rdw', 'rdlrc', 'rslfmem', 
        'apoe', 'apoe4', 'APOE_Imputed'))


# look at the data
# colnames(hrs_filter)
# table(hrs_filter$firstiw, exclude=NULL)
# summary(hrs_filter$age)

# id and study vars --------------------------------------------------

# Create study id variable for HRS
hrs_filter$study <- 'HRS'
hrs_filter$id <- as.character(hrs_filter$hhidpn)

# HRS data cleaning --------------------------------------------------

# demographic vars ---

# age
# summary(hrs_filter$agetv)
hrs_filter <- hrs_filter %>% 
  rename(agetv=age)

# sex/gender
# table(hrs_filter$gender, exclude=NULL)
hrs_filter$female <- ifelse(hrs_filter$gender==2,1,
                             ifelse(is.na(hrs_filter$gender),NA,0))

# race/ethnicity
table(hrs_filter$rahispan, hrs_filter$race, exclude=NULL)

# currently coded hispanic=1, white=2, black=3, other=4 
hrs_filter$raceth <- 
  ifelse(hrs_filter$race==1 & 
           (hrs_filter$rahispan==0 | is.na(hrs_filter$rahispan)),2,
         ifelse(hrs_filter$race==2 &
                  (hrs_filter$rahispan==0 | is.na(hrs_filter$rahispan)),3,
                ifelse(hrs_filter$race==7 &
                        (hrs_filter$rahispan==0 | is.na(hrs_filter$rahispan)),4, 
                       ifelse(hrs_filter$rahispan==1,1,NA))))
# table(hrs_filter$raceth, exclude=NULL)

# education
# table(hrs_filter$raedyrs, exclude=NULL) # top-coded at 17 years

hrs_filter <- hrs_filter %>%
  rename(educyr=raedyrs)

# marital status
# table(hrs_filter$rmstat, exclude=NULL)

hrs_filter <- hrs_filter %>%
  rename(marital=rmstat)

# physical function ---

# vision
# table(hrs_filter$c095, exclude=NULL)

hrs_filter <- hrs_filter %>%
  rename(vision = c095)

# hearing 
# table(hrs_filter$c103, exclude=NULL)

hrs_filter <- hrs_filter %>%
  rename(hearing = c103)

# sr memory 
# table(hrs_filter$rslfmem, exclude=NULL)

hrs_filter <- hrs_filter %>%
  rename(srmem = rslfmem)

# health vars ---

# htn/high bp: note, HRS asks if ever had high BP, NACC asks sr htn 
# table(hrs_filter$rhibpe, exclude=NULL)

hrs_filter <- hrs_filter %>%
  rename(htn=rhibpe)

# diabetes/high blood sugar
# table(hrs_filter$rdiabe, exclude=NULL)

hrs_filter <- hrs_filter %>%
  rename(diab=rdiabe)

# depression: CESD in HRS
# table(hrs_filter$rcesd, exclude=NULL)

hrs_filter <- hrs_filter %>%
  rename(depsymp=rcesd)

# HRS analytic sample creation ----------------------------------------

# 1. subset to HRS 2010 wave + 60+ age restriction
# check distribution
# table(hrs_filter$year, exclude=NULL) #19235 in 2010

hrs_clean <- hrs_filter %>% 
  filter((year==2010))

# check year distribution
# table(hrs_clean$year, exclude=NULL)

# age distribution
summary(hrs_clean$agetv, exclude=NULL)
hrs_clean %>% filter(agetv <60) %>% count() #6413 <60 years old

hrs_clean <- hrs_clean %>% 
  filter(agetv >= 60) #n=12822

# summary(hrs_clean$agetv, exclude=NULL)

# 2. exclude missing and "other race/ethnicity" group
hrs_clean %>% group_by(raceth) %>% count() #294 4 or NA

hrs_clean <- hrs_clean %>% filter(raceth %in% c(1,2,3)) #n=12528

# 6. keep people with non-zero weights: n=454 
# summary(hrs_clean$rwtresp)
# hrs_clean %>% filter(rwtresp==0) %>% count() #454
# hrs_clean %>% filter(is.na(rwtresp)) %>% count() #0

hrs_clean <- hrs_clean %>% filter(rwtresp>0) #12074

# check parental dem hx vars -- too much missingness
# table(hrs_clean$f234, exclude=NULL)
# table(hrs_clean$f236, exclude=NULL)

# filter columns of interest
hrs_clean <- hrs_clean %>%
  select(id, study, firstiw, rwtresp, year, agetv, female, raceth, educyr, 
         marital, vision, hearing, srmem, htn, diab, depsymp)

# saving
saveRDS(hrs_clean, paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                           "Data/hrs_clean.RDS"))

