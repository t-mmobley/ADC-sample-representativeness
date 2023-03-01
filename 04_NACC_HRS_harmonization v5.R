# Loading packages, options and initializations ------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse","ggplot2","survey", "tableone", "mice", "openxlsx")

# Read -----------------------------------------------------------------

hrs_imp <- readRDS(paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                            "Data/hrs_analysis_pmm.RDS"))
nacc_imp <- readRDS(paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                            "Data/nacc_analysis_pmm.RDS"))

# stack data sets
hrs_imp$srmem <- factor(hrs_imp$srmem, ordered=F)
stacked <- bind_rows(hrs_imp, nacc_imp)

# harmonize vars -------------------------------------------------------

# initial vars  
harmonize_clean<-data.frame(id=stacked$id)
harmonize_clean$study <- stacked$study
harmonize_clean$imp <- stacked$imp
harmonize_clean$nacc_h <- ifelse(stacked$study=='NACC',1,0)
harmonize_clean$hrs_h <- ifelse(stacked$study=='HRS',1,0)

harmonize_clean$nacc_visit <- stacked$naccvnum
harmonize_clean$nacc_visityr <- stacked$visityr
harmonize_clean$hrs_firstiw <- stacked$firstiw
harmonize_clean$rwtresp <- 
  ifelse(stacked$study=='HRS',stacked$rwtresp,
         ifelse(stacked$study=='NACC',1,NA))

  # summary(harmonize_clean$rwtresp)

# physical function vars ----------------------------------------------

# vision difficulty
# table(stacked$vision_diff, stacked$study, exclude=NULL)
# table(stacked$vision, stacked$study, exclude=NULL)

harmonize_clean$vision_diff <- 
  ifelse(stacked$study=='NACC' & stacked$vision_diff==1,1,
         ifelse(stacked$study=='NACC' & stacked$vision_diff==0,0,
            ifelse(stacked$study=='HRS' & stacked$vision %in% c(1,2,3,4),0,
                ifelse(stacked$study=='HRS' & 
                         stacked$vision %in% c(5,6),1,NA)))) 

  # table(harmonize_clean$vision_diff, harmonize_clean$study, exclude=NULL)

# hearing difficulty
# table(stacked$hearing_diff, stacked$study, exclude=NULL)
# table(stacked$hearing, stacked$study, exclude=NULL)

harmonize_clean$hearing_diff <- 
  ifelse(stacked$study=='NACC' & stacked$hearing_diff==1,1,
         ifelse(stacked$study=='NACC' & stacked$hearing_diff==0,0,
                ifelse(stacked$study=='HRS' & stacked$hearing %in% c(1,2,3,4),0,
                       ifelse(stacked$study=='HRS' & 
                                stacked$hearing==5,1,NA)))) 

  # table(harmonize_clean$hearing_diff, harmonize_clean$study, exclude=NULL)

# sr memory 
# table(stacked$srmem, stacked$study, exclude=NULL)

harmonize_clean$srmem <- 
  ifelse(stacked$study=='NACC' & stacked$srmem==1,1,
    ifelse(stacked$study=='NACC' & stacked$srmem==0,0,
      ifelse(stacked$study=='HRS' & stacked$srmem %in% c(1,2,3),0,
       ifelse(stacked$study=='HRS' & stacked$srmem %in% c(4,5),1,NA))))

  # table(harmonize_clean$srmem, harmonize_clean$study, exclude = NULL)

# demographics --------------------------------------------------------

# age
  # summary(stacked$agetv)

harmonize_clean$agetv <- stacked$agetv

  # summary(harmonize_clean$agetv)

# sex/gender
  # table(stacked$female, stacked$study, exclude=NULL)

harmonize_clean$female <- stacked$female

  # table(harmonize_clean$female, harmonize_clean$study, exclude=NULL)

# race/ethnicity
  # table(stacked$raceth, stacked$study, exclude=NULL)
 
# currently coded hispanic=1, white=2, black=3, other=4 
harmonize_clean$raceth <- stacked$raceth
  
  # table(harmonize_clean$raceth, harmonize_clean$study, exclude=NULL)

# years of education
  # table(stacked$educyr, stacked$study, exclude=NULL)
  
harmonize_clean$educyr <- 
  ifelse(stacked$study=='HRS',stacked$educyr,
    ifelse(stacked$study=='NACC' & 
             stacked$educ >= 0 & stacked$educ <= 17,stacked$educyr,
           ifelse(stacked$study=='NACC' & 
                    stacked$educyr >= 17 & stacked$educyr <= 30,17,NA)))

  # table(harmonize_clean$educyr, harmonize_clean$study, exclude=NULL)

#- marital status
  # table(stacked$marital, stacked$study, exclude=NULL)

harmonize_clean$marital <- 
  ifelse(harmonize_clean$study=='NACC' & 
          stacked$marital %in% c(1,6),1,
    ifelse(harmonize_clean$study=='NACC' & 
          stacked$marital %in% c(2,3,4,5),0,     
  ifelse(harmonize_clean$study=='HRS' & stacked$marital %in% c(1,2,3),1,
    ifelse(harmonize_clean$study=='HRS' & 
             stacked$marital %in% c(4,5,6,7,8),0,NA))))

  # table(harmonize_clean$marital, harmonize_clean$study, exclude=NULL)

# health vars ---------------------------------------------------------
  
# htn/high bp: note, HRS asks if ever had high BP, NACC asks sr htn 
  table(stacked$htn, stacked$study, exclude=NULL)

harmonize_clean$htn <- 
  ifelse(harmonize_clean$study=='NACC' & stacked$htn %in% c(1,2),1,
         ifelse(harmonize_clean$study=='NACC' & stacked$htn==0,0,
                ifelse(harmonize_clean$study=='HRS' &  stacked$htn==1,1,
                       ifelse(harmonize_clean$study=='HRS' &  stacked$htn==0,0,
                              NA))))

  # table(harmonize_clean$htn, harmonize_clean$study, exclude=NULL)


# diabetes/high blood sugar
  # table(stacked$diab, stacked$study, exclude=NULL)

harmonize_clean$diab <- 
  ifelse(harmonize_clean$study=='NACC' & stacked$diab %in% c(1,2),1,
         ifelse(harmonize_clean$study=='NACC' & stacked$diab==0,0,
                ifelse(harmonize_clean$study=='HRS' &  stacked$diab==1,1,
                       ifelse(harmonize_clean$study=='HRS' &  
                                stacked$diab==0,0,NA))))

  # table(harmonize_clean$diab, harmonize_clean$study, exclude=NULL)

# depression: CESD in HRS, GDS in NACC, respective cutoffs used
# table(stacked$depsymp, stacked$study, exclude=NULL)

harmonize_clean$depsymp <-
  ifelse(harmonize_clean$study=='NACC' & 
           (stacked$depsymp >=0 & stacked$depsymp <9),0,
  ifelse(harmonize_clean$study=='NACC' & 
           (stacked$depsymp >=9 & stacked$depsymp <= 15),1,
         ifelse(harmonize_clean$study=='HRS' & stacked$depsymp <4,0,
                ifelse(harmonize_clean$study=='HRS' & 
                         (stacked$depsymp >= 4 & stacked$depsymp <=8),1,
                       NA))))

  # table(harmonize_clean$depsymp, harmonize_clean$study, exclude=NULL)

#---- save harmonized dataframe ----
saveRDS(harmonize_clean, file=paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/Data/",
                             "nacchrs_harmonized.RDS")) 
