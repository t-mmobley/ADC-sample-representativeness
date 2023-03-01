#- Loading packages, options and initializations -----------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "magrittr", "foreign", "tidyverse","ggplot2",
       "srvyr", "tableone", "mice", "table1", "openxlsx", "twang")

options(scipen = 100)

# Read ----------------------------------------------------------------
clean_data <- readRDS(paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/Data/",
                             "nacchrs_harmonized.RDS"))

# add labels
label(clean_data$female)        <- "Sex"
label(clean_data$agetv)         <- "Age"
label(clean_data$raceth)        <- "Race/Ethnicity"
label(clean_data$marital)       <- "Married/living as if married"
label(clean_data$educyr)        <- "Educational attainment"
label(clean_data$htn)           <- "Hypertension/high bp"
label(clean_data$diab)          <- "Diabetes/high blood sugar"
label(clean_data$depsymp)       <- "Depressive symptoms"
label(clean_data$srmem)         <- "Low subjective memory function"
label(clean_data$vision_diff)   <- "SR vision dificulty"
label(clean_data$hearing_diff)  <- "SR hearing difficulty"

#Final list of harmonized variables
harmonized<-
  colnames(clean_data)[!(colnames(clean_data) %in% 
                           c("id", "study", "imp", "nacc_h", "hrs_h", 
                             "nacc_visit", "hrs_firstiw", "rwtresp"))]

#Calculate mean of harmonizable variables across all imputations (for table 1)
temp<-bal.stat(data=clean_data, 
               vars=harmonized,
               w.all=clean_data$rwtresp,
               treat.var='hrs_h', 
               sampw=clean_data$rwtresp, 
               estimand="ATT", 
               multinom = F)

tab1_MIcomb<-temp$results
tab1_MIcomb$var<-rownames(tab1_MIcomb)

# Calculate sample sizes
nacc_tot <- (clean_data %>% filter(study=="NACC") %>% count())/20
nacc_tot # 36639
hrs_tot <- clean_data %>% filter(study=="HRS" & imp==1) 
sum(hrs_tot$rwtresp) #52,071,840

# Check proportion calculations
nacc_raceth_prop <- clean_data %>% filter(study=="NACC") %>%
  group_by(raceth) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) 

hrs_raceth_prop <- clean_data %>% filter(study=="HRS") %>%
  group_by(raceth) %>%
  summarise(raceth_tot = sum(rwtresp),
            freq = raceth_tot/(52071840*20))

#Save results For Table 1
write.xlsx(tab1_MIcomb, file =paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                                     "Output/tab1_MIcomb.xlsx"))
