# Loading packages, options and initializations -----------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("tidyverse","ggplot2","survey", "tableone", "mice", "openxlsx")

# Read HRS -----------------------------------------------------------

hrs_clean <- readRDS(paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                            "Data/hrs_clean.RDS"))

str(hrs_clean)
vars<-colnames(hrs_clean)

#Assess missingness in vars we want to harmonize on / use for analysis
harmonizable.vars<-c("agetv", "female", "raceth", "marital", "educyr", "vision",
                     "hearing", "srmem", "htn", "diab", "depsymp")

missingpattern<-md.pattern(hrs_clean[,harmonizable.vars], plot=F)
missingpattern

#Assess missingness in vars we want to impute / use for analysis
missingsummary <- data.frame(varname = harmonizable.vars, pctmiss = NA)
row.names(missingsummary) <- harmonizable.vars
for (i in harmonizable.vars){
  missingsummary[i, "pctmiss"] <- 100*sum(is.na(hrs_clean[, i]))/nrow(hrs_clean)
  
  print(i)
  print(table(hrs_clean[, i], exclude = NULL))
}

missingordered <- missingsummary[order(missingsummary$pctmiss), ]
missingordered

# save HRS missingess
write.xlsx(missingordered, file =paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                                     "Output/HRS_missingordered.xlsx"))

ordered.var.list <- c(paste(missingordered$varname))

# data prep -----------------------------------------------------------
# Run single imputation
# prep data by dropping vars we don't need, ordering by missingness
impute.hrs.data <- hrs_clean[, ordered.var.list]

# Set variable classes by type
# continuous vars
impute.hrs.data$agetv <- as.numeric(impute.hrs.data$agetv)
impute.hrs.data$educyr <- as.numeric(impute.hrs.data$educyr)
impute.hrs.data$depsymp <- as.numeric(impute.hrs.data$depsymp)

# binary vars
impute.hrs.data$female <- as.factor(impute.hrs.data$female)
impute.hrs.data$htn <- as.factor(impute.hrs.data$htn)
impute.hrs.data$diab <- as.factor(impute.hrs.data$diab)

# categorical vars
impute.hrs.data$raceth <- factor(impute.hrs.data$raceth, ordered = F)
impute.hrs.data$marital <- factor(impute.hrs.data$marital, ordered = F)

# ordinal vars
impute.hrs.data$vision <- factor(impute.hrs.data$vision, ordered = T)
impute.hrs.data$hearing <- factor(impute.hrs.data$hearing, ordered = T)
impute.hrs.data$srmem <- factor(impute.hrs.data$srmem, ordered = T)

# recheck classes
str(impute.hrs.data)

# Initiate imputations
ini<-mice(impute.hrs.data, maxit=0, 
          defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

ini$method
meth<-ini$method
meth

ini$predictorMatrix
pred<-ini$predictorMatrix

# Run imputations
hrs_pmm_all<-mice(impute.hrs.data, m=20, maxit=10, pred=pred, meth=meth, 
                  defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

# examine diagnostics
hrs_pmm_all$loggedEvents
plot(hrs_pmm_all)
densityplot(hrs_pmm_all, ~educyr)
densityplot(hrs_pmm_all, ~depsymp)

#Save final imputed datasetss
temp_HRS<-list()
for (i in 1:20){
  temp_HRS[[i]]<-complete(hrs_pmm_all,action=i)
  
  temp_HRS[[i]]<-cbind(hrs_clean$id,hrs_clean$study, hrs_clean$firstiw,
                       hrs_clean$rwtresp, temp_HRS[[i]][,harmonizable.vars])
  temp_HRS[[i]][,"imp"]<-i
}

hrs_analysis_pmm<-do.call(rbind,temp_HRS)

hrs_analysis_pmm <- hrs_analysis_pmm %>%
  rename(id=`hrs_clean$id`,
         study=`hrs_clean$study`, 
         firstiw=`hrs_clean$firstiw`,
         rwtresp=`hrs_clean$rwtresp`)

saveRDS(hrs_analysis_pmm, 
     file=paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                 "Data/hrs_analysis_pmm.RDS"))

# Read nacc -----------------------------------------------------------

nacc_clean <- readRDS(paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/Data/",
                             "nacc_clean.RDS"))

str(nacc_clean)
vars<-colnames(nacc_clean)

#Assess missingness in vars we want to harmonize on / use for analysis
nacc.harmoniz.vars<-c("agetv", "female", "raceth", "marital", "educyr",
                     "vision_diff", "hearing_diff", "srmem", "htn",
                     "diab", "depsymp")

missingpattern<-md.pattern(nacc_clean[,nacc.harmoniz.vars], plot=F)
missingpattern

#Assess missingness in vars we want to impute / use for analysis
missingsummary <- data.frame(varname = nacc.harmoniz.vars, pctmiss = NA)
row.names(missingsummary) <- nacc.harmoniz.vars
for (i in nacc.harmoniz.vars){
  missingsummary[i, "pctmiss"] <- 
    100*sum(is.na(nacc_clean[, i]))/nrow(nacc_clean)
  
  print(i)
  print(table(nacc_clean[, i], exclude = NULL))
}

missingordered <- missingsummary[order(missingsummary$pctmiss), ]
missingordered

# save NACC missingess
write.xlsx(missingordered, file =paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                                        "Output/NACC_missingordered.xlsx"))

ordered.var.list <- c(paste(missingordered$varname))

# data prep -----------------------------------------------------------
# Run single imputation
# prep data by dropping vars we don't need, ordering by missingness
impute.nacc.data <- nacc_clean[, ordered.var.list]

# Set variable classes by type
# continuous vars
impute.nacc.data$agetv <- as.numeric(impute.nacc.data$agetv)
impute.nacc.data$educyr <- as.numeric(impute.nacc.data$educyr)
impute.nacc.data$depsymp <- as.numeric(impute.nacc.data$depsymp)

# binary vars
impute.nacc.data$female <- as.factor(impute.nacc.data$female)
impute.nacc.data$htn <- as.factor(impute.nacc.data$htn)
impute.nacc.data$diab <- as.factor(impute.nacc.data$diab)
impute.nacc.data$vision_diff <- factor(impute.nacc.data$vision_diff)
impute.nacc.data$hearing_diff <- factor(impute.nacc.data$hearing_diff)
impute.nacc.data$srmem <- factor(impute.nacc.data$srmem)

# categorical vars
impute.nacc.data$raceth <- factor(impute.nacc.data$raceth, ordered = F)
impute.nacc.data$marital <- factor(impute.nacc.data$marital, ordered = F)

# recheck classes
str(impute.nacc.data)

# Initiate imputations
ini<-mice(impute.nacc.data, maxit=0, 
          defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

ini$method
meth<-ini$method
meth

ini$predictorMatrix
pred<-ini$predictorMatrix

# Run imputations
nacc_pmm_all<-mice(impute.nacc.data, m=20, maxit=10, pred=pred, meth=meth, 
                  defaultMethod = c("pmm", "pmm", "pmm", "pmm"), seed=12345)

# examine diagnostics
nacc_pmm_all$loggedEvents
plot(nacc_pmm_all)
densityplot(nacc_pmm_all, ~educyr)
densityplot(nacc_pmm_all, ~depsymp)

#Save final imputed datasetss
temp_nacc<-list()
for (i in 1:20){
  temp_nacc[[i]]<-complete(nacc_pmm_all,action=i)
  
  temp_nacc[[i]]<-cbind(nacc_clean$id,nacc_clean$study, nacc_clean$naccvnum,
                        temp_nacc[[i]][,nacc.harmoniz.vars])
  temp_nacc[[i]][,"imp"]<-i
}

nacc_analysis_pmm<-do.call(rbind,temp_nacc)

nacc_analysis_pmm <- nacc_analysis_pmm %>%
  rename(id=`nacc_clean$id`,
         study=`nacc_clean$study`, 
         naccvnum=`nacc_clean$naccvnum`)

saveRDS(nacc_analysis_pmm, 
        file=paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                    "Data/nacc_analysis_pmm.RDS"))
