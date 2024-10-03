#######################################################################
########## Process and merge data to generate dataset for ML ########## 
#######################################################################


#### setups ####

options(scipen = 999)
# empty work space
rm(list = ls())

# define directory
dir <- getwd()

# load libraries
library(dplyr)

#### process teacher-level data ####

# load in data #
teach <- read.csv("data_raw/GTI-Teacher-Data.csv")
# tl <- read.csv("data_raw/GTI-TeachLog-Data.csv")

# countries to exclude
countries <- c("Shanghai", "Madrid")

# remove data from country
teach <- teach %>% 
  filter(!COUNTRY %in% countries)

# replace all 9999 with NA
teach <- teach %>%
  mutate(across(where(is.integer), ~na_if(., 9999))) %>% # missing
  mutate(across(where(is.integer), ~na_if(., 9998))) %>% # multiple responses
  mutate(across(where(is.integer), ~na_if(., 9997))) # Illegible response

# get relevant variable names: video observations
misc <- xlsx::read.xlsx(file = "misc/voi_v3.xlsx", sheetName = "Video", header = T)
vid_cols <- misc$Variable

# reduce teach to relevant variables only
rel_cols <- c("T_ID", vid_cols)
teach <- teach[, rel_cols]

# omit any teachers that have no classroom observation ratings
teach <- teach[!is.na(teach$VDOMAIN_CM),] # this excludes T_ID == "00826-0070-0002"

# aggregate all Class Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXCT", 1:8, "PCT"))]
tmpp$sum <- rowSums(teach[, c(paste0("VIND_CEXCT", 1:8, "PCT"))] > 0)
teach$VIND_CEXCT_SUM <- rowSums(teach[, c(paste0("VIND_CEXCT", 1:8, "PCT"))] > 0)

# aggregate GENERAL Class Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXCT", c(1, 2, 5, 6, 7, 8), "PCT"))]
tmpp$sum <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXCT", c(1, 2, 5, 6, 7, 8), "PCT"))] > 0) > 0)
teach$VIND_CEXCT_general <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXCT", c(1, 2, 5, 6, 7, 8), "PCT"))] > 0) > 0)

# aggregate MATHS Class Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXCT", c(3, 4), "PCT"))]
tmpp$sum <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXCT", c(3, 4), "PCT"))] > 0) > 0)
teach$VIND_CEXCT_maths <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXCT", c(3, 4), "PCT"))] > 0) > 0)

# aggregate all Student Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXST", c(3, 4, 5, 7, 8), "PCT"))]
tmpp$sum <- rowSums(teach[, c(paste0("VIND_CEXST",  c(3, 4, 5, 7, 8), "PCT"))] > 0)
teach$VIND_CEXST_SUM <- rowSums(teach[, c(paste0("VIND_CEXST",  c(3, 4, 5, 7, 8), "PCT"))] > 0)

# aggregate GENERAL Student Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXST", c(5, 7, 8), "PCT"))]
tmpp$sum <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXST", c(5, 7, 8), "PCT"))] > 0) > 0)
teach$VIND_CEXST_general <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXST", c(5, 7, 8), "PCT"))] > 0) > 0)

# aggregate MATHS Student Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXST", c(3, 4), "PCT"))]
tmpp$sum <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXST", c(3, 4), "PCT"))] > 0) > 0)
teach$VIND_CEXST_maths <- as.numeric(rowSums(teach[, c(paste0("VIND_CEXST", c(3, 4), "PCT"))] > 0) > 0)

# aggregate all Activity Structure Indicators #
tmpp <- teach[, grepl("T_ID|VIND_CM2|VIND_CM3|VIND_CM4|VIND_CM5", names(teach))]
tmpp$sum <- rowSums(teach[, grepl("VIND_CM2|VIND_CM3|VIND_CM4|VIND_CM5", names(teach))] > 0)
teach$VIND_CMAS_SUM <- rowSums(teach[, grepl("VIND_CM2|VIND_CM3|VIND_CM4|VIND_CM5", names(teach))] > 0)

# aggregate all Representations Types Indicators #
tmpp <- teach[, grepl("T_ID|VIND_QS6|VIND_QS7|VIND_QS8|VIND_QS9|VIND_QS10", names(teach))]
tmpp$sum <- rowSums(teach[, grepl("VIND_QS6|VIND_QS7|VIND_QS8|VIND_QS9|VIND_QS10", names(teach))] > 0)
teach$VIND_QSRT_SUM <- rowSums(teach[, grepl("VIND_QS6|VIND_QS7|VIND_QS8|VIND_QS9|VIND_QS10", names(teach))] > 0)


#### process student-level data ####

# load in data #
stud <- read.csv("data_raw/GTI-Student-Data.csv")

# rename some variables in the interest of consistency
names(stud)[names(stud) == "POST_PINT"] <- "SB_PINT"
names(stud)[names(stud) == "POST_GENSELFEFF"] <- "SB_GENSELFEFF"

# remove countries #

# countries to exclude
countries <- c("Shanghai", "Madrid")

# remove data from country
stud <- stud %>% 
  filter(!COUNTRY %in% countries)

# replace all 9999 with NA
stud <- stud %>%
  mutate(across(where(is.integer), ~na_if(., 9999))) %>% # missing
  mutate(across(where(is.integer), ~na_if(., 9998))) %>% # multiple responses
  mutate(across(where(is.integer), ~na_if(., 9997))) # Illegible response

# re-compute outcome and baseline measures #

# achievement: there seem to be unclear rounding issues in the data
table((stud$STA_TOTALSCORE/30) - stud$STA_PROPCORRECTSCORE)
table((stud$STB_TOTALSCORE/24) - stud$STB_PROPCORRECTSCORE)

# hence, after checking, the values are recomputed
stud$STA_PROPCORRECTSCORE <- stud$STA_TOTALSCORE/30
stud$STB_PROPCORRECTSCORE <- stud$STB_TOTALSCORE/24

# add factor scores of noncog computed in separate script #

# read in file
sqf_out <-  read.csv(file.path(dir, "01_preproc", "SQF_out.csv"))

# select relevant cols
sqf_out <- sqf_out[, grepl("_", names(sqf_out))]

# merge with main dataset
stud <- merge(stud, sqf_out, by = "S_ID", all = T)

# apply same exclusion as GTI authors 

# self-concept
stud$SAF_SELFCON <- ifelse(!is.na(stud$SA_SELFCON), stud$SAF_SELFCON, NA)
stud$SBF_SELFCON <- ifelse(!is.na(stud$SB_SELFCON), stud$SBF_SELFCON, NA)

# personal interest
stud$SAF_PINT <- ifelse(!is.na(stud$SA_PINT), stud$SAF_PINT, NA)
stud$SBF_PINT <- ifelse(!is.na(stud$SB_PINT), stud$SBF_PINT, NA)

# general self-efficacy
stud$SAF_GENSELFEFF <- ifelse(!is.na(stud$SA_GENSELFEFF), stud$SAF_GENSELFEFF, NA)
stud$SBF_GENSELFEFF <- ifelse(!is.na(stud$SB_GENSELFEFF), stud$SBF_GENSELFEFF, NA)

# matched task-specific self-efficacy
stud$SAF_EFFICACY_MATCHED <- ifelse(!is.na(stud$SA_EFFICACY), stud$SAF_EFFICACY_MATCHED, NA)
stud$SBF_EFFICACY_MATCHED <- ifelse(!is.na(stud$SB_EFFICACY), stud$SBF_EFFICACY_MATCHED, NA)


# add factor scores of teaching questionnaires computed in separate script #

# read in file
sqf_feat <-  read.csv(file.path(dir, "01_preproc", "SQF_feat.csv"))

# select relevant cols
sqf_feat <- sqf_feat[, grepl("_", names(sqf_feat))]

# merge with main dataset
stud <- merge(stud, sqf_feat, by = "S_ID", all = T)

# classroom management
stud$SBF_CM_DISRUPT <- ifelse(!is.na(stud$SB_CM_DISRUPT), stud$SBF_CM_DISRUPT, NA)
stud$SBF_CM_TEACHMAN <- ifelse(!is.na(stud$SB_CM_TEACHMAN), stud$SBF_CM_TEACHMAN, NA)

# socio-emotional support
stud$SBF_TESUP <- ifelse(!is.na(stud$SB_TESUP), stud$SBF_TESUP, NA)
stud$SBF_SUPCOM <- ifelse(!is.na(stud$SB_SUPCOM), stud$SBF_SUPCOM, NA)
stud$SBF_SUPAUT <- ifelse(!is.na(stud$SB_SUPAUT), stud$SBF_SUPAUT, NA)

# discourse
stud$SBF_DISCOURSE <- ifelse(!is.na(stud$SB_DISCOURSE), stud$SBF_DISCOURSE, NA)

# quality of subject matter
stud$SBF_CLARITY <- ifelse(!is.na(stud$SB_CLARITY), stud$SBF_CLARITY, NA)
stud$SBF_MEANING <- ifelse(!is.na(stud$SB_MEANING), stud$SBF_MEANING, NA)

# cognitive engagement
stud$SBF_COGACT <- ifelse(!is.na(stud$SB_COGACT), stud$SBF_COGACT, NA)

# assessment and response
stud$SBF_ADAPT <- ifelse(!is.na(stud$SB_ADAPT), stud$SBF_ADAPT, NA)
stud$SBF_FEEDBACK <- ifelse(!is.na(stud$SB_FEEDBACK), stud$SBF_FEEDBACK, NA)

# get relevant variable names: student reported variables
misc <- xlsx::read.xlsx(file = "misc/voi_v3.xlsx", sheetName = "Student", header = T)
stud_cols <- misc$Variable

# get relevant variable names: outcome measures
misc <- xlsx::read.xlsx(file = "misc/voi_v3.xlsx", sheetName = "Noncog", header = T)
out_cols <- misc$Variable

# reduce teach to relevant variables only
rel_cols <- c("COUNTRY", "SCH_ID", "T_ID", "S_ID", out_cols, stud_cols)

# stud <- stud[, rel_cols]
tmp <- stud[, rel_cols]


# compute Leave-One-Out mean for each student observation #

# determine vector with column names
loo_cols <- c(stud_cols)
v = 0

for (c in 1:length(loo_cols)) {
  
  # determine variable
  dv <- loo_cols[c]
  
  if (grepl("SB_OTL", dv) == T ) {
    
    v = v + 1
    
    # create tmp dat with relevant columns
    tmpp <- stud[, c("S_ID", "T_ID", dv)]
    
    # add CL mean
    tmpp <- tmpp %>%
      group_by(T_ID) %>%
      # compute leave one out mean
      mutate(clm = # class-level mean
               # compute group mean across all relevant observations
               mean(get(dv), na.rm = T)
      )
    
    # delete student-reported variable
    tmp[, dv] <- NULL
    
    # change variable name
    names(tmpp)[names(tmpp) == "clm"] <- paste0("CLM_", dv)
    
    # make df (currently tbl)
    tmpp <- as.data.frame(tmpp)
    
    # drop columns that are not needed
    tmpp$T_ID <- NULL
    tmpp[, dv] <- NULL
    
    # collect all LOO and GM in new df
    if (v == 1) {
      dat <- tmpp
    } else {
      dat <- merge(dat, tmpp, by = "S_ID")
    }
    
    # remove tmp objects
    rm(tmpp)
    
  }
  
}


#### combine student and teacher data ####

df <- merge(tmp, dat, by = "S_ID")
df <- merge(df, teach, by = "T_ID", all = F) # only merge data if classroom observations are available for a given teacher

write.csv(df, file = file.path(dir, "01_preproc", "GTI_preproc_v3.csv"), row.names = F)
