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
tl <- read.csv("data_raw/GTI-TeachLog-Data.csv")

# get relevant variable names: teacher level
misc <- xlsx::read.xlsx(file = "misc/features.xlsx", sheetName = "Teacher", header = T)
teach_cols <- misc$Variable

# get relevant variable names: video observations
misc <- xlsx::read.xlsx(file = "misc/features.xlsx", sheetName = "Video", header = T)
vid_cols <- misc$Variable

# get relevant variable names: artefacts
misc <- xlsx::read.xlsx(file = "misc/features.xlsx", sheetName = "Artefacts", header = T)
arte_cols <- misc$Variable

# reduce teach to relevant variables only
rel_cols <- c("T_ID", teach_cols, vid_cols, arte_cols)
teach <- teach[, rel_cols]

# compute derived variables based on teacher log to include in the teacher level file
tmp <- tl %>% 
  filter(DATE_TL != "") %>%
  group_by(T_ID) %>%
  count()
names(tmp) <- c("T_ID", "N_ENTRIES_TL")

# add to teach data
teach <- merge(teach, tmp, by = "T_ID", all.x = T)

# remove objects no longer needed
rm(tl, tmp, misc)

# replace all 9999 with NA
teach <- teach %>%
  mutate(across(where(is.integer), ~na_if(., 9999)))

#### process teacher-level data ####

# load in data #
stud <- read.csv("data_raw/GTI-Student-Data.csv")

# replace all 9999 with NA
stud <- stud %>%
  mutate(across(where(is.integer), ~na_if(., 9999)))

# compute matched task-specific self-efficacy scale #

# PRE: calculated as the mean of SQA16F2 to SQA16J2 - in all cases where the SA_EFFICACY is not NA
stud$SA_EFFICACY_MATCHED <- ifelse(!is.na(stud$SA_EFFICACY), rowMeans(stud[, paste0("SQA16", LETTERS[6:10],"2")], na.rm = T), NA)

# POST: calculated as the mean of SQB07CA to SQB07CE - in all cases where the SB_EFFICACY is not NA
stud$SB_EFFICACY_MATCHED <- ifelse(!is.na(stud$SB_EFFICACY), rowMeans(stud[, paste0("SQB07C", LETTERS[1:5])], na.rm = T), NA)

# recode some binary measurements: change 2 (= no) to 0
stud$SB_ASSESS_CHECK <- ifelse(stud$SB_ASSESS_CHECK == 2, 0, stud$SB_ASSESS_CHECK)
stud$SB_ASSESS_SELFEV <- ifelse(stud$SB_ASSESS_SELFEV == 2, 0, stud$SB_ASSESS_SELFEV)
stud$SB_ASSESS_OBS <- ifelse(stud$SB_ASSESS_OBS == 2, 0, stud$SB_ASSESS_OBS)

# get relevant variable names: teacher level
misc <- xlsx::read.xlsx(file = "misc/features.xlsx", sheetName = "Student", header = T)
stud_cols <- misc$Variable

# get relevant variable names: video observations
misc <- xlsx::read.xlsx(file = "misc/features.xlsx", sheetName = "Noncog", header = T)
out_cols <- misc$Variable

# reduce teach to relevant variables only
rel_cols <- c("COUNTRY", "SCH_ID", "T_ID", "S_ID", out_cols, stud_cols)

# stud <- stud[, rel_cols]
tmp <- stud[, rel_cols]

# compute Leave-One-Out mean for each student observation #

# determine vector with column names
loo_cols <- c(out_cols, stud_cols)

for (c in 1:length(loo_cols)) {
  
  # determine variable
  dv <- loo_cols[c]
  
  # create tmp dat with relevant columns
  tmpp <- stud[, c("S_ID", "T_ID", dv)]
  
  # add LOO mean
  tmpp <- tmpp %>%
    group_by(T_ID) %>%
    # compute leave one out mean
    mutate(loo = # LEAVE ONE OUT
             # compute sum of all values without the current row value
             # and divide that by the number of observations for this variable after excl NAs and subtracting 1 for current row value
             (sum(get(dv), na.rm = T) - get(dv))/(n() - sum(is.na(get(dv))) - 1),
           gm = # class-level mean
             # compute group mean across all relevant observations
             mean(get(dv), na.rm = T)
    )

  names(tmpp)[names(tmpp) == "loo"] <- paste0("LOO_", dv)
  names(tmpp)[names(tmpp) == "cvm"] <- paste0("CLM_", dv)
  
  # make df (currently tbl)
  tmpp <- as.data.frame(tmpp)
  
  # drop columns that are not needed
  tmpp$T_ID <- NULL
  tmpp[, dv] <- NULL
  
  # collect all LOO and GM in new df
  if (c == 1) {
    dat <- tmpp
  } else {
    dat <- merge(dat, tmpp, by = "S_ID")
  }
  
  # remove tmp objects
  rm(tmpp)
  
}

#### combine student and teacher data ####

df <- merge(tmp, dat, by = "S_ID")
df <- merge(df, teach, by = "T_ID")