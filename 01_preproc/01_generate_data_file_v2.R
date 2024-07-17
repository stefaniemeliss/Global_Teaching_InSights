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


# # recode predictor variables #
# 
# # classroom management
# table(rowMeans((5- teach[, paste0("TQB11", LETTERS[1:3])])) - teach$TB_CM_DISRUPT)
# table(rowMeans(teach[, paste0("TQB11", LETTERS[c(6, 7, 9, 10)])]) - teach$TB_CM_TEACHMAN) # very different
# 
# teach$TB_CM_DISRUPT <- ifelse(!is.na(teach$TB_CM_DISRUPT), rowMeans((5- teach[, paste0("TQB11", LETTERS[1:3])]), na.rm = T), NA)
# teach$TB_CM_TEACHMAN <- ifelse(!is.na(teach$TB_CM_TEACHMAN), rowMeans(teach[, paste0("TQB11", LETTERS[c(6, 7, 9, 10)])], na.rm = T), NA)
# 
# tmpp <- teach[, c("T_ID", paste0("TQB11", LETTERS[c(6, 7, 9, 10)]), "TB_CM_TEACHMAN")]
# 
# # socio-emotional support
# table(rowMeans(teach[, paste0("TQB12", LETTERS[1:3])]) - teach$TB_TESUP)
# table(rowMeans(teach[, paste0("TQB12", LETTERS[4:7])]) - teach$TB_SUPCOM)
# table(rowMeans(teach[, paste0("TQB12", LETTERS[8:11])]) - teach$TB_SUPAUT)
# table(rowMeans(teach[, paste0("TQB13", LETTERS[1:5])]) - teach$TB_REL_STUDTEACH)
# 
# teach$TB_TESUP <- ifelse(!is.na(teach$TB_TESUP), rowMeans(teach[, paste0("TQB12", LETTERS[1:3])], na.rm = T), NA)
# teach$TB_REL_STUDTEACH <- ifelse(!is.na(teach$TB_REL_STUDTEACH), rowMeans(teach[, paste0("TQB13", LETTERS[1:5])], na.rm = T), NA)
# 
# # discourse
# table(rowMeans(teach[, paste0("TQB08", LETTERS[9:11])]) - teach$TB_DISCOURSE)
# teach$TB_DISCOURSE <- ifelse(!is.na(teach$TB_DISCOURSE), rowMeans(teach[, paste0("TQB08", LETTERS[9:11])], na.rm = T), NA)
# 
# # quality of subject matter
# table(rowMeans(teach[, paste0("TQB08", LETTERS[1:4])]) - teach$TB_CLARITY)
# table(rowMeans(teach[, paste0("TQB09", LETTERS[1:4])]) - teach$TB_MEANING)
# 
# # cognitive engagement
# table(rowMeans(teach[, paste0("TQB08", LETTERS[5:8])]) - teach$TB_COGACT) # VERY DIFFERENT RESULTS
# 
# # assessment and response
# table(rowMeans(teach[, paste0("TQB10", LETTERS[1:5])]) - teach$TB_ADAPT)
# 
# teach$TB_ADAPT <- ifelse(!is.na(teach$TB_ADAPT), rowMeans(teach[, paste0("TQB10", LETTERS[1:5])], na.rm = T), NA)

# get relevant variable names: teacher level
misc <- xlsx::read.xlsx(file = "misc/voi_v2.xlsx", sheetName = "Teacher", header = T)
teach_cols <- misc$Variable

# get relevant variable names: video observations
misc <- xlsx::read.xlsx(file = "misc/voi_v2.xlsx", sheetName = "Video", header = T)
vid_cols <- misc$Variable

# get relevant variable names: artefacts
misc <- xlsx::read.xlsx(file = "misc/voi_v2.xlsx", sheetName = "Artefacts", header = T)
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

# aggregate all Class Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXCT", 1:8, "PCT"))]
tmpp$sum <- rowSums(teach[, c(paste0("VIND_CEXCT", 1:8, "PCT"))] > 0)
teach$VIND_CEXCT_SUM <- rowSums(teach[, c(paste0("VIND_CEXCT", 1:8, "PCT"))] > 0)

# aggregate all Student Tech Indicators #
tmpp <- teach[, c("T_ID", paste0("VIND_CEXST", c(3, 4, 5, 7, 8), "PCT"))]
tmpp$sum <- rowSums(teach[, c(paste0("VIND_CEXST",  c(3, 4, 5, 7, 8), "PCT"))] > 0)
teach$VIND_CEXST_SUM <- rowSums(teach[, c(paste0("VIND_CEXST",  c(3, 4, 5, 7, 8), "PCT"))] > 0)

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
countries <- c("Shanghai", "Japan", "Madrid")

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

# recode some binary measurements: change 2 (= no) to 0
stud$SB_ASSESS_CHECK <- ifelse(stud$SB_ASSESS_CHECK == 2, 0, stud$SB_ASSESS_CHECK)
stud$SB_ASSESS_SELFEV <- ifelse(stud$SB_ASSESS_SELFEV == 2, 0, stud$SB_ASSESS_SELFEV)
stud$SB_ASSESS_OBS <- ifelse(stud$SB_ASSESS_OBS == 2, 0, stud$SB_ASSESS_OBS)

# get relevant variable names: teacher level
misc <- xlsx::read.xlsx(file = "misc/voi_v2.xlsx", sheetName = "Student", header = T)
stud_cols <- misc$Variable

# get relevant variable names: video observations
misc <- xlsx::read.xlsx(file = "misc/voi_v2.xlsx", sheetName = "Noncog", header = T)
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
  
  if (grepl("SB_OTL", dv) == T ) {
    # add LOO and CL mean
    tmpp <- tmpp %>%
      group_by(T_ID) %>%
      # compute leave one out mean
      mutate(loo = # LEAVE ONE OUT
               # compute sum of all values without the current row value
               # and divide that by the number of observations for this variable after excl NAs and subtracting 1 for current row value
               (sum(get(dv), na.rm = T) - get(dv))/(n() - sum(is.na(get(dv))) - 1),
             clm = # class-level mean
               # compute group mean across all relevant observations
               mean(get(dv), na.rm = T)
      )
  } else {
    # add LOO mean
    tmpp <- tmpp %>%
      group_by(T_ID) %>%
      # compute leave one out mean
      mutate(loo = # LEAVE ONE OUT
               # compute sum of all values without the current row value
               # and divide that by the number of observations for this variable after excl NAs and subtracting 1 for current row value
               (sum(get(dv), na.rm = T) - get(dv))/(n() - sum(is.na(get(dv))) - 1)
      )
  }
  
  names(tmpp)[names(tmpp) == "loo"] <- paste0("LOO_", dv)
  names(tmpp)[names(tmpp) == "clm"] <- paste0("CLM_", dv)
  
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

write.csv(df, file = file.path(dir, "01_preproc", "GTI_preproc_v2.csv"), row.names = F)


# code to create variable dict
# tmp <- data.frame("Variable" = names(df))
# tmp$Source <- 
#   ifelse(startsWith(tmp$Variable, "STA_"), "Student test variable - RAW",
#          ifelse(startsWith(tmp$Variable, "STB_"), "Student test variable - RAW",
#                 ifelse(startsWith(tmp$Variable, "SA_"), "Student reported variable - RAW",
#                        ifelse(startsWith(tmp$Variable, "SB_"), "Student reported variable - RAW",
#                               ifelse(grepl("LOO_", tmp$Variable), "Student derived variable - LOO",
#                                      ifelse(grepl("CLM_", tmp$Variable), "Student derived variable - CLM", 
#                                             ifelse(startsWith(tmp$Variable, "TA_"), "Teacher reported variable",
#                                                    ifelse(startsWith(tmp$Variable, "TB_"), "Teacher reported variable",
#                                                           ifelse(grepl("_TL", tmp$Variable), "Teacher logged variable",
#                                                                  ifelse(startsWith(tmp$Variable, "VCOMP"), "Video rating - Component",
#                                                                         ifelse(startsWith(tmp$Variable, "VIND"), "Video rating - Indicator",
#                                                                                ifelse(startsWith(tmp$Variable, "ACOMP"), "Artefact rating - Component",
#                                                                                       ifelse(grepl("_AR_", tmp$Variable), "Artefact rating - OTL",
#                                                                                              NA)))))))))))))
# write.csv(tmp, file = file.path(dir, "01_preproc", "GTI_vars_v1.csv"), row.names = F)

