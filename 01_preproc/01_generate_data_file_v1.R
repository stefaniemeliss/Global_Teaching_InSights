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

# replace all 9999 with NA
teach <- teach %>%
  mutate(across(where(is.integer), ~na_if(., 9999))) %>% # missing
  mutate(across(where(is.integer), ~na_if(., 9998))) %>% # multiple responses
  mutate(across(where(is.integer), ~na_if(., 9997))) # Illegible response

# remove countries #

# countries to exclude
countries <- c("Shanghai", "Japan", "Madrid")

# remove data from country
teach <- teach %>% 
  filter(!COUNTRY %in% countries)

# recode predictor variables #

# classroom management
table(rowMeans((5- teach[, paste0("TQB11", LETTERS[1:3])])) - teach$TB_CM_DISRUPT)
table(rowMeans(teach[, paste0("TQB11", LETTERS[c(6, 7, 9, 10)])]) - teach$TB_CM_TEACHMAN) # very different

teach$TB_CM_DISRUPT <- ifelse(!is.na(teach$TB_CM_DISRUPT), rowMeans((5- teach[, paste0("TQB11", LETTERS[1:3])]), na.rm = T), NA)
teach$TB_CM_TEACHMAN <- ifelse(!is.na(teach$TB_CM_TEACHMAN), rowMeans(teach[, paste0("TQB11", LETTERS[c(6, 7, 9, 10)])], na.rm = T), NA)

tmpp <- teach[, c("T_ID", paste0("TQB11", LETTERS[c(6, 7, 9, 10)]), "TB_CM_TEACHMAN")]

# socio-emotional support
table(rowMeans(teach[, paste0("TQB12", LETTERS[1:3])]) - teach$TB_TESUP)
table(rowMeans(teach[, paste0("TQB12", LETTERS[4:7])]) - teach$TB_SUPCOM)
table(rowMeans(teach[, paste0("TQB12", LETTERS[8:11])]) - teach$TB_SUPAUT)
table(rowMeans(teach[, paste0("TQB13", LETTERS[1:5])]) - teach$TB_REL_STUDTEACH)

teach$TB_TESUP <- ifelse(!is.na(teach$TB_TESUP), rowMeans(teach[, paste0("TQB12", LETTERS[1:3])], na.rm = T), NA)
teach$TB_REL_STUDTEACH <- ifelse(!is.na(teach$TB_REL_STUDTEACH), rowMeans(teach[, paste0("TQB13", LETTERS[1:5])], na.rm = T), NA)

# discourse
table(rowMeans(teach[, paste0("TQB08", LETTERS[9:11])]) - teach$TB_DISCOURSE)
teach$TB_DISCOURSE <- ifelse(!is.na(teach$TB_DISCOURSE), rowMeans(teach[, paste0("TQB08", LETTERS[9:11])], na.rm = T), NA)

# quality of subject matter
table(rowMeans(teach[, paste0("TQB08", LETTERS[1:4])]) - teach$TB_CLARITY)
table(rowMeans(teach[, paste0("TQB09", LETTERS[1:4])]) - teach$TB_MEANING)

# cognitive engagement
table(rowMeans(teach[, paste0("TQB08", LETTERS[5:8])]) - teach$TB_COGACT) # VERY DIFFERENT RESULTS

# assessment and response
table(rowMeans(teach[, paste0("TQB10", LETTERS[1:5])]) - teach$TB_ADAPT)

teach$TB_ADAPT <- ifelse(!is.na(teach$TB_ADAPT), rowMeans(teach[, paste0("TQB10", LETTERS[1:5])], na.rm = T), NA)

# get relevant variable names: teacher level
misc <- xlsx::read.xlsx(file = "misc/voi_v1.xlsx", sheetName = "Teacher", header = T)
teach_cols <- misc$Variable

# get relevant variable names: video observations
misc <- xlsx::read.xlsx(file = "misc/voi_v1.xlsx", sheetName = "Video", header = T)
vid_cols <- misc$Variable

# get relevant variable names: artefacts
misc <- xlsx::read.xlsx(file = "misc/voi_v1.xlsx", sheetName = "Artefacts", header = T)
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

# remove data from country
stud <- stud %>% 
  filter(!COUNTRY %in% countries)


# replace all 9999 with NA
stud <- stud %>%
  mutate(across(where(is.integer), ~na_if(., 9999))) %>% # missing
  mutate(across(where(is.integer), ~na_if(., 9998))) %>% # multiple responses
  mutate(across(where(is.integer), ~na_if(., 9997))) # Illegible response

# rename some variables in the interest of consistency
names(stud)[names(stud) == "PRE_PINT"] <- "SA_PINT_PREV"
names(stud)[names(stud) == "POST_PINT"] <- "SB_PINT"
names(stud)[names(stud) == "PRE_GENSELFEFF"] <- "SA_GENSELFEFF_PREV"
names(stud)[names(stud) == "POST_GENSELFEFF"] <- "SB_GENSELFEFF"

# re-compute outcome and baseline measures #

# there seem to be unclear rounding issues in the data
# hence, after checking, the values are recomputed

# achivement
table((stud$STA_TOTALSCORE/30) - stud$STA_PROPCORRECTSCORE)
table((stud$STB_TOTALSCORE/24) - stud$STB_PROPCORRECTSCORE)

stud$STA_PROPCORRECTSCORE <- stud$STA_TOTALSCORE/30
stud$STB_PROPCORRECTSCORE <- stud$STB_TOTALSCORE/24

# self-concept
table(rowMeans(stud[, paste0("SQA06", LETTERS[1:6])]) - stud$SA_SELFCON)
table(rowMeans(stud[, paste0("SQB01", LETTERS[1:6])]) - stud$SB_SELFCON)

stud$SA_SELFCON <- ifelse(!is.na(stud$SA_SELFCON), rowMeans(stud[, paste0("SQA06", LETTERS[1:6])], na.rm = T), NA)
stud$SB_SELFCON <- ifelse(!is.na(stud$SB_SELFCON), rowMeans(stud[, paste0("SQB01", LETTERS[1:6])], na.rm = T), NA)

# personal interest
table(rowMeans(stud[, paste0("SQA12", LETTERS[1:3])]) - stud$SA_PINT_PREV)
table(rowMeans(stud[, paste0("SQA14", LETTERS[1:3])]) - stud$SA_PINT_CURR)
table(rowMeans(stud[, paste0("SQB03", LETTERS[1:3])]) - stud$SB_PINT)

stud$SA_PINT_PREV <- ifelse(!is.na(stud$SA_PINT_PREV), rowMeans(stud[, paste0("SQA12", LETTERS[1:3])], na.rm = T), NA)
stud$SA_PINT_CURR <- ifelse(!is.na(stud$SA_PINT_CURR), rowMeans(stud[, paste0("SQA14", LETTERS[1:3])], na.rm = T), NA)
stud$SB_PINT <- ifelse(!is.na(stud$SB_PINT), rowMeans(stud[, paste0("SQB03", LETTERS[1:3])], na.rm = T), NA)

# general self-efficacy
table(rowMeans(stud[, paste0("SQA13", LETTERS[1:5])]) - stud$SA_GENSELFEFF_PREV)
table(rowMeans(stud[, paste0("SQA15", LETTERS[1:5])]) - stud$SA_GENSELFEFF_CURR)
table(rowMeans(stud[, paste0("SQB02", LETTERS[1:5])]) - stud$SB_GENSELFEFF)

stud$SA_GENSELFEFF_PREV <- ifelse(!is.na(stud$SA_GENSELFEFF_PREV), rowMeans(stud[, paste0("SQA13", LETTERS[1:5])], na.rm = T), NA)
stud$SA_GENSELFEFF_CURR <- ifelse(!is.na(stud$SA_GENSELFEFF_CURR), rowMeans(stud[, paste0("SQA15", LETTERS[1:5])], na.rm = T), NA)
stud$SB_GENSELFEFF <- ifelse(!is.na(stud$SB_GENSELFEFF), rowMeans(stud[, paste0("SQB02", LETTERS[1:5])], na.rm = T), NA)

# compute matched task-specific self-efficacy scale #

# PRE: calculated as the mean of SQA16F2 to SQA16J2 - in all cases where the SA_EFFICACY is not NA
stud$SA_EFFICACY_MATCHED <- ifelse(!is.na(stud$SA_EFFICACY), rowMeans(stud[, paste0("SQA16", LETTERS[6:10],"2")], na.rm = T), NA)

# POST: calculated as the mean of SQB07CA to SQB07CE - in all cases where the SB_EFFICACY is not NA
stud$SB_EFFICACY_MATCHED <- ifelse(!is.na(stud$SB_EFFICACY), rowMeans(stud[, paste0("SQB07C", LETTERS[1:5])], na.rm = T), NA)


# recode predictor variables #

# classroom management
table(rowMeans((5- stud[, paste0("SQB11", LETTERS[1:3])])) - stud$SB_CM_DISRUPT)
table(rowMeans(stud[, paste0("SQB11", LETTERS[c(6, 7, 9, 10)])]) - stud$SB_CM_TEACHMAN)

stud$SB_CM_DISRUPT <- ifelse(!is.na(stud$SB_CM_DISRUPT), rowMeans((5- stud[, paste0("SQB11", LETTERS[1:3])]), na.rm = T), NA)

# socio-emotional support
table(rowMeans(stud[, paste0("SQB12", LETTERS[1:3])]) - stud$SB_TESUP)
table(rowMeans(stud[, paste0("SQB12", LETTERS[4:7])]) - stud$SB_SUPCOM)
table(rowMeans(stud[, paste0("SQB12", LETTERS[8:11])]) - stud$SB_SUPAUT)
table(rowMeans(stud[, paste0("SQB13", LETTERS[1:5])]) - stud$SB_REL_STUDTEACH)

stud$SB_TESUP <- ifelse(!is.na(stud$SB_TESUP), rowMeans(stud[, paste0("SQB12", LETTERS[1:3])], na.rm = T), NA)
stud$SB_REL_STUDTEACH <- ifelse(!is.na(stud$SB_REL_STUDTEACH), rowMeans(stud[, paste0("SQB13", LETTERS[1:5])], na.rm = T), NA)

# discourse
table(rowMeans(stud[, paste0("SQB08", LETTERS[9:11])]) - stud$SB_DISCOURSE)
stud$SB_DISCOURSE <- ifelse(!is.na(stud$SB_DISCOURSE), rowMeans(stud[, paste0("SQB08", LETTERS[9:11])], na.rm = T), NA)

# quality of subject matter
table(rowMeans(stud[, paste0("SQB08", LETTERS[1:4])]) - stud$SB_CLARITY)
table(rowMeans(stud[, paste0("SQB09", LETTERS[1:4])]) - stud$SB_MEANING)

# cognitive engagement
table(rowMeans(stud[, paste0("SQB08", LETTERS[5:8])]) - stud$SB_COGACT) # VERY DIFFERENT RESULTS

stud$SB_COGACT <- ifelse(!is.na(stud$SB_COGACT), rowMeans(stud[, paste0("SQB08", LETTERS[5:8])], na.rm = T), NA)

# assessment and response
table(rowMeans(stud[, paste0("SQB10", LETTERS[1:5])]) - stud$SB_ADAPT)
table(rowMeans(stud[, paste0("SQB16", LETTERS[1:4])]) - stud$SB_FEEDBACK)

stud$SB_ADAPT <- ifelse(!is.na(stud$SB_ADAPT), rowMeans(stud[, paste0("SQB10", LETTERS[1:5])], na.rm = T), NA)

# recode some binary measurements: change 2 (= no) to 0
stud$SB_ASSESS_CHECK <- ifelse(stud$SB_ASSESS_CHECK == 2, 0, stud$SB_ASSESS_CHECK)
stud$SB_ASSESS_SELFEV <- ifelse(stud$SB_ASSESS_SELFEV == 2, 0, stud$SB_ASSESS_SELFEV)
stud$SB_ASSESS_OBS <- ifelse(stud$SB_ASSESS_OBS == 2, 0, stud$SB_ASSESS_OBS)

# additional factors
table(rowMeans(stud[, paste0("SQB14", LETTERS[1:4])]) - stud$SB_EXPECT)
table(rowMeans(stud[, paste0("SQB17", LETTERS[1:8])]) - stud$SB_TEACHENTHUS)

# use of opportunities
table(rowMeans(stud[, paste0("SQB06", LETTERS[1:3])]) - stud$SB_USECONT)
table(rowMeans(stud[, paste0("SQB06", LETTERS[4:6])]) - stud$SB_USECOGACT)
table(rowMeans(stud[, paste0("SQB06", LETTERS[7:9])]) - stud$SB_USESELFDET)
table(rowMeans(stud[, paste0("SQB06", LETTERS[10:12])]) - stud$SB_USETOT)

stud$SB_USECONT <- ifelse(!is.na(stud$SB_USECONT), rowMeans(stud[, paste0("SQB06", LETTERS[1:3])], na.rm = T), NA)
stud$SB_USECOGACT <- ifelse(!is.na(stud$SB_USECOGACT), rowMeans(stud[, paste0("SQB06", LETTERS[4:6])], na.rm = T), NA)
stud$SB_USESELFDET <- ifelse(!is.na(stud$SB_USESELFDET), rowMeans(stud[, paste0("SQB06", LETTERS[7:9])], na.rm = T), NA)
stud$SQB06L <- 5 - stud$SQB06L # recode item
stud$SB_USETOT <- ifelse(!is.na(stud$SB_USETOT), rowMeans(stud[, paste0("SQB06", LETTERS[10:12])], na.rm = T), NA)


# get relevant variable names: teacher level
misc <- xlsx::read.xlsx(file = "misc/voi_v1.xlsx", sheetName = "Student", header = T)
stud_cols <- misc$Variable

# get relevant variable names: video observations
misc <- xlsx::read.xlsx(file = "misc/voi_v1.xlsx", sheetName = "Noncog", header = T)
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

write.csv(df, file = file.path(dir, "01_preproc", "GTI_preproc_v1.csv"), row.names = F)


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

