#########################################################################
########## Compute factor scores of scales included in ML data ########## 
#########################################################################


#### setups ####

options(scipen = 999)
# empty work space
rm(list = ls())

# define directory
dir <- getwd()

# load libraries
library(dplyr)

########## process student-level data ########## 

# load in data #
stud <- read.csv(file.path(dir, "data_raw", "GTI-Student-Data.csv"))

devtools::source_url("https://github.com/stefaniemeliss/Global_Teaching_InSights/blob/main/functions.R?raw=TRUE")

# remove countries #

# countries to exclude
countries <- c("Shanghai", "Japan", "Madrid")

# remove data from country
stud <- stud %>% 
  filter(!COUNTRY %in% countries)

# replace all 9999 with NA #
stud <- stud %>%
  mutate(across(where(is.integer), ~na_if(., 9999))) %>% # missing
  mutate(across(where(is.integer), ~na_if(., 9998))) %>% # multiple responses
  mutate(across(where(is.integer), ~na_if(., 9997))) # Illegible response

# recode items
stud[,paste0("SQB11", LETTERS[c(1, 2, 3)])] <- 5 - stud[,paste0("SQB11", LETTERS[c(1, 2, 3)])]



#### compute factor scores for predictors ####

# formulate lavaan CFA model in alignment with GTI authors to extract factor scores
m_gti <- 
  ' 
  SBF_CLARITY       =~ SQB08A + SQB08B + SQB08C + SQB08D
  SBF_COGACT        =~ SQB08E + SQB08F + SQB08G + SQB08H
  SBF_DISCOURSE     =~ SQB08I + SQB08J + SQB08K
  SBF_MEANING       =~ SQB09A + SQB09B + SQB09C + SQB09D
  SBF_ADAPT         =~ SQB10A + SQB10B + SQB10C + SQB10D + SQB10E
  SBF_CM_DISRUPR    =~ SQB11A + SQB11B + SQB11C
  SBF_CM_TEACHMAN   =~ SQB11F + SQB11G + SQB11I + SQB11J
  SBF_TESUP         =~ SQB12A + SQB12B + SQB12C
  SBF_SUPCOM        =~ SQB12D + SQB12E + SQB12F + SQB12G 
  SBF_SUPAUT        =~ SQB12H + SQB12I + SQB12J + SQB12K 
  SBF_REL_STUDTEACH =~ SQB13A + SQB13B + SQB13C + SQB13D + SQB13E 
  SBF_FEEDBACK      =~ SQB16A + SQB16B + SQB16C + SQB16D 
'
# extract factor score for each subject
extract_factorscores(data_in = stud,
                     cfa_model = m_gti,
                     id_var = "S_ID",
                     file_out = file.path(dir, "01_preproc", "SQF_feat.csv"))

#### compute factor scores for outcomes and baselines ####

# determine unique variable names
vars <- c(
  # self-concept
  "SAF_SELFCON",
  "SBF_SELFCON",
  # personal interest
  "SAF_PINT",
  "SBF_PINT",
  # general self-efficacy
  "SAF_GENSELFEFF",
  "SBF_GENSELFEFF",
  # MATCHED task-specific self- efficacy 
  "SAF_EFFICACY_MATCHED",
  "SBF_EFFICACY_MATCHED"
)

# determine items that are summed up to factor
scales <- c(
  # self-concept
  paste0("SQA06", LETTERS[1:6],collapse = " + "),
  paste0("SQB01", LETTERS[1:6],collapse = " + "),
  # personal interest
  paste0("SQA14", LETTERS[1:3],collapse = " + "),
  paste0("SQB03", LETTERS[1:3],collapse = " + "),
  # general self-efficacy
  paste0("SQA15", LETTERS[1:5],collapse = " + "),
  paste0("SQB02", LETTERS[1:5],collapse = " + "),
  # MATCHED task-specific self- efficacy 
  paste0("SQA16", LETTERS[6:10],"2", collapse = " + "),
  paste0("SQB07C", LETTERS[1:5], collapse = " + ")
)


for (v in 1:length(vars)) {
  
  cat("### ", vars[v])
  cat("\n")
  
  # lavaan model formulation
  model <- paste(vars[v], "=~", scales[v])
  
  # extract factor score for each subject
  extract_factorscores(data_in = stud,
                       cfa_model = model,
                       id_var = "S_ID",
                       file_out = file.path(dir, "01_preproc", "tmp.csv"))
  
  # read tmp csv file back in
  tmp <- read.csv(file.path(dir, "01_preproc", "tmp.csv"))
  
  if (v == 1) {
    SQF_out <- tmp
  } else {
    SQF_out <- merge(SQF_out, tmp, by = "S_ID", all = T)
  }
  
  # remove tmp file
  file.remove(file.path(dir, "01_preproc", "tmp.csv"))
}

# save factor scores in file
write.csv(SQF_out, file.path(dir, "01_preproc", "SQF_out.csv"), row.names = F)

