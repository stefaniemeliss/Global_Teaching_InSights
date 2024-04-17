## This directory contains the following set of files:

### 01_variable_exploration_[]
Rmd files to create html reports that explore the GTI data using psych::describe() and scatterplots of baseline and posttest measures. Reports also contain information on items used in each scale.

### 02_mi_[]: 
Rmd files to run measurement invariance (MI) analyses. 
#### 02b_mi_[]: 
MI is tested using multi-group confirmatory factor analysis (MG-IRT). *non_cog* contains 15 student-reported outcome variables whereas *non_cog_fin* contains final selection of 8 student-reported outcome variables. *[]removed* indicates how many out of the eight countries available were randomly removed. This approach was taken as MI did not hold across all countries. The files ending in *_select* repeat MI analyses for a selected subgroup of countries that show MI.
#### 02c_mi_[]: 
MI is tested using multi-group item response theory (MG-IRT) models. 
#### 02d_mi_[]: 
MI is tested using multi-group confirmatory factor analysis (MG-IRT). *stud_teaching* contains 33 student-reported variables of teaching quality whereas *stud_teaching_fin* contains final selection of 10 student-reported variables of teaching quality. *[]removed* indicates how many out of the eight countries available were randomly removed. This approach was taken as MI did not hold across all countries. The files ending in *_select* repeat MI analyses for a selected subgroup of countries that show MI.
