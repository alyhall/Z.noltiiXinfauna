## Header and Preamble ---------------------------
##
## Script name: 
##
## Purpose of script: 
##
## Author: Alyson & Ashitha
##
## Date Created: 2025-07-18
## Date Edited: 2025-07-18
## Last Edited by: Alyson
##

## Notes:
## 
##  
##

## Load up the packages we will need: 
library(tidyverse)


##
## When using or reading my code, I recommended turning on code soft-wrap.
########End


## Data Load In ----------------

ImageJ <- read_excel("Data/Ant Farm/ImageJ_Analysis.xlsx", 
                                 sheet = "ImageJ", col_names = TRUE, 
                                 skip = 29)

# To-Do:
# - read in ImageJ Data
# - read in treatment data 
# Merge them together using a function from the join() family (Ex: left_join())
# Create a summary boxplot for some of the metrics per species through time
