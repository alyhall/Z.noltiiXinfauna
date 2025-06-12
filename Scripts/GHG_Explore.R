## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG Explore
##
## Purpose of script: Explore GHG data from mesocosm experiment
##
## Author: Alyson Hall
##
## Date Created: 2025-06-12
## Date Edited: 2025-06-12
##
## Copyright (c) Alyson Hall, 2025
## Email: achall@vims.edu
##

## Notes:
##
##
##
##

## Packages Needed

library(tidyverse)

## Load in Data ----------------------------

GHG <- read_excel("Data/GHG_2025-06-12.xlsx", 
                  na = "nan", skip = 5) %>%
  {
    colnames(.) <- as.character(unlist(.[1, ]))  # First row becomes column names
    . <- .[-c(1, 2), ]  # Remove both the header row and units row
    .
  }
