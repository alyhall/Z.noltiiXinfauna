## Header and Preamble ------------------------------------------------------------
##
## Script name: GHG Explore
##
## Purpose of script: Explore GHG data from mesocosm experiment
##
## Author: Alyson Hall
##
## Date Created: 2025-06-12
## Date Edited: 2025-06-13
##
## Copyright (c) Alyson Hall, 2025
## Email: achall@vims.edu
##

## Notes:
##
## Licore time was behind real time by 50 s
##
##

## Packages Needed

library(tidyverse)
library(readxl)
library(fuzzyjoin)

## Load in Data ----------------------------

GHG <- read_excel("Data/GHG_2025-06-12.xlsx", 
                  na = "nan", skip = 4) %>%
              {
                colnames(.) <- as.character(unlist(.[1, ]))  # First row becomes column names
                . <- .[-c(1, 2), ]  # Remove both the header row and units row
                . 
                } %>% 
          select(TIME:LASER_T) %>%  # Retain only columns I want
          mutate(TIME = hms::as_hms(TIME),
                 TIME = hms::as_hms(TIME - 50), # Licore was ahead by 50 s
                 CO2 = as.numeric(CO2),
                 CH4 = as.numeric(CH4)) 


# Read in measurements from data sheet

Measure <- read_excel("Data/GHG_2025-06-12.xlsx", sheet = 2) %>% 
                mutate(Start_Time = hms::as_hms(Start_Time),
                       End_Time = hms::as_hms(End_Time),
                       shade_Per = as.factor(Shade_Per))


# Combine based on run time

Combo <- 
    fuzzy_left_join(
      GHG,
      Measure,
      by = c("TIME" = "Start_Time", "TIME" = "End_Time"),
      match_fun = list(`>=`, `<=`)) 

Combo <- 
  Combo %>% 
      mutate(Dur = difftime(Combo$TIME, Combo$Start_Time)) %>% # Create a column for runtime 
      filter(!Dur == "NA") # Remove the NA (between measurements and before machine started)

# get flux per s for each treatment

Fluxes <- 
    Combo %>%
      group_by(Mesocosm_Treatment, Replicate, Shade_Per) %>%
      arrange(Mesocosm_Treatment, Replicate, Shade_Per, Dur) %>% 
      # Generating fluxes for gar exchange per s 
      mutate(CO2flux = (as.numeric(CO2) - lag(as.numeric(CO2), n = 1)),
             CH4flux = (as.numeric(CH4) - lag(as.numeric(CH4), n = 1))) 


# Data Exploration 

Combo %>% 
  filter(Dur < 241) %>% 
  filter(Dur > 120) %>% 
  filter(Mesocosm_Treatment == "MED") %>% 
  ggplot(aes( x = Dur, y = CH4, color = Shade_Per)) +
  geom_point() +
  facet_wrap(~Replicate, scales = "free_y")


# No MED 2 dark
